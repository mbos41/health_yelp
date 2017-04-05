library(lda)
library(tidyverse)
library(stringr)
library(tm)
library(glmnet)
library(slam)

setwd("Documents/COMP_790/health_yelp")
raw <- read.csv('data/analysis_data.csv')

raw$pos_text_temp <- trimws(as.character(raw$pos_text))
raw$neg_text_temp <- trimws(as.character(raw$neg_text))
analysis <- raw %>% 
  mutate(year = str_sub(date, 1, 4),
         wgt_viol = (X.*1 + X..*2 + X...*5),
         pos_text = ifelse(nchar(pos_text_temp) == 0, 'blank', pos_text_temp),
         neg_text = ifelse(nchar(neg_text_temp) == 0, 'blank', neg_text_temp),
         all_text = ifelse(nchar(trimws(paste(pos_text_temp, neg_text_temp, set = ""))) == 0, 'blank', paste(pos_text_temp, neg_text_temp)),
         unhygienic = ifelse(wgt_viol >= 7, 1, 0))

analysis %>% group_by(year) %>% summarise(count = n())

train <- analysis %>% 
  filter(year < 2010) %>% 
  mutate(log_wgt_viol = log(wgt_viol + 1)) 

summary(train$wgt_viol)
summary(train$unhygienic)
ggplot(data = train) + geom_histogram(mapping = aes(x = wgt_viol))

mean_baseline_err = mean((train$wgt_viol - 10.44)^2)^0.5
log_mean_baseline_err = mean((train$log_wgt_viol - 1.97)^2)^0.5

set.seed(10)
sample_train <- train[sample(nrow(train), 3000),]

# Clean up text for creating restaurant document term matrix
docs <- Corpus(VectorSource(sample_train$all_text))
docs <-tm_map(docs,content_transformer(tolower))
#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Stem document
docs <- tm_map(docs,stemDocument)

writeLines(as.character(docs[[30]]))
#Create document-term matrix (Limit to terms appearing in at least 10 restaurants and no more than 200 restaurants)
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20)))
dtm2 <- removeSparseTerms(dtm, .975)

# Filter DTM by tfidf
summary(col_sums(dtm))
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)
dtm_filter <- dtm[,term_tfidf >= 0.1]

# Filter DTM by term counts
term_cnts <- col_sums(dtm) 
dtm_filter <- dtm[, term_cnts > 50]

#colnames(dtm_pos) <- paste(colnames(dtm_pos), "p", sep = "_")
                           
dtm_merge <- as.matrix(dtm2)


fit <- glmnet(dtm_merge, factor(sample_train$unhygienic), family = "binomial")
plot(fit, xvar = "dev", label = TRUE)
cvfit <- cv.glmnet(dtm_merge, factor(sample_train$unhygienic), family = "binomial", type.measure = "class")
plot(cvfit)
cvfit$lambda.min
(coeffs <- as.matrix(coef(cvfit, s = "lambda.min")))



######################################################################
###############       Supervised LDA approach          ###############
######################################################################
# pre-processing:
text <- gsub("'", "", sample_train$all_text)  # remove apostrophes
text <- gsub("[[:punct:]]", " ", text)  # replace punctuation with space
text <- gsub("[[:cntrl:]]", " ", text)  # replace control characters with space
text <- gsub("^[[:space:]]+", "", text) # remove whitespace at beginning of documents
text <- gsub("[[:space:]]+$", "", text) # remove whitespace at end of documents
text <- tolower(text)  # force to lowercase

stop_words <- stopwords("SMART")
# tokenize on space and output as a list:
doc.list <- strsplit(text, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 300 times:
del <- names(term.table) %in% stop_words | term.table < 100
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(doc.list, get.terms)
#doc_totals <- lapply(documents, length)

set.seed(200)
k = 40
params <- sample(c(0, 0), k, replace=TRUE)
t1 <- Sys.time()
sldaOut40 <- slda.em(documents = documents, vocab =  vocab, K = k, num.e.iterations = 10, num.m.iterations = 10, variance = 1.05, 
                   alpha = 1, eta = 1, annotations = sample_train$log_wgt_viol, params, logistic = FALSE, method = "sLDA")
t2 <- Sys.time()
t2 - t1

# 50/100 iterations seems to show only small changes from 20/20 iterations 
top_words40 <- top.topic.words(sldaOut40$topics, num.words = 20)
topic_coeff40 <- as.data.frame(sldaOut40$coef)

pred40 <- slda.predict(documents = documents, topics = sldaOut40$topics, model = sldaOut40$model, alpha = 1, eta = 1)

sample_train$predicted40 <-  pred40
rmse <- mean((sample_train$log_wgt_viol - sample_train$predicted40)^2)^0.5
