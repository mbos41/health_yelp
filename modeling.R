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
         neg_text = ifelse(nchar(neg_text_temp) == 0, 'blank', neg_text_temp))

analysis %>% group_by(year) %>% summarise(count = n())

train <- analysis %>% 
  filter(year < 2010) %>% 
  mutate(log_wgt_viol = log(wgt_viol + 1))

summary(train$log_wgt_viol)
ggplot(data = train) + geom_histogram(mapping = aes(x = wgt_viol))

mean_baseline_err = mean((train$wgt_viol - 10.44)^2)^0.5
log_mean_baseline_err = mean((train$log_wgt_viol - 1.97)^2)^0.5

# Clean up text for creating restaurant document term matrix
docs <- Corpus(VectorSource(train$pos_text))
docs <-tm_map(docs,content_transformer(tolower))
#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Create document-term matrix (Limit to terms appearing in at least 10 restaurants and no more than 200 restaurants)
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20)))

# Filter DTM by tfidf
summary(col_sums(dtm))
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)
dtm_filter <- dtm[,term_tfidf >= 0.1]

# Filter DTM by term counts
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
term_cnts <- col_sums(dtm) 
dtm_filter <- dtm[, term_cnts > 50]

colnames(dtm_pos) <- paste(colnames(dtm_pos), "p", sep = "_"
                           
dtm_merge <- as.matrix(dtm_filter)


fit <- glmnet(dtm_merge, train$log_wgt_viol)
cvfit <- cv.glmnet(dtm_merge, train$log_wgt_viol)
plot(cvfit)
cvfit$lambda.min
(coeffs <- as.matrix(coef(cvfit, s = "lambda.min")))
# pre-processing:
pos_text <- gsub("'", "", train$pos_text)  # remove apostrophes
pos_text <- gsub("[[:punct:]]", " ", pos_text)  # replace punctuation with space
pos_text <- gsub("[[:cntrl:]]", " ", pos_text)  # replace control characters with space
pos_text <- gsub("^[[:space:]]+", "", pos_text) # remove whitespace at beginning of documents
pos_text <- gsub("[[:space:]]+$", "", pos_text) # remove whitespace at end of documents
pos_text <- tolower(pos_text)  # force to lowercase


######################################################################
###############       Supervised LDA approach          ###############
######################################################################

stop_words <- stopwords("SMART")
# tokenize on space and output as a list:
doc.list <- strsplit(pos_text, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 50
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

set.seed(200)
k = 10
params <- sample(c(0, 0), k, replace=TRUE)
t1 <- Sys.time()
sldaOut <- slda.em(documents = documents, vocab =  vocab, K = k, num.e.iterations = 50, num.m.iterations = 100, variance = 125, 
                   alpha = 1, eta = 1, annotations = train$wgt_viol, params, logistic = FALSE, method = "sLDA")
t2 <- Sys.time()
t2 - t1

# 50/100 iterations seems to show only small changes from 20/20 iterations 
top_words2 <- top.topic.words(sldaOut$topics, num.words = 20)
topic_coeff2 <- as.data.frame(sldaOut$coef)

pred2 <- slda.predict(documents = documents, topics = sldaOut$topics, model = sldaOut$model, alpha = 1, eta = 1)

train$predicted2 <-  pred2
rmse <- mean((train$wgt_viol - train$pred2)^2)^0.5
