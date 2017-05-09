library(lda)
library(tidyverse)
library(stringr)
library(tm)
library(glmnet)
library(slam)
library(pROC)
library(randomForest)

setwd("Documents/COMP_790/health_yelp")

raw <- read.csv('data/analysis_data.csv')
raw$pos_text_temp <- trimws(as.character(raw$pos_text))
raw$neg_text_temp <- trimws(as.character(raw$neg_text))

filter_data <- raw %>% mutate(remove1 = (ifelse((as.character(restaurant_id) == as.character(prev_rest) 
                                            & (as.Date(date) - as.Date(prev_date)) < 15), 1, 0)),
                              remove2 = (ifelse((nchar(pos_text_temp) == 0) & (nchar(neg_text_temp) == 0), 1, 0))) %>% 
  filter((remove1 == 0) & (remove2 == 0))
                                  

analysis <- filter_data %>% 
  mutate(year = str_sub(date, 1, 4),
         wgt_viol = (X.*1 + X..*2 + X...*5),
         severe = as.integer((X... != 0)),
         severe_count = X...,
         very_severe = as.integer((severe_count >= 3)),
         log_wgt_viol = log(wgt_viol + 1),
         pos_text = ifelse(nchar(pos_text_temp) == 0, 'blank', pos_text_temp),
         neg_text = ifelse(nchar(neg_text_temp) == 0, 'blank', neg_text_temp),
         all_text = ifelse(nchar(trimws(paste(pos_text_temp, neg_text_temp, set = ""))) == 0, 'blank', paste(pos_text_temp, neg_text_temp)),
         unhygienic = ifelse(wgt_viol >= 20, 1, 0)) %>% 
  select(date, restaurant_id, pos_text, neg_text, all_text, year, wgt_viol, log_wgt_viol, unhygienic, severe, severe_count, very_severe)

analysis %>% group_by(year) %>% summarise(count = n())

test <- analysis %>%
  filter(year > 2012)

train_valid <- analysis %>% 
  filter(year < 2013) %>% 
  mutate()

summary(train_valid$very_severe)

positive_sample <- sample(which(train_valid$very_severe == 1), size = 1000, replace = F)
negative_sample <- sample(which(train_valid$wgt_viol == 0), size = 1000, replace = F)
sample_chi <- train_valid[c(positive_sample, negative_sample),]
docs_chi <- Corpus(VectorSource(sample_chi$neg_text))
docs_chi <-tm_map(docs_chi,content_transformer(tolower))
docs_chi <- tm_map(docs_chi, removePunctuation)
docs_chi <- tm_map(docs_chi, removeNumbers)
docs_chi <- tm_map(docs_chi, removeWords, stopwords("english"))
docs_chi <- tm_map(docs_chi, stripWhitespace)
max_docs <- ceiling(length(docs_chi)/2)
dtm_chi <- DocumentTermMatrix(docs_chi, control=list(wordLengths=c(3, 30), bounds = list(global = c(10,max_docs))))

terms <- as.data.frame(as.matrix(dtm_chi))

chi_text <- function(x){
  a <- as.numeric(sum((x==1)*(sample_chi$very_severe==1)))
  b <- as.numeric(sum((x==1)*(sample_chi$very_severe==0)))
  c <- as.numeric(sum((x==0)*(sample_chi$very_severe==1)))
  d <- as.numeric(sum((x==0)*(sample_chi$very_severe==0)))
  (length(x)*((a*d - c*b)**2))/((a+c)*(b+d)*(a+b)*(c+d))
}
#check_sum <- sapply(terms, sum)
chi_stats <- sapply(terms, chi_text)
chi_sorted <- sort(chi_stats, decreasing = TRUE)
vocab_chi <- names(chi_sorted[1:2000])

set.seed(10)
samples <- sample(nrow(train_valid), nrow(train_valid)*0.75)
sample_train <- train_valid[samples,]
sample_valid <- train_valid[-samples,]
#full_valid <- analysis %>% filter(year >= 2011 & year <= 2012)
#positive_sample <- sample(which(full_valid$unhygienic == 1), size = 2000, replace = F)
#negative_sample <- sample(which(full_valid$unhygienic == 0), size = 2000, replace = F)
#sample_valid <- full_valid[c(positive_sample, negative_sample),]
#sample_valid <- analysis %>% filter(year == 2012)
summary(sample_train$severe)
summary(sample_valid$severe)


# Clean up text for creating restaurant document term matrix
docs <- Corpus(VectorSource(sample_train$neg_text))
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
#docs <- tm_map(docs,stemDocument)
#Create document-term matrix (Limit to terms appearing in at least 10 restaurants and no more than 200 restaurants)
#max_docs <- ceiling(length(docs)/5)
#dtm_lim <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 30), bounds = list(global = c(10,max_docs))))
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 30), bounds = list(global = c(10,Inf))))

# Filter DTM by tfidf
summary(col_sums(dtm))
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)
t_sorted <- sort(term_tfidf, decreasing = TRUE)[2:10654]
sorted <- as.numeric(names(sort(term_tfidf, decreasing = TRUE)[1:2000]))
vocab_tfidf <- dtm$dimnames[[2]][sorted]


dtm_model <- sparseMatrix( i = dtm_lasso$i, j=dtm_lasso$j, x =dtm_lasso$v, dimnames = dtm_lasso$dimnames)
cvfit <- cv.glmnet(dtm_model, factor(sample_lasso$very_severe), nfolds = 5, family = "binomial", type.measure = "class")
plot(cvfit)
coeffs <- as.data.frame(as.matrix(coef(cvfit, s = "lambda.min")))
names(coeffs) <- c("coefficient")
coeffs_nonzero <- coeffs %>% rownames_to_column(var = "term") %>% filter(coefficient != 0 & term != "(Intercept)") %>% select(term, coefficient)
vocab_lasso <-coeffs_nonzero[[1]]


######################################################################
###############       Supervised LDA approach          ###############
######################################################################
# pre-processing:

prepare_text <- function(raw){
  text <- gsub("[^[:alnum:][:space:]_]", "", raw) # remove everything not alphanumeric, space, or "_"
  text <- gsub("[[:cntrl:]]", "", text)  # replace control characters with space
  text <- gsub("^[[:space:]]+", "", text) # remove whitespace at beginning of documents
  text <- gsub("[[:space:]]+$", "", text) # remove whitespace at end of documents
  text <- tolower(text)  # force to lowercase
  text
}

vocab <- vocab_tfidf

train_all <- prepare_text(sample_train$neg_text)
valid_all <- prepare_text(sample_valid$neg_text)
test_all <- prepare_text(test$neg_text)

create_docs <- function(text_in){
  # tokenize on space and output as a list:
  doc.list <- strsplit(text_in, "[[:space:]]+")

  # compute the table of terms:
  #term.table <- table(unlist(doc.list))
  #term.table <- sort(term.table, decreasing = TRUE)
  
  # remove terms that are stop words or occur fewer than X times:
  #del <- names(term.table) %in% stop_words | term.table < 25 | term.table > 1000
  #term.table <- term.table[!del]
  #if (train_vocab) {vocab <<- names(term.table)}
  
  # now put the documents into the format required by the lda package:
  get.terms <- function(x) {
    index <- match(x, vocab)
    index <- index[!is.na(index)]
    rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  }
  
  docs <- lapply(doc.list, get.terms)
  return(docs)
}  

docs_train_full <- create_docs(train_all)
doc_train_totals <- sapply(docs_train_full, length)

values_train <- sample_train$severe[doc_train_totals > 0]
docs_train <- docs_train_full[doc_train_totals > 0]

docs_valid_full <- create_docs(valid_all)
doc_valid_totals <- sapply(docs_valid_full, length)

values_valid <- sample_valid$severe[doc_valid_totals > 0]
docs_valid <- docs_valid_full[doc_valid_totals > 0]

docs_test_full <- create_docs(test_all)
doc_test_totals <- sapply(docs_test_full, length)

values_test <- test$severe[doc_test_totals > 0]
docs_test <- docs_test_full[doc_test_totals > 0]


set.seed(200)
k = 10
params <- sample(c(-1, 1), k, replace=TRUE)
t1 <- Sys.time()
slda_trained <- slda.em(documents = docs_train, vocab =  vocab, K = k, num.e.iterations = 10, num.m.iterations = 5, variance = .25, 
                   alpha = 50/k, eta = 0.1, annotations = values_train, params, logistic = TRUE, method = "sLDA")
t2 <- Sys.time()
t2 - t1

slda_topics <- t(slda_trained$topics)
lda_trained <- lda.collapsed.gibbs.sampler(documents = docs_train, K =k, vocab = vocab, num.iterations = 100, alpha = 50/k,
                            eta = 0.1, burnin = 20, compute.log.likelihood = TRUE)
top_words_lda<- top.topic.words(lda_trained$topics, num.words = 10)
lda_docs <- t(lda_trained$document_sums)
likelihoods <- t(lda_trained$log.likelihoods)

top_words<- top.topic.words(slda_trained$topics, num.words = 10)
topic_coeff <- as.data.frame(slda_trained$coef)

pred_valid <- slda.predict(documents = docs_valid, topics = slda_trained$topics, model = slda_trained$model, 
                          alpha = 50/k, eta = 0.1) 

predicted <-  exp(pred_valid) / (1 + exp(pred_valid))
valid_results <- as.data.frame(cbind(predicted, values_valid))
accuracy <- mean(values_valid == (predicted > 0.50), na.rm = TRUE)
#mse <- mean((pred_valid - values_valid)**2)


ggplot(data = valid_results) + geom_histogram(mapping = aes(x = predicted)) + facet_wrap(~values_valid)
curve1 <- roc(valid_results$values_valid, valid_results$V1)
plot(curve1)
#rmse <- mean((sample_train$wgt_viol - exp(sample_train$predicted40))^2)^0.5

valid_check <- sample_valid[doc_valid_totals > 0,]
valid_check$predicted <- predicted
valid_check <- valid_check %>% select(pos_text, neg_text, wgt_viol, unhygienic, predicted)
