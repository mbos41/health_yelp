library(lda)
library(tidyverse)
library(stringr)
library(tm)
library(glmnet)
library(slam)
library(pROC)
library(rpart)
library(rpart.plot)

setwd("Documents/COMP_790/health_yelp")

raw <- read.csv('data/seattle.csv')

analysis <- raw %>%
  mutate(t20 = (inspection_penalty_score >= 20),
         t30 = (inspection_penalty_score >= 30),
         t40 = (inspection_penalty_score >= 40),
         t50 = (inspection_penalty_score >= 50)) %>% 
  filter(inspection_penalty_score == 0 | inspection_penalty_score >= 20)

mean(analysis$t20)
sum(analysis$t20)

set.seed(10)
samples <- sample(nrow(analysis), nrow(analysis)*0.5)
sample_train <- analysis[samples,]
valid_test <- analysis[-samples,]
samples2 <- sample(nrow(valid_test), nrow(valid_test)*0.5)
sample_valid <- valid_test[samples2,]
sample_test <- valid_test[-samples2,]

model <- glm(t20 ~ inspection_average_prev_penalty_scores,
             family=binomial(link='logit'),data=sample_train)
summary(model)

sample_valid$avg_pred <- predict(model, newdata = sample_valid, type = "response")
sample_test$avg_pred <- predict(model, newdata = sample_test, type = "response")
acc_avg <- mean((sample_test$avg_pred > .50) == sample_test$t20)
curve_avg <- roc(as.integer(sample_test$t20), sample_test$avg_pred)
plot(curve_avg)
curve_avg

# Clean up text for creating restaurant document term matrix
docs <- Corpus(VectorSource(sample_train$review_contents))
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
max_docs <- ceiling(length(docs)/2)
#dtm_lim <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 30), bounds = list(global = c(10,max_docs))))
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 30), bounds = list(global = c(10,Inf))))
dtm_max <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 30), bounds = list(global = c(10,max_docs))))

findAssocs(dtm, "roaches", 0.3)
findAssocs(dtm, "rat", 0.3)
findAssocs(dtm, "dirty", 0.25)

dtm_matrix <- as.data.frame(as.matrix(dtm))
rat_docs <- sample_train[dtm_matrix$rat > 0,]
roach_docs <- sample_train[dtm_matrix$roach > 0,]

# Filter DTM by tfidf
summary(col_sums(dtm))
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)
t_sorted <- sort(term_tfidf, decreasing = TRUE)
sorted <- as.numeric(names(sort(term_tfidf, decreasing = TRUE)[1:4000]))
vocab_tfidf <- dtm$dimnames[[2]][sorted]

#terms <- as.data.frame(as.matrix(dtm_max))

chi_text <- function(x){
  a <- as.numeric(sum((x==1)*(sample_train$t20==TRUE)))
  b <- as.numeric(sum((x==1)*(sample_train$t20==FALSE)))
  c <- as.numeric(sum((x==0)*(sample_train$t20==TRUE)))
  d <- as.numeric(sum((x==0)*(sample_train$t20==FALSE)))
  (length(x)*((a*d - c*b)**2))/((a+c)*(b+d)*(a+b)*(c+d))
}
#check_sum <- sapply(terms, sum)
#chi_stats <- sapply(terms, chi_text)
#chi_sorted <- sort(chi_stats, decreasing = TRUE)
#vocab_chi <- names(chi_sorted[1:4000])


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

train_all <- prepare_text(sample_train$review_contents)
valid_all <- prepare_text(sample_valid$review_contents)
test_all <- prepare_text(sample_test$review_contents)

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

values_train <- as.integer(sample_train$t20[doc_train_totals > 0])
docs_train <- docs_train_full[doc_train_totals > 0]

docs_valid_full <- create_docs(valid_all)
doc_valid_totals <- sapply(docs_valid_full, length)

values_valid <- as.integer(sample_valid$t20[doc_valid_totals > 0])
docs_valid <- docs_valid_full[doc_valid_totals > 0]

docs_test_full <- create_docs(test_all)
doc_test_totals <- sapply(docs_test_full, length)

values_test <- as.integer(sample_test$t20[doc_test_totals > 0])
docs_test <- docs_test_full[doc_test_totals > 0]

set.seed(200)
k = 10
#params <- c(-50, -50, -50, 50, 50)
params <- sample(c(-1, 1), k, replace=TRUE)
t1 <- Sys.time()
slda_trained <- slda.em(documents = docs_train, vocab =  vocab, K = k, num.e.iterations = 20, num.m.iterations = 50, variance = .25, 
                        alpha = 50/k, eta = 0.1, annotations = values_train, params, logistic = TRUE, method = "sLDA")
t2 <- Sys.time()
t2 - t1

lda_trained <- lda.collapsed.gibbs.sampler(documents = docs_train, K =k, vocab = vocab, num.iterations = 100, alpha = 50/k,
                                           eta = 0.1, burnin = 20, compute.log.likelihood = TRUE)
top_words_lda<- top.topic.words(lda_trained$topics, num.words = 10)
lda_ll <- t(lda_trained$log.likelihoods)
top_words<- top.topic.words(slda_trained$topics, num.words = 10)
topic_coeff <- as.data.frame(slda_trained$coef)

doc_sums <- t(slda_trained$document_sums)
slda_topics <- t(slda_trained$topics)
words <- row.names(slda_topics)

pred_train <- slda.predict(documents = docs_train, topics = slda_trained$topics, model = slda_trained$model,
                           alpha = 50/k, eta = 0.1) 
predicted_train <-  exp(pred_train) / (1 + exp(pred_train))

pred_valid <- slda.predict(documents = docs_valid, topics = slda_trained$topics, model = slda_trained$model,
                           alpha = 50/k, eta = 0.1) 

pred_test <- slda.predict(documents = docs_test, topics = slda_trained$topics, model = slda_trained$model,
                           alpha = 50/k, eta = 0.1) 

predicted <-  exp(pred_test) / (1 + exp(pred_test))
test_results <- as.data.frame(cbind(predicted, values_test))
acc_slda <- mean(values_test == (predicted > 0.5), na.rm = TRUE)
curve_slda <- roc(test_results$values_test, test_results$V1)
plot(curve_slda)
curve_slda

ggplot(data = test_results) + geom_histogram(mapping = aes(x = predicted)) + facet_wrap(~values_test)

#sample_valid2 <- sample_valid[doc_valid_totals > 0,]
#sample_valid2$slda_pred <- predicted

sample_train2 <- sample_train[doc_train_totals > 0,]
sample_train2$slda_pred <- predicted_train

sample_test2 <- sample_test[doc_test_totals > 0,]
sample_test2$slda_pred <- predicted

sample_train2$t20 <- as.factor(sample_train2$t20)
levels(sample_train2$t20) <- c("hygienic", "unhygienic")
tree_fit <- rpart(t20 ~ inspection_average_prev_penalty_scores + slda_pred, data = sample_train2, method = "class")
prp(tree_fit, tweak = 1.5, varlen = 0)

sample_train2$prev_avg = sample_train2$inspection_average_prev_penalty_scores
sample_train2$slda_prob = sample_train2$slda_pred
tree_plot <- rpart(t20 ~ prev_avg + slda_prob, data = sample_train2, method = "class")
prp(tree_plot, tweak = 1.25, varlen = 0)


#sample_valid2$comb_pred <- predict(tree_fit, newdata = sample_valid2, type = "prob")[,2]
#accuracy_comb <- mean(sample_valid2$t20 == (sample_valid2$comb_pred > 0.5), na.rm = TRUE)
#curve1 <- roc(as.integer(sample_valid2$t20), sample_valid2$comb_pred)

sample_test2$comb_pred <- predict(tree_fit, newdata = sample_test2, type = "prob")[,2]
acc_comb <- mean(sample_test2$t20 == (sample_test2$comb_pred > 0.5), na.rm = TRUE)
curve_comb <- roc(as.integer(sample_test2$t20), sample_test2$comb_pred)
curve1
plot(curve1)
str(curve1)

plot(curve_avg, col = 'blue', main = "ROC Comparison")
plot(curve_slda, col = 'red', add = TRUE)
plot(curve_comb, col = 'green', add = TRUE)
legend(0.3, 0.3, c("History", "sLDA", "Combined"), lty=c(1,1,1), lwd=c(1,1,1), col = c("blue", "red", "green"), bty = "n")

sample_test2$correct <- as.factor(((sample_test2$comb_pred > 0.5) == sample_test2$t20))
levels(sample_test2$correct) <- c("Incorrect", "Correct")
ggplot(data = sample_test2) + geom_point(mapping = aes(x = avg_pred, y = slda_pred)) + facet_wrap(~correct) +
  xlab("History") + ylab("sLDA")
plot(sample_test2$avg_pred, sample_test2$slda_pred, xlab = "History", ylab = "sLDA")
title()
