library(MCMCpack)
library(lda)
library(tm)
library(glmnet)
library(tidyverse)
library(slam)
library(randomForest)

set.seed(12)
M <- 100
K <- 3
nTerms <- 20
alpha <- rep(1, K)
beta <- rep(0.1, nTerms)
eta <- c(25,15,1)
sigma2 <- 2
  
#Terms <- c("dirty", "greasy", "beef", "fresh", "delicious")
Terms <- paste0("signal", seq(nTerms))
Topics <- paste("Topic", seq(K))
Documents <- paste("Document", seq(M))

Theta <- rdirichlet(M, alpha)
colnames(Theta) <- Topics
rownames(Theta) <- Documents

#zn <- rbind(c(0.45, 0.45, 0.05, 0.025, 0.025), 
#      c(0.025, 0.3, 0.3, 0.025, 0.35),
#      c(0.01, 0.01, 0.3, 0.5, 0.18))
zn <- rdirichlet(K, beta) 
zn_t <- t(zn)
colnames(zn) <- Terms
rownames(zn) <- Topics

noise <- paste0("noise", seq(2000))
docLength <- 20

generateDoc <- function(docLength, topic_dist, terms_topics_dist){
  document <- c()
  topics <- c()
  for (i in seq(docLength)){
    topic <- rmultinom(1,1, topic_dist)
    topics <- c(topics, which.max(topic))
    term <- rmultinom(1,1,terms_topics_dist[which.max(topic),])
    document <- paste(document, Terms[which.max(term)])
  }
  zBar <- c()
  for (i in seq(length(eta))){
   zBar <- c(zBar, mean(topics == i))
  }
  mu <- t(eta) %*% zBar
  y <- rnorm(1, mu, sigma2)
  noise_sample <- c()
  #for (j in seq(200)){noise_sample <- paste(noise_sample, sample(noise, 1, replace= TRUE))}
  #return(c(y, paste(document, noise_sample)))
  return(c(y, document))
}

corpus <- list()
response <-  c()
text <- c()

for (i in seq(M)){
  corpus[[i]] <- generateDoc(10, Theta[i,], zn)
  response <- rbind(response, as.numeric(corpus[[i]][1]))
  text <- rbind(text, (corpus[[i]][2]))
  
}


doc.list <- strsplit(text, "[[:space:]]+")
vocab <- c(Terms)
#vocab <- c(Terms)
#vocab <- lasso_terms
#vocab <- rf_terms
#vocab <- tfidf_terms
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
docs <- lapply(doc.list, get.terms)

doc_totals <- sapply(docs, length)

response_valid <- response[doc_totals > 0]
docs_valid <- docs[doc_totals > 0]

set.seed(200)
k = 3
params <- sample(c(-1, 1), k, replace=TRUE)
t1 <- Sys.time()
slda_trained <- slda.em(documents = docs_valid, vocab =  vocab, K = k, num.e.iterations = 20, num.m.iterations = 1000, variance = 50, 
                        alpha = 1, eta = 0.1, annotations = response_valid, params, logistic = FALSE, method = "sLDA")
t2 <- Sys.time()
t2 - t1

top_words<- top.topic.words(slda_trained$topics, num.words = 10)
topic_coeff <- as.data.frame(slda_trained$coef)

pred_valid <- slda.predict(documents = docs_valid, topics = slda_trained$topics, model = slda_trained$model, 
                           alpha = 0.5, eta = 0.1) 

mse <- mean((response_valid - pred_valid)**2)


### Lasso ###
docs_dtm <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(docs_dtm, control=list(wordLengths=c(3, 30)))

dtm_model <- sparseMatrix( i = dtm$i, j=dtm$j, x =dtm$v, dimnames = dtm$dimnames)

cvfit <- cv.glmnet(dtm_model, response, nfolds = 5, family = "gaussian", type.measure = "mse")
plot(cvfit)
coeffs <- as.data.frame(as.matrix(coef(cvfit, s = "lambda.min")))
names(coeffs) <- c("coefficient")

coeffs_nonzero <- coeffs %>% rownames_to_column(var = "term") %>% filter(coefficient != 0 & term != "(Intercept)") %>% select(term)
lasso_terms <-coeffs_nonzero[[1]]

### Random Forest ###
dtm_rf <- cbind(as.data.frame(as.matrix(dtm)), response)
rf_fit <- randomForest(response~., data = dtm_rf, importance = TRUE)
rf_fit
varImpPlot(rf_fit)
imp_terms <- importance(rf_fit, type = 1)
rf_terms <- row.names(imp_terms[order(-imp_terms[,1]), , drop = FALSE])[1:20]


### TF-IDF ###
summary(col_sums(dtm))
term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)
dtm_filter <- dtm[,term_tfidf > 0.025]
dtm_filter$dimnames

tfidf_terms <- dtm_filter$dimnames[[2]]

### Chi-squared ###
terms <- as.data.frame(as.matrix(dtm))

chi_text <- function(x){
  a <- as.numeric(sum((x==1)*(response==1)))
  b <- as.numeric(sum((x==1)*(response==0)))
  c <- as.numeric(sum((x==0)*(response==1)))
  d <- as.numeric(sum((x==0)*(response==0)))
  (length(x)*((a*d - c*b)**2))/((a+c)*(b+d)*(a+b)*(c+d))
}

chi_stats <- sapply(terms, chi_text)
chi_sorted <- sort(chi_stats, decreasing = TRUE)
vocab_chi <- names(chi_sorted[1:2000])

