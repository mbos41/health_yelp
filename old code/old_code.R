freq <- colSums(as.matrix(dtm_lim))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[ord[1:10]]
terms <- as.data.frame(as.matrix(dtm_lasso))

chi_text <- function(x){
  a <- as.numeric(sum((x==1)*(sample_lasso$very_severe==1)))
  b <- as.numeric(sum((x==1)*(sample_lasso$very_severe==0)))
  c <- as.numeric(sum((x==0)*(sample_lasso$very_severe==1)))
  d <- as.numeric(sum((x==0)*(sample_lasso$very_severe==0)))
  (length(x)*((a*d - c*b)**2))/((a+c)*(b+d)*(a+b)*(c+d))
}
#check_sum <- sapply(terms, sum)
chi_stats <- sapply(terms, chi_text)
chi_sorted <- sort(chi_stats, decreasing = TRUE)
vocab_chi <- names(chi_sorted[1:2000])
