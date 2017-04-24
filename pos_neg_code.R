prepare_text <- function(raw, sentiment){
  if (sentiment == "p") {text <- gsub(" ", " p_", paste("p_", raw, sep=""))}
  if (sentiment == "n") {text <- gsub(" ", " n_", paste("n_", raw, sep=""))}
  text <- gsub("[^[:alnum:][:space:]_]", "", text) # remove everything not alphanumeric, space, or "_"
  text <- gsub("[[:cntrl:]]", "", text)  # replace control characters with space
  text <- gsub("^[[:space:]]+", "", text) # remove whitespace at beginning of documents
  text <- gsub("[[:space:]]+$", "", text) # remove whitespace at end of documents
  text <- tolower(text)  # force to lowercase
  text
}

stop_words_p <- paste("p_", c(stopwords("SMART"), "food", "good", "place", ""), sep="")
stop_words_n <- paste("n_", c(stopwords("SMART"), "food", "good", "place", ""), sep="")
stop_words <- c(stop_words_p, stop_words_n)

vocab_words_p <- paste("p_", top_terms, sep="")
vocab_words_n <- paste("n_", top_terms, sep="")
vocab <- c(vocab_words_p, vocab_words_n)


train_p <- prepare_text(sample_train$pos_text, "p")
train_n <- prepare_text(sample_train$neg_text, "n")
train_all <- paste(train_p, train_n, sep=" ")

valid_p <- prepare_text(sample_valid$pos_text, "p")
valid_n <- prepare_text(sample_valid$neg_text, "n")
valid_all <- paste(valid_p, valid_n, sep=" ")