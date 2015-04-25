library(stringi)
library(dplyr)

options(warn = -1)

load("freq.rda")
predict_next_word <- function(words, number_of_preds = 3){
  #words <- 'when it comes'  
  words <- stri_extract_all_words(words)
  words.size <- length(words[[1]])
  
  wf <- data.frame()
  
  # if only one word given look for bigrams that start with the word
  # return best predictions
  if (words.size == 1){
    
     wf <- filter(bi.freq,grepl(paste('^',words,' ',sep=''),bigram))
     wf <- arrange(wf,desc(count))[1,]
  } else if (words.size == 2){
    
    # only take the last two words in the string
    words <- paste(words[[1]][words.size-1],words[[1]][words.size])
    wf <- filter(tri.freq,grepl(paste('^',words,' ',sep=''),trigram))
    wf <- arrange(wf,desc(count))[1,]
    
    # if no good prediction backoff to bigram
    if (is.na(wf[,1])){
       words <- unlist(strsplit(words,' '))
       words <- words[words.size]
       wf <- filter(bi.freq,grepl(paste('^',words,' ',sep=''),bigram))
       wf <- arrange(wf,desc(count))[1,]
    }
    
  } else if (words.size >= 3) {
    
    # for 3 words and higher use the quad gram to predict the next word
    words <- paste(words[[1]][words.size-2],words[[1]][words.size-1],words[[1]][words.size])
    wf <- filter(quad.freq,grepl(paste('^',words,' ',sep=''),quadgram))
    wf <- arrange(wf,desc(count))[1,]
    
    # if no good prediction backoff to trigram
    if ( is.na(wf[1])){
       words <- unlist(strsplit(words,' '))
       words <- paste(words[words.size-1],words[words.size])
       wf <- filter(tri.freq,grepl(paste('^',words,' ',sep=''),trigram))
       wf <- arrange(wf,desc(count))[1,]
    }
  
    # if no good prediction backoff to bigram
    if (is.na(wf[,1])){
       words <- unlist(strsplit(words,' '))
       wf <- filter(bi.freq,grepl(paste('^',words,' ',sep=''),bigram))
       wf <- arrange(wf,desc(count))[1,]
    }
  }
  
wf <- wf[,1]
names(wf) <- "gram"
w <- gsub(words,'',wf$gram)
w <- sub(' ','',w)

w
}

call_predict <- function(words){
  
  w <- ''
  if (nchar(words) > 0) {
     w <- predict_next_word(words)
     wl <- unlist(strsplit(w,''))
     if ( is.na(w[1])){
        w <- arrange(uni.freq,desc(count))[1,]
        w <- as.character(w$unigram)
     }
  }  else {
    w <- '<Please type some input to get a word prediction>'
  }
  w
}
