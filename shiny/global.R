library(stringi)
library(dplyr)

load("freq.rda")
predict_next_word <- function(words, number_of_preds = 3){
  
  words <- stri_extract_all_words(words)
  words.size <- length(words[[1]])
  
  wf <- data.frame()
  
  # if only one word given look for bigrams that start with the word
  # return best predictions
  if (words.size == 1){
    
     wf <- filter(bi.freq,grepl(paste('^',words,' ',sep=''),bigram))
     wf <- arrange(wf,desc(count))[1,]
#     wf[,1]    
  } else if (words.size == 2){
    
    # only take the last two words in the string
    words <- paste(words[[1]][words.size-1],words[[1]][words.size])
    wf <- filter(tri.freq,grepl(paste('^',words,' ',sep=''),trigram))
    wf <- arrange(wf,desc(count))[1,]
    
  } else if (words.size == 3) {
    
    # only take the last two words in the string
    words <- paste(words[[1]][words.size-1],words[[1]][words.size])
    wf <- filter(quad.freq,grepl(paste('^',words,' ',sep=''),quadgram))
    wf <- arrange(wf,desc(count))[1,]
  
  } else if (words.size > 3 ){
    
    words <- paste(words[[1]][words.size-1],words[[1]][words.size])
    wf <- filter(quad.freq,grepl(paste('^',words,' ',sep=''),quadgram))
    wf <- arrange(wf,desc(count))[1,]
    
  } 

#print(wf)  
#print(words)
#print(wf[,1])
wf <- wf[,1]
names(wf) <- "gram"
w <- gsub(words,'',wf$gram)
w <- sub(' ','',w)
if ( is.na(w[1]) ){
  
  w <- arrange(uni.freq,desc(count))[1,]
  w <- as.character(w$unigram)
  #print()
} 
#print(words,row.names= FALSE)
print(length(words))
w
  
}

call_predict <- function(words){
  
  w <- predict_next_word(words)
  w
  
}
