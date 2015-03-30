library(stringi)
library(dplyr)

predict_next_word <- function(words, number_of_preds = 3){
  
  words <- stri_extract_all_words(words)
  words.size <- length(words[[1]])
  
  # if only one word given look for bigrams that start with the word
  # return best predictions
  if (words.size == 1){
    
     bi <- filter(bi.freq,grepl(paste('^',words,' ',sep=''),bigram))
     bi <- arrange(bi,desc(count))[1:3,]
    
  }
  
  
}