
predict_next_word <- function(words, number_of_preds = 3){
library(stringi)
library(dplyr)
  
  words <- stri_extract_all_words(words)
  words.size <- length(words[[1]])
  
  # if only one word given look for bigrams that start with the word
  # return best predictions
  if (words.size == 1){
    
     bi <- filter(bi.freq,grepl(paste('^',words,' ',sep=''),bigram))
     bi <- arrange(bi,desc(count))[1:3,]
     bi[,1]    
  } else if (words.size > 1){
    
    # only take the last two words in the string
    words <- paste(words[[1]][words.size-1],words[[1]][words.size])
    tri <- filter(tri.freq,grepl(paste('^',words,' ',sep=''),trigram))
    tri <- arrange(tri,desc(count))[1:3,]
    tri[,1]    
    
    
  }
  
  
}