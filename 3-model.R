library(dplyr)
library(stringr)
library(qdap)
library(stringi)


load("freq.rda")


word <- '^her'
bi <- 'a little'

words <- lapply(bi,bag_o_words)
words <- stri_extract_all_words(bi)

filter(bi.freq,grepl(word,bigram))
arrange(filter(tri.freq,grepl(paste('^',bi,sep=''),trigram)),desc(count))[1:3,]
