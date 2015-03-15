library(ngram)

options(mc.cores=1)

data("crude")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tg <- TermDocumentMatrix(crude, control = list(tokenize = TrigramTokenizer))
  
dm <- removeSparseTerms(bdm,.75)
tm <- removeSparseTerms(tg,.75)

inspect(dm)
inspect(tm)

# play with ngram package
bigram <- ngram(doc,n = 2)
trigram <- ngram(doc,n = 3)