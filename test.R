
options(mc.cores=1)

data("crude")
dtm <- DocumentTermMatrix(crude)
dtm <- removeSparseTerms(dtm,.95)
dim(dtm)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tg <- TermDocumentMatrix(crude, control = list(tokenize = TrigramTokenizer))
  
dm <- removeSparseTerms(bdm,.75)
tm <- removeSparseTerms(tg,.75)

inspect(dm)
inspect(tm)
