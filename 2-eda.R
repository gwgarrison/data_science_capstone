source("1-setup.R")

# set to use only one thread since default results in "invalid 'times' argument in
#  NGramTokenizer
options(mc.cores=1)
# Trigrams
#http://stackoverflow.com/questions/19615181/finding-ngrams-in-r-and-comparing-ngrams-across-corpora
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bdm <- TermDocumentMatrix(crps, control = list(tokenize = BigramTokenizer))
dm <- removeSparseTerms(bdm, 0.9)
inspect(dm[1:5,1:5])

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm <- TermDocumentMatrix(crps, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)
inspect(tdm[1:5,1:5])

p <- ggplot(subset(wf, freq>1000), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

# word cloud
wordcloud(names(freq), freq, min.freq=1000, colors=brewer.pal(6, "Dark2"))
