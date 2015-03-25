source("1-setup.R")

dtm <- DocumentTermMatrix(crps)
#tdm <- TermDocumentMatrix(crps)
#inspect(dtm[1:5, 1000:1005])
# Explore the corpus.
#findFreqTerms(dtm, lowfreq=30)
#findAssocs(dtm, "data", corlimit=0.3)
d <- removeSparseTerms(dtm,.9999)
freq <- sort(colSums(as.matrix(d)), decreasing=TRUE)
wf   <- data.frame(word=names(freq), freq=freq)

# set to use only one thread since default results in "invalid 'times' argument in
#  NGramTokenizer
# Trigrams
#http://stackoverflow.com/questions/19615181/finding-ngrams-in-r-and-comparing-ngrams-across-corpora
BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
bdm <- TermDocumentMatrix(crps, control = list(tokenize = BigramTokenizer))
tdm <- TermDocumentMatrix(crps, control = list(tokenize = TrigramTokenizer))
udm <- TermDocumentMatrix(crps)
dm <- removeSparseTerms(bdm, 0.9999)
tm <- removeSparseTerms(tdm, 0.9999)
um <- removeSparseTerms(udm, 0.9999)
tm <- as.matrix(tm)
dm <- as.matrix(dm)
um <- as.matrix(um)
b.freq <- as.data.frame(rowSums(dm))
t.freq <- as.data.frame(rowSums(tm))
u.freq <- as.data.frame(rowSums(um))
b.freq$bigram <- row.names(b.freq)
t.freq$trigram <- row.names(t.freq)
u.freq$unigram <- row.names(u.freq)
uni.freq <- tbl_df(data.frame(u.freq[,2],u.freq[,1]))
bi.freq <- tbl_df(data.frame(b.freq[,2],b.freq[,1]))
tri.freq <- tbl_df(data.frame(t.freq[,2],t.freq[,1]))
names(bi.freq) <- c("bigram","count")
names(tri.freq) <- c("trigram","count")
names(uni.freq) <- c("unigram","count")


p <- ggplot(subset(wf, freq>2000), aes(word, freq))
p <- p + geom_bar(stat="identity",fill = "blue")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) + coord_flip()
p

# word cloud
set.seed(77)
system.time(wordcloud(names(freq), freq, min.freq=800, colors=brewer.pal(6, "Dark2")))

# save current work
system.time(save.image("work_20150323.rda"))
system.time(load("work_20150323.rda"))

