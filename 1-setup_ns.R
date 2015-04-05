# exploratory analysis

# setup
library(tm);library(SnowballC);#library(qdap);library(qdapDictionaries);
library(plyr);library(RColorBrewer);library(ggplot2); library(scales);library(wordcloud)
library(RWeka);library(slam);library(dplyr)

# setting this prevents an error in RWeka package
options(mc.cores=1)

blogs <- readLines("samples/blogs.txt")
news <- readLines("samples/news.txt")
twitter <- readLines("samples/twitter.txt")

# combine docs
doc <- c(blogs,news,twitter)

#system.time(crps <- Corpus(DirSource(), readerControl = list(language="en_US")))
raw.crps <- Corpus(VectorSource(doc), readerControl = list(language="english"))
crps <- raw.crps #Corpus(VectorSource(doc), readerControl = list(language="english"))

# get rid of the numbers and punctuation
crps <- tm_map(crps,removeNumbers)
crps <- tm_map(crps,removePunctuation)

# remove profanity
profanity.df <- read.table("profanity.txt")
profanity <- as.character(profanity.df[,1])

crps <- tm_map(crps,removeWords,profanity)
#crps <- tm_map(crps,stripWhitespace)

# function to get rid of non standard characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
crps <- tm_map(crps, toSpace, "/|@|\\||\\)|\\(")
crps <- tm_map(crps, content_transformer(tolower))
# make sure to stem and remove stopwords after other transforms
#crps <- tm_map(crps,stemDocument)

#remove stop words from the corpus
crps <- tm_map(crps, removeWords, stopwords("english"))
crps <- tm_map(crps,stripWhitespace)

inspect(crps[10000])
inspect(raw.crps[10000])

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
QuadgramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))
bdm <- TermDocumentMatrix(crps, control = list(tokenize = BigramTokenizer))
tdm <- TermDocumentMatrix(crps, control = list(tokenize = TrigramTokenizer))
qdm <- TermDocumentMatrix(crps, control = list(tokenize = QuadgramTokenizer))
udm <- TermDocumentMatrix(crps)
bm <- removeSparseTerms(bdm, 0.99997)
tm <- removeSparseTerms(tdm, 0.99997)
um <- removeSparseTerms(udm, 0.99997)
qm <- removeSparseTerms(qdm, 0.99997)
tm <- as.matrix(tm)
bm <- as.matrix(bm)
um <- as.matrix(um)
#qm <- as.matrix(qm)
b.freq <- as.data.frame(rowSums(bm))
t.freq <- as.data.frame(rowSums(tm))
u.freq <- as.data.frame(rowSums(um))
#q.freq <- as.data.frame(rowSums(qm))
b.freq$bigram <- row.names(b.freq)
t.freq$trigram <- row.names(t.freq)
u.freq$unigram <- row.names(u.freq)
#q.freq$quadgram <- row.names(q.freq)
uni_ns.freq <- tbl_df(data.frame(u.freq[,2],u.freq[,1]))
bi_ns.freq <- tbl_df(data.frame(b.freq[,2],b.freq[,1]))
tri_ns.freq <- tbl_df(data.frame(t.freq[,2],t.freq[,1]))
#quad_ns.freq <- tbl_df(data.frame(q.freq[,2],q.freq[,1]))
names(bi_ns.freq) <- c("bigram","count")
names(tri_ns.freq) <- c("trigram","count")
names(uni_ns.freq) <- c("unigram","count")
#names(quad_ns.freq) <- c("quadgram","count")

save(bi_ns.freq,tri_ns.freq,uni_ns.freq,file = "shiny/freq_ns.rda")
