# exploratory analysis

# setup
library(tm);library(SnowballC);#library(qdap);library(qdapDictionaries);
library(plyr);library(RColorBrewer);library(ggplot2); library(scales);library(wordcloud)
library(RWeka);library(slam);library(dplyr)

# setting this prevents an error in RWeka package
options(mc.cores=1)

# get line count for input file
#tmp <- readChar("en_US.blogs.txt", file.info("en_US.blogs.txt")$size)
#length(gregexpr("\n",tmp)[[1L]])

#system.time(doc <- readLines("en_US.blogs.txt",n = 100))

# get only a sample of the file
#source("sample.file.R")
# system.time(blogs <- sample.file("final/en_US/en_US.blogs.txt",.01))
#system.time(news <- sample.file("final/en_US/en_US.news.txt",.01))
# system.time(twitter <- sample.file("final/en_US/en_US.twitter.txt",.01))
# write(blogs,"samples/blogs.txt")
# write(news,"samples/news.txt")
# write(twitter,"samples/twitter.txt")

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
#crps <- tm_map(crps, removeWords, stopwords("english"))
crps <- tm_map(crps,stripWhitespace)

#inspect(crps[100])
#inspect(raw.crps[100])

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

save(bi.freq,tri.freq,uni.freq,freq,wf,file = "freq.rda")
