# exploratory analysis

# setup
library(tm);library(SnowballC);#library(qdap);library(qdapDictionaries);
library(plyr);library(RColorBrewer);library(ggplot2); library(scales);library(wordcloud)
library(RWeka);library(slam)

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

crps <- tm_map(crps,removeNumbers)
crps <- tm_map(crps,removePunctuation)
crps <- tm_map(crps,stripWhitespace)

# function to get rid of non standard characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
crps <- tm_map(crps, toSpace, "/|@|\\||\\)|\\(")
crps <- tm_map(crps, content_transformer(tolower))
# make sure to stem and remove stopwords after other transforms
#crps <- tm_map(crps,stemDocument)
#crps <- tm_map(crps, removeWords, stopwords("english"))
crps <- tm_map(crps,stripWhitespace)

inspect(crps[100])
inspect(raw.crps[100])

dtm <- DocumentTermMatrix(crps)
inspect(dtm[1:5, 1000:1005])
# Explore the corpus.
findFreqTerms(dtm, lowfreq=30)
findAssocs(dtm, "data", corlimit=0.3)
d <- removeSparseTerms(dtm,.6)
#freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
dtm.ru <- rollup(dtm, 2, na.rm=TRUE, FUN = sum)
dtm.df <- as.data.frame(as.matrix(dtm.ru))
wf   <- data.frame(word=names(freq), freq=freq)


