# exploratory analysis

# setup
library(tm);library(SnowballC);#library(qdap);library(qdapDictionaries);
library(plyr);library(RColorBrewer);library(ggplot2); library(scales);library(wordcloud)
library(RWeka)

# get line count for input file
#tmp <- readChar("en_US.blogs.txt", file.info("en_US.blogs.txt")$size)
#length(gregexpr("\n",tmp)[[1L]])

#system.time(doc <- readLines("en_US.blogs.txt",n = 100))

# get only a sample of the file
#system.time(doc <- sample.file("en_US.blogs.txt",.02))
#write(doc,"sample.txt")

doc <- readLines("sample.txt")

#system.time(crps <- Corpus(DirSource(), readerControl = list(language="en_US")))
raw.crps <- Corpus(VectorSource(doc), readerControl = list(language="english"))
crps <- Corpus(VectorSource(doc), readerControl = list(language="english"))

crps <- tm_map(crps,removeNumbers)
crps <- tm_map(crps,removePunctuation)
crps <- tm_map(crps,stripWhitespace)

# function to get rid of non standard characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
crps <- tm_map(crps, toSpace, "/|@|\\||\\)|\\(")
crps <- tm_map(crps, content_transformer(tolower))
# make sure to stem and remove stopwords after other transforms
crps <- tm_map(crps,stemDocument)
crps <- tm_map(crps, removeWords, stopwords("english"))
inspect(crps[100])
inspect(raw.crps[100])

dtm <- DocumentTermMatrix(crps)
inspect(dtm[1:5, 1000:1005])
# Explore the corpus.
findFreqTerms(dtm, lowfreq=100)
findAssocs(dtm, "data", corlimit=0.6)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf   <- data.frame(word=names(freq), freq=freq)


