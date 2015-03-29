source("1-setup.R")

# save current work
system.time(save.image("work_20150323.rda"))
system.time(load("work_20150323.rda"))


wf.ordered <- transform(wf,word = reorder(word,freq))
p <- ggplot(subset(wf.ordered, freq>2000), aes(word, freq))
p <- p + geom_bar(stat="identity",fill = "blue")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) + coord_flip()
p

bf.ordered <- transform(bi.freq,bigram = reorder(bigram ,count))
p <- ggplot(subset(bf.ordered, count>800), aes(bigram, count))
p <- p + geom_bar(stat="identity",fill = "red")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) + coord_flip()
p


# word cloud
set.seed(77)
system.time(wordcloud(names(freq), freq, min.freq=800, colors=brewer.pal(6, "Dark2")))


