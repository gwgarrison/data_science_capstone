#setwd("F:/data_science_capstone/data/Coursera-SwiftKey/final/en_US")

sample.file <- function(file.path, size=.10){
  
  fileName=file.path
  samp.file <- vector()
  conn=file(fileName,open="r")
  linn=readLines(conn)
  
  for (i in 1:length(linn)){
    if (rbinom(1,1,size)){
       samp.file <- append(samp.file,linn[i])      
    }
  }
  
  close(conn)
  samp.file
}

#sample.file("test.txt")

