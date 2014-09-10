library("NMF")
library("reshape2")
library("Matrix")

rm(list = ls())

datLoc <- "/Users/Ash/Documents/database/bagofwords/docword.kos.txt"
vocLoc <- "/Users/Ash/Documents/database/bagofwords/vocab.kos.txt"

nb <- 100 # How many groups to formget

print("Loading data...")
lib.n <- read.csv(datLoc,skip=0,nrows=1,header=F)
voc.n <- read.csv(datLoc,skip=1,nrows=1,header=F)
word.n <- read.csv(datLoc,skip=2,nrows=1,header=F)
d <- read.csv(datLoc,sep=" ",skip=3,header=F)
colnames(d) <- c("docID", "wordID", "count")
voc <- read.csv(vocLoc,sep=" ",header=F)
colnames(voc) <- c("word")
voc$wordID <- 1:nrow(voc)

# get word occurance count per doc
#d.wordFreq <- as.data.frame(table(d[,c("wordID")]))
#d.wordFreq <- d.wordFreq[order(-d.wordFreq$Freq),]
##d.wordFreq <- merge(d.wordFreq,voc)

print("Converting data to wide format...")
d.wide <- as.matrix(dcast(d,wordID~docID))
cat("Size is",dim(d.wide))

tm1 <- system.time({
  print("Performing NMF",)
  res <- nmf(d.wide,nb)
})

w <- basis(res)
h <- coef(res)