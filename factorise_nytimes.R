library("NMF")
library("reshape2")
library("Matrix")

rm(list = ls())

datLoc <- "/Users/Ash/Documents/database/bagofwords/docword.nytimes.txt"
vocLoc <- "/Users/Ash/Documents/database/bagofwords/vocab.nytimes.txt"

nb <- 4 # How many groups to form

print("Loading data...")
lib.n <- read.csv(datLoc, skip=0, nrows=1, header=F)
voc.n <- read.csv(datLoc, skip=1, nrows=1, header=F)
word.n <- read.csv(datLoc, skip=2, nrows=1, header=F)
d <- read.csv(datLoc,sep=" ",skip=3, nrows=100000, header=F)
colnames(d) <- c("docID", "wordID", "count")
voc <- read.csv(vocLoc, sep=" ", header=F)
colnames(voc) <- c("word")
voc$wordID <- 1:nrow(voc)

print("Converting data to wide format...")
d.wide <- as.matrix(dcast(d, wordID~docID))
d.wide[is.na(d.wide)] <- 0
cat("Size is", dim(d.wide))

tm1 <- system.time({
  print("Performing NMF",)
  res <- nmf(d.wide, nb, .options='v3')
})

W <- basis(res)
H <- coef(res)