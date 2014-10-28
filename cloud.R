library(wordcloud)
library(RColorBrewer)

# function to get n biggest values from a list
getMaxN <- function(x, n=10) {
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  which(x > xp)
}

normalise <- function(x) {
  return((x-min(x)) / (max(x)-min(x)))
}

printCloud <- function(W, nb, voc, numWords = 50) {
  topwords.id <- matrix(nrow=numWords, ncol=nb);
  topwords.freq <- matrix(nrow=numWords, ncol=nb);
  for (i in 1:nb) {
    topwords.id[, i] <- unlist(getMaxN(W[, i], numWords))
    topwords.freq[, i] <- W[topwords.id[, i], i]
  }

  pal <- brewer.pal(9, "PuBuGn")
  pal <- pal[-(1:5)]
  par(mfrow=c(ceiling(sqrt(nb)),ceiling(sqrt(nb))))
  for (i in 1:nb) {
    wordcloud(voc[topwords.id[, i]], normalise(topwords.freq[, i]),
              scale=c(3,.3), min.freq=0, max.words=numWords, random.order=T,
              rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
  }
}

compCloud <- function(W, nb, voc, numWords = 50) {
  topwords.id <- matrix(nrow=numWords, ncol=nb);
  topwords.freq <- matrix(nrow=numWords, ncol=nb);
  for (i in 1:nb) {
    topwords.id[, i] <- unlist(getMaxN(W[, i], numWords))
    topwords.freq[, i] <- W[topwords.id[, i], i]
  }

  pal <- brewer.pal(9, "PuBuGn")
  pal <- pal[-(1:5)]
  par(mfrow=c(ceiling(sqrt(nb)),ceiling(sqrt(nb))))
  for (i in 1:nb) {
    wordcloud(voc[topwords.id[, i]], normalise(topwords.freq[, i]),
              scale=c(3,.3), min.freq=0, max.words=numWords, random.order=T,
              rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
  }
}