
library(tm)

reut21578 <- system.file("texts", "crude", package = "tm")

reuters <- Corpus(DirSource(reut21578),
                  readerControl = list(reader = readReut21578XML))

### download reuters21578 data first (use first 1000 documents; 1984/85)
file <- "reut2-000.xml"
reuters <- Corpus(ReutersSource(file), readerControl = list(reader = readReut21578XML))

## Convert to Plain Text Documents
reuters <- tm_map(reuters, as.PlainTextDocument)
reuters[[1]]

## Convert to Lower Case
reuters <- tm_map(reuters, tolower)
reuters[[1]]

## Remove Stopwords
reuters <- tm_map(reuters, removeWords, stopwords("english"))
reuters[[1]]

## Remove Punctuations
reuters <- tm_map(reuters, removePunctuation)
reuters[[1]]

## Stemming
reuters <- tm_map(reuters, stemDocument)
reuters[[1]]

## Remove Numbers
reuters <- tm_map(reuters, removeNumbers)
reuters[[1]]


## Eliminating Extra White Spaces
reuters <- tm_map(reuters, stripWhitespace)
reuters[[1]]

## create a term document matrix
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[1:10, 5001:5010])

findFreqTerms(dtm, 100)
findAssocs(dtm, "washington", .4)
#washington  secretari  political     reagan republican      white      regan
#      1.00       0.49       0.46       0.45       0.45       0.42       0.41
#staff strategist
#0.41       0.41



## do tfxidf
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:10, 5001:5010])

## do document clustering

### k-means (this uses euclidean distance)
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)

### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)


### cluster into 10 clusters
cl <- kmeans(m_norm, 10)
cl

table(cl$cluster)

### show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cl)

findFreqTerms(dtm[cl$cluster==1], 50)
inspect(reuters[which(cl$cluster==1)])

## hierarchical clustering
library(proxy)

### this is going to take 4-ever (O(n^2))
d <- dist(m, method="cosine")
hc <- hclust(d, method="average")
plot(hc)

cl <- cutree(hc, 50)
table(cl)
findFreqTerms(dtm[cl==1], 50)
