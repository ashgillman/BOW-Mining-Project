library(tm)
rm(list=ls())
source("mynmf.R")

##### Data Acquisition #####

# Load Data
child <- VCorpus(DirSource("data/ChildFiction/", encoding = "UTF-8", pattern="txt$"),
                 readerControl = list(language = "lat"))
nmf <- VCorpus(DirSource("data/NMF/", encoding = "UTF-8", pattern="txt$"),
               readerControl = list(language = "lat"))
photo <- VCorpus(DirSource("data/PhotoAssignment/", encoding = "UTF-8", pattern="txt$"),
                 readerControl = list(language = "lat"))
speech <- VCorpus(DirSource("data/Speech/", encoding = "UTF-8", pattern="txt$"),
                  readerControl = list(language = "lat"))

##### Data Preprocessing #####

# tag and combine data
meta(child, tag="type", type="corpus") <- "child"
meta(nmf, tag="type", type="corpus") <- "nmf"
meta(photo, tag="type", type="corpus") <- "photo"
meta(speech, tag="type", type="corpus") <- "speech"

# remove punctuation
child <- tm_map(child, removePunctuation, lazy=T)
nmf <- tm_map(nmf, removePunctuation, lazy=T)
photo <- tm_map(photo, removePunctuation, lazy=T)
speech <- tm_map(speech, removePunctuation, lazy=T)

# remove stop words
corpus <- tm_map(child, removeWords, stopwords("english"), lazy=T)
corpus <- tm_map(nmf, removeWords, stopwords("english"), lazy=T)
corpus <- tm_map(photo, removeWords, stopwords("english"), lazy=T)
corpus <- tm_map(speech, removeWords, stopwords("english"), lazy=T)

corpus <- c(child, nmf, photo, speech)

# document-term matrix
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, 0.95)

##### NMF Data Mining #####

nb <- 4

factTime <- system.time({
  print("Performing mynmf",)
  list[W, H] <- mynmf(as.matrix(tdm), nb, precision=10)
})

groupings <- form_groups(H)
