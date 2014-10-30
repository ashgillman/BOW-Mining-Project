library(tm)
library(corrgram)
rm(list=ls())

##### Data Aggregation #####

# Load Data
child <- VCorpus(DirSource("data/ChildFiction/", encoding = "UTF-8",
                           pattern="txt$"),
                 readerControl = list(language = "lat"))
nmf <- VCorpus(DirSource("data/NMF/", encoding = "UTF-8", pattern="txt$"),
               readerControl = list(language = "lat"))
kos <- VCorpus(DirSource("data/KOSdiary/", encoding = "UTF-8",
                           pattern="txt$"),
                 readerControl = list(language = "lat"))
speech <- VCorpus(DirSource("data/Speech/", encoding = "UTF-8",
                            pattern="txt$"),
                  readerControl = list(language = "lat"))

##### Data Preprocessing #####

# remove punctuation
child <- tm_map(child, removePunctuation, lazy=T)
nmf <- tm_map(nmf, removePunctuation, lazy=T)
kos <- tm_map(kos, removePunctuation, lazy=T)
speech <- tm_map(speech, removePunctuation, lazy=T)

# remove stop words
child <- tm_map(child, removeWords, stopwords("english"))
nmf <- tm_map(nmf, removeWords, stopwords("english"))
kos <- tm_map(kos, removeWords, stopwords("english"))
speech <- tm_map(speech, removeWords, stopwords("english"))

# combine and remember groupings
corpus <- c(child, nmf, kos, speech)
orig.groupings <- c(rep(1, length(child)),
                    rep(2, length(nmf)),
                    rep(3, length(kos)),
                    rep(4, length(speech)))
orig.groupingnames <- c(rep("child", length(child)),
                        rep("nmf", length(nmf)),
                        rep("kos", length(kos)),
                        rep("speech", length(speech)))

# create term-document matrix
tdm <- TermDocumentMatrix(corpus)

# remove sparse words (only appear in 1 doc)
tdm <- removeSparseTerms(tdm, 0.95)

source("mynmf.R")
source("matchClusters.R")
source("cloud.R")
doNmfClustering <- function(tdm, ignoreIfInMoreThan, sparsity, nb) {
  # remove common words (ignoreIfInMoreThan% most common)
  wordInDocCount <- ncol(tdm) - rowSums(as.matrix(tdm) == 0)
  tdm <- tdm[wordInDocCount < (ignoreIfInMoreThan * ncol(tdm)), ]

  ##### NMF Data Mining #####

  factTime <- system.time({
    #print("Performing mynmf",)
    print("Performing gd_cls_nmf",)
    #list[W, H] <- mynmf(as.matrix(tdm), nb, precision=10)
    list[W, H] <- gd_cls_nmf(as.matrix(tdm), nb, lambda=sparsity)
  })
  return(list(W, H, tdm))
}
doKmeansClustering <- function(tdm, ignoreIfInMoreThan, nb) {
  # remove common words (ignoreIfInMoreThan% most common)
  wordInDocCount <- ncol(tdm) - rowSums(as.matrix(tdm) == 0)
  tdm <- tdm[wordInDocCount < (ignoreIfInMoreThan * ncol(tdm)), ]

  ##### NMF Data Mining #####


  factTime <- system.time({
    print("Performing kmeans",)
    m <- as.matrix(weightTfIdf(tdm))
    ### don't forget to normalize the vectors so Euclidean makes sense
    norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
    m <- norm_eucl(m)
    cl <- kmeans(t(m), nb)
  })
  return(list(cl, tdm))
}

evaluatePerformance <- function(W, H, nb) {
  groupings <- form_groups(H)

  ##### Analyse data #####

  # form contingency table
  confusionMat <- matrix(0, nrow=length(unique(orig.groupingnames)), ncol=nb,
                         dimnames=list(unique(orig.groupingnames),
                                       as.character(1:nb)))
  colnames(W) <- as.character(1:nb)
  for (i in 1:length(groupings)) {
    confusionMat[orig.groupings[i], groupings[i]] <-
      confusionMat[orig.groupings[i], groupings[i]] + 1
  }

  # decide on cluster matches
  matching <- minWeightBipartiteMatching(groupings, orig.groupings)

  # reorder confusion matrix
  temp <- confusionMat
  for (i in matching) {
    confusionMat[, matching[i]] <- temp[, i]
    colnames(confusionMat)[matching[i]] <- as.character(i)
    colnames(W)[i] <- unique(orig.groupingnames)[i]
  }
  (confusionMat)
  TP <- diag(confusionMat)
  FP <- apply(confusionMat, 2, sum) - TP
  FN <- apply(confusionMat, 1, sum) - TP
  TN <- rep(sum(confusionMat), nb) - TP - FP - FN
  Sensitivity <- TP / (TP + FN)
  Specificity <- TN / (FP + TN)
  Precision <- TP / (TP + FP)
  Sensitivity.avg <- mean(Sensitivity)
  Specificity.avg <- mean(Specificity)
  Precision.avg <- mean(Precision)

  return(list(W, H, Sensitivity.avg, Specificity.avg, Precision.avg,
              confusionMat))
}

evaluateKmeansPerformance <- function(cl, nb) {
  groupings <- cl$cluster
  # form contingency table
  confusionMat <- matrix(0, nrow=length(unique(orig.groupingnames)), ncol=nb,
                         dimnames=list(unique(orig.groupingnames),
                                       as.character(1:nb)))
  colnames(W) <- as.character(1:nb)
  for (i in 1:length(groupings)) {
    confusionMat[orig.groupings[i], groupings[i]] <-
      confusionMat[orig.groupings[i], groupings[i]] + 1
  }

  # decide on cluster matches
  matching <- minWeightBipartiteMatching(groupings, orig.groupings)

  # reorder confusion matrix
  temp <- confusionMat
  for (i in matching) {
    confusionMat[, matching[i]] <- temp[, i]
    colnames(confusionMat)[matching[i]] <- as.character(i)
    colnames(W)[i] <- unique(orig.groupingnames)[i]
  }
  (confusionMat)
  TP <- diag(confusionMat)
  FP <- apply(confusionMat, 2, sum) - TP
  FN <- apply(confusionMat, 1, sum) - TP
  TN <- rep(sum(confusionMat), nb) - TP - FP - FN
  Sensitivity <- TP / (TP + FN)
  Specificity <- TN / (FP + TN)
  Precision <- TP / (TP + FP)
  Sensitivity.avg <- mean(Sensitivity)
  Specificity.avg <- mean(Specificity)
  Precision.avg <- mean(Precision)

  return(list(Sensitivity.avg, Specificity.avg, Precision.avg, confusionMat))
}

ignoreIfInMoreThans <- c(0.1, 0.2, 0.3, 0.5, 0.8, 0.9)
sparsitys <- c(0, 0.01, 0.05, 0.1, 0.5, 1)
noRuns <- 3
n <- (length(ignoreIfInMoreThans) * noRuns) * 2 * length(sparsitys)
results <- data.frame(run=numeric(n), ignore=numeric(n), sparsity=numeric(n),
                      err=numeric(n),
                      Sensitivity=numeric(n), Specificity=numeric(n),
                      Precision=numeric(n), Time=numeric(n))
confusionMats <- vector("list", n)
counter <- 1
for (ignoreIfInMoreThan in ignoreIfInMoreThans) {
  for (run in 1:noRuns) {
    for (sparsity in sparsitys) {
      ## NMF
      tm <- system.time({
        list[W, H, V] <- doNmfClustering(tdm, ignoreIfInMoreThan, sparsity, 4)
        list[W, H, Sensitivity, Specificity, Precision, confusionMat] <-
          evaluatePerformance(W, H, 4)
        sumsqerr <- sum((V - W %*% H) ^ 2)
      })
      results[counter,] <- list(run=run, ignore=ignoreIfInMoreThan,
                                sparsity=sparsity, err=sumsqerr,
                                Sensitivity=Sensitivity,
                                Specificity=Specificity,
                                Precision=Precision, Time=tm)
      confusionMats[[counter]] <- confusionMat
      pdf(paste0("res/sparse-", sparsity, "_ignore-",
                 ignoreIfInMoreThan, "_", run, ".pdf"))
      comparison.cloud(W, max.words=400)
      dev.off()

      groupings <- form_groups(H)

      pdf(paste0("res/sparse-", sparsity, "_ignore-",
                 ignoreIfInMoreThan, "_", run, "_full.pdf"),
          width=10, height=10)
      plot.new()
      text(x=1, y=0.55, paste(Docs(tdm)[groupings==1], collapse="\n"), adj=c(1,0))
      text(x=1, y=0.45, paste(Docs(tdm)[groupings==3], collapse="\n"), adj=c(1,1))
      text(x=0, y=0.55, paste(Docs(tdm)[groupings==2], collapse="\n"), adj=c(0,0))
      text(x=0, y=0.45, paste(Docs(tdm)[groupings==4], collapse="\n"), adj=c(0,1))
      dev.off()

      counter <- counter + 1
    }
    ## Kmeans
    tm <- system.time({
      list[cl, tdmNew] <- doKmeansClustering(tdm, ignoreIfInMoreThan, 4)
      list[Sensitivity, Specificity, Precision, confusionMat] <-
        evaluateKmeansPerformance(cl, 4)
    })
    results[counter,] <- list(run=run, ignore=ignoreIfInMoreThan,
                              sparsity=NA, err=cl$totss,
                              Sensitivity=Sensitivity,
                              Specificity=Specificity,
                              Precision=Precision, Time=tm)
    confusionMats[[counter]] <- confusionMat

    groupings <- cl$cluster
    pdf(paste0("res/km_ignore-",
               ignoreIfInMoreThan, "_", run, ".pdf"),
        width=10, height=10)
    plot.new()
    text(x=1, y=0.55, paste(Docs(tdm)[groupings==1], collapse="\n"), adj=c(1,0))
    text(x=1, y=0.45, paste(Docs(tdm)[groupings==3], collapse="\n"), adj=c(1,1))
    text(x=0, y=0.55, paste(Docs(tdm)[groupings==2], collapse="\n"), adj=c(0,0))
    text(x=0, y=0.45, paste(Docs(tdm)[groupings==4], collapse="\n"), adj=c(0,1))
    dev.off()

    counter <- counter + 1
  }
}

##### Analyse #####
library(reshape2)

results <- results[results$run != 0, ]
results$type <- ifelse(is.na(results$sparsity), "K means", "NMF")

source("myCorrgramPanels.R")
pdf("res/corr.pdf")
corrgram(results, lower.panel=panel.shadeConf, upper.panel=panel.ptsAlpha)
dev.off()
pdf("res/NMFcorr.pdf")
corrgram(results[!is.na(results$sparsity), ],
         lower.panel=panel.shadeConf, upper.panel=panel.ptsAlpha)
dev.off()
pdf("res/Kmeanscorr.pdf")
corrgram(results[is.na(results$sparsity),
                 -which(colnames(results) == "sparsity")],
         lower.panel=panel.shadeConf, upper.panel=panel.ptsAlpha)
dev.off()
(ggplot(results, aes(x=Sensitivity)) + stat_bin() + theme_bw())
(ggplot(results, aes(x=Specificity)) + stat_bin() + theme_bw())
(ggplot(results, aes(x=Precision)) + stat_bin() + theme_bw())

results.stack <- melt(results,
                      measure.vars=which(colnames(results) %in%
                                           c("Sensitivity", "Specificity",
                                             "Precision")))
names(results.stack)[which(names(results.stack) %in% c("variable", "value"))] <-
  c("Measure", "Result")
pdf("res/resultsBox.pdf", width=6, height=4)
(ggplot(results.stack, aes(x=Measure, y=Result)) +
   geom_boxplot() +
   facet_grid(~type) +
   theme_bw())
dev.off()