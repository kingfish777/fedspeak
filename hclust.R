library(tm)
library(RWeka)
library(ape)
library(RTextTools)
home <- "/home/hinckley"
homePath = paste(home, "/Public/corpora/FEDSpeak2", sep="")
setwd(paste(homePath, sep=""))
text <- system.file("texts", "txt", package="tm");
corpus <- Corpus(DirSource())
corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument, language = "english")
#ngrams <- RWeka::NGramTokenizer(corpus, Weka_control(min=1, max=4))
#dtm <- DocumentTermMatrix(corpus, control = list(ngrams, wordLengths=c(3, 25), weighting = weightTfIdf, stopwords=TRUE))
dtm <- create_matrix(cbind(as.vector(corpus)), language="english", minDocFreq=1, maxDocFreq=Inf, 
              minWordLength=3, maxWordLength=Inf, ngramLength=3, originalMatrix=NULL, 
              removeNumbers=FALSE, removePunctuation=TRUE, removeSparseTerms=0, 
              removeStopwords=TRUE,  stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, 
              weighting=weightTf)
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals> 0]           #remove all docs without words
dtm <- removeSparseTerms(dtm, .95)
plot(hclust(dist(dtm), method="complete"), xlab="text from corpus", "ylab"="distance", main="Cluster Dendrogram of Various Texts")
op = par(bg="#DDE3CA")
plot(dtm_complete, col="#487AA1", col.main="#45ADA8", col.lab="#7C8071",
     col.axis="#F38630", lwd=1, lty=1, sub='', hang=-1, axes=FALSE,
     main = "Cluster Dendrogram Representing Author Similarity", 
     xlab="Author/Publication", ylab = "Stylistic Distance")
par(op)
nplot(dtm_complete, hang=1, axes = TRUE, ann=TRUE, main = "Cluster Dendrogram Representing Author Similarity", 
      xlab="Author/Publication", ylab = "Distance")
phyl <- as.phylo(hclust(dtm_distro))
plot(phyl, edge.col=c("blue", "green", "red")[c(TRUE, FALSE) + 1 + (phyl$edge.length > 20)])


