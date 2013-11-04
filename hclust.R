library(tm)
library(RWeka)
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
ngrams <- RWeka::NGramTokenizer(corpus, Weka_control(min=1, max=4))
dtm <- DocumentTermMatrix(corpus, control = list(ngrams, wordLengths=c(3, 25), weighting = weightTfIdf, stopwords=TRUE))
dtm <- removeSparseTerms(dtm, .95)
plot(hclust(dist(dtm), method="complete"), xlab="text from corpus", "ylab"="distance", main="Cluster Dendrogram of Various Texts")
dtm$dimnames
