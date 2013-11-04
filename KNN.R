#########################################################################
# Copyright (c) 2012 All Rights Reserved, http://www.bostondecision.com
#
# This source is free to use and distribute so long as credit is provided.
# This code is provided "AS IS" without warranty of any kind, either
# expressed or implied, including but not limited to the implied
# warranties of merchantability and/or fitness for a particular purpose.
#
# Author: Timothy D'Auria
# Email: tdauria [at] bostondecision [dot] com
# Date: 5/14/2012
#
# TITLE: THE R TEXT CLASSIFIER - Brand Management, Plagiarism Detection
#
# Purpose: R tools to predict the author of a text document or url
# Uses a simple K-nearest Neighbor algorithm.
#
# Setup: Set pathname to a directory that contains 1 subdirectory
# for each candidate/author.  Each candidate subdirectory contains 1 or
# more speeches saved as .txt files.
# Set candidates to the names of the candidate subdirectories
#
#########################################################################

# Load libraries
library(tm)
library(wordcloud)
library(kernlab)
library(plyr)
library(class)
library(Snowball)
library(RWeka)

# Set options
options(stringsAsFactors = FALSE)

#BiGramTokenize
#myTokenizer <- function(x) RWeka::NGramTokenizer(x, Weka_control(min = 3, max = 3))


# Set parameters
#candidates <- c("romney", "obama")
#candidates <- c("Simon_", "Levenda")
#pathname <- "C:/Users/tdauria/Google Drive/meetups/05/speeches"
#pathname <- "/home/hinckley/levenda/"

#candidates <- c("applebaum", "coulter")
#candidates <- c("hedges", "baker", "Plato", "kudlow", "coulter", "myron_magnet", "Castro", "Greenspan", "applebaum",  "j_stossel", "Simon_", "Levenda")
candidates <- c("hedges", "baker", "Plato", "kudlow", "coulter", "eccles", "myron_magnet", "Castro_1963", "greenspanD", "applebaum",  "j_stossel", "Simon_", "Levenda")

#candidates <- c("volcker/1984/", "bernanke/2008/", "eccles/1935/", "greenspanD")

#candidates <- c("Greenspan", "Castro")
#candidates <- c("greenspanD", "Castro_1963")
#candidates <- c("Simon_", "Levenda")

pathname <- "/home/hinckley/Public/corpora/"

# Function to convert pretty apostrophe
convertPrettyApostrophe <- function(x) gsub("’", "'", x)

# Function to generate corpus where each paragraph is
# set as a document.
generateParagraphDocCorpus <- function(cand, path) {
  
  # Set directory and list files
  s.dir <- sprintf("%s/%s", path, cand)
  filelist <- list.files(s.dir, full.names = TRUE)
  
  # Read each paragraph and append to vector
  speech.v <- unlist(sapply(filelist, function(x) {
    speech.tmp <- readLines(x)
    speech.tmp <- speech.tmp[speech.tmp != ""]
    return(speech.tmp)
  }))
  
  # Instantiate Corpus
  s.cor <- Corpus(VectorSource(speech.v, encoding = "ANSI"))
  
  return(s.cor)
}


# Function to clean Corpus text
cleanCorpus <- function(corpus) {
  
  # Apply Text Mining Clean-up Functions
  corpus.tmp <- tm_map(corpus, convertPrettyApostrophe)
  corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, tolower)
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
  corpus.tmp <- tm_map(corpus.tmp, stemDocument, language = "english")
  
  return(corpus.tmp)
}


# Function to generate corpus from a single file
generateSpeechDocCorpus <- function(filepath) {
  
  # Read data from file
  vec <- scan(filepath, what = "", quiet = TRUE)
  
  # Collapse word vector
  vec <- paste(vec, collapse = " ")
  
  # Instantiate Corpus
  s.cor <- Corpus(VectorSource(vec, encoding = "ANSI"))
  
  return(s.cor)
}   

# Function to generate term document matrices
generateTDM <- function(cand, path) {
  # Set directory
  s.dir <- sprintf("%s/%s", path, cand)
  
  # Instantiate Corpus
  s.cor <- Corpus(DirSource(directory = s.dir, encoding = "ANSI"))
  #s.cor <- generateParagraphDocCorpus(cand, path)
  #s.cor <- generateSpeechDocCorpus(paste(pathname, "Simon","/", sep=""))
  #paste(pathname, "Simon", sep="")
  #s.cor
  # Clean corpus
  s.cor.cl <- cleanCorpus(s.cor)
  
  #ngrams <- RWeka::NGramTokenizer(s.cor, Weka_control(min=2, max=2))
  
  # Create term document matrix
  #s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(ngrams, weighting = weightTfIdf))
  s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(weighting = weightTfIdf))
  # s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(weighting = weightTfIdf))
  
  # Remove sparse terms
  s.tdm <- removeSparseTerms(s.tdm, 0.7) # 0.7
  
  # Construct return object
  result <- list(name = cand, tdm = s.tdm)
  
  return(result)
}

# Run term document matrix function on all candidates
tdm <- lapply(candidates, generateTDM, path = pathname)

# Bind Candidate Name to Term Document Matrices
bindCandidateToTDM <- function(tdm) {
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsfactors = FALSE)
  s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetcandidate"
  return(s.df)
}

# Append Candidate Field to TDM
candTDM <- lapply(tdm, bindCandidateToTDM)

# Rbind Candidate TDMs
tdm.stack <- do.call(rbind.fill, candTDM)
tdm.stack[is.na(tdm.stack)] <- 0

# Random sample 70% for training of data mining model; remainder for test
train.idx <- na.omit(sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * .70)))
test.idx <- na.omit((1:nrow(tdm.stack))[- train.idx])

# Extract candidate name
tdm.cand <- na.omit(tdm.stack[, "targetcandidate"])
tdm.stack.nl <- tdm.stack[,!colnames(tdm.stack) %in% "targetcandidate"]

#tdm.stack.nl
# K-nearest Neighbor
knn.pred <- knn(na.omit(tdm.stack.nl[train.idx, ]), tdm.stack.nl[test.idx, ], tdm.cand[train.idx])
knn.pred <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.cand[train.idx])

knn.train.data <- tdm.stack[train.idx, ]

# Confusion Matrix
conf.mat <- table("Predictions" = knn.pred, Actual = tdm.cand[test.idx])
conf.mat
# Accuracy
(accuracy <- sum(diag(conf.mat))/length(test.idx) * 100)

#findAssocs(tdm[[1]][[2]], 'greed', 0.01) #
#findAssocs(tdm[[2]][[2]], 'greed', 0.01)
#findAssocs(tdm[[3]][[2]], 'greed', 0.01)
#findAssocs(tdm[[4]][[2]], 'greed', 0.01) #

#findAssocs(tdm[[1]][[2]], 'risk', 0.01) #
#findAssocs(tdm[[2]][[2]], 'risk', 0.01)
#findAssocs(tdm[[3]][[2]], 'risk', 0.01)
#findAssocs(tdm[[4]][[2]], 'risk', 0.01) #

#findAssocs(tdm[[1]][[2]], 'excess', 0.01) #
#findAssocs(tdm[[2]][[2]], 'excess', 0.01)
#findAssocs(tdm[[3]][[2]], 'excess', 0.01)
#findAssocs(tdm[[4]][[2]], 'excess', 0.01) #

#findAssocs(tdm[[1]][[2]], 'fraud', 0.01) #
#findAssocs(tdm[[2]][[2]], 'fraud', 0.01)
#findAssocs(tdm[[3]][[2]], 'fraud', 0.01)
#findAssocs(tdm[[4]][[2]], 'fraud', 0.01) #

#tdm
