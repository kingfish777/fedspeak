#########################################################################
# Copyright (c) 2014 All Rights Reserved, Scott Alexander Malec
#
# This source is free to use and distribute so long as credit is provided.
# This code is provided "AS IS" without warranty of any kind, either
# expressed or implied, including but not limited to the implied
# warranties of merchantability and/or fitness for a particular purpose.
#
# Author: Scott Alexander Malec
# Email: scott [dot] malec [at] gmail [dot] com
# Date: 1/23/2014
#
# TITLE: greenspanMOVEE.R
#
# Purpose: R tools to animate GraphViz of connection between popular n-grams
# across time for a colleciton of texts
#
#########################################################################

library("calibrate")
library("tm")
#library("lsa")
#library("topicmodels")
#library("lda")
#library("gsubfn")
#library("ggplot2")
#library("reshape2")
#library("ape")
#library("rgl")
library("rJava")
library("RWeka")
library("RWekajars")
library("Rgraphviz")
library("MASS")
library("slam")
#library("igraph")

years = list(40)
years = sequence(20)+1987
#year = 1985
param = list(10)
interestingList = list(1000)
interestingList = list("money", "capital", "oil", "violence", "imperi", "ameri", "cuba")
interestingList = list("money", "capita", "cia", "missiles", "workers", "fbi", "freedom", "union", "batista", "conspir")
#interestingList = list("assassin", "fbi", "cia", "conspiracy", "plot")
interestingList = list("john kennedy", "richard nixon", "president")
#, "washington", "provocation", "imperialist policy", "fascism")
#interestingList = list("imperialist", "fascist", "exploiter", "democracy", "classless society", "antifascist", "zionist", "counterrevolutionary")
#interestingList = list("cuban", "bulgarian", "chillean", "israeli", "america")
#interestingList = list("television", "radio", "propaganda", "computer", "technology", "information")
#interestingList = list("direct action", "terrorism", "assassinatoin", "plot", "conspiracy", "counterrevolutionary", "imperialism")
interestingList = list("genocide", "guatemala", "torture", "mass graves", "sandanista", "contras")
interestingList = list("richard nixon", "nixon", "vietnam", "vietnamese", "kissinger")
interestingList = list("surveillance", "propaganda", "espionage", "spies", "spy", "provocation")
#interestingList = list("press freedom", "physician", "health")
interestingList = list("richard nixon", "money", "oil", "wealth", "privilege", "lies", "prevarication",
                       "cia", "dollar", "sugar", "racism", "conspiracy", "counterrevolutionaries", "freedom", "propaganda", "vietnam", "torture", "fascist", "democracy", "exploiter")
#interestingList = list("cia", "fascism", "america", "castro", "imperialist")
#interestingList = list("market", "free market", "derivative", "bubble", "fraud", "regulator", "bureaucracy", "innovation", "sec", "depression", "recession", "minority", "poverty", "wealthy", "glass","stupid", "liberals")
#interestingList = list("fraud", "war", "president", "spending", "taxes",
                       "keynes", "friedman", "baby boom", "entitlement", "innovation", "bank regulators", "glass steagall", "steagall act", "bubble", "protectionist", "labor")
#interestingList = list("rent seeking", "fascism", "liquidity", "baby boomers", "market crash", "macroeconomics", "globalization", "tax revenues", "entitlement", "labor costs", "crisis", "downsizing process", "fdic", "corporate personhood", "minorities", "civilization")
#interestingList = list("government intervention")
#interestingList = list("actually")
#interestingList = list("nixon", "economy", "electronic money", "money", "money supply", "chicago school")
#interestingList = list("postwar era", "chicago school")
#interestingList = list("froth") #2005
#interestingList = list("congress", "military", "cia")
#interestingList = list("efficient market")
#interestingList = list("bail")

interestingList = list("enron", "riskier", "hightech", "energi price", "asset pric", "enro", "enr", "freedom", 
                       "longterm capita", "longterm capit", "labor cost", "synergi", "domest demand",
                       "credit default", "asset", "new rule", "bank manag", "dramat shift",
                       "riskmanagement", "middle east", "grow popul", "riskless asset", "ten year", 
                       "offshore", "power", "outsource", "risk", "credit derivati", "crossbord trade", 
                       "deriv", "hedge funds", "banking system", "collateral", "glass", "clear hous", "franchis",
                       "adequ capit", "firewal", "firewall", "bank system", "equiti", "great depress", "regim", 
                       "leverage", "default swap", "banking system", "excess risk", "household", "tax spend",
                       "supply disrupt", "mani observ", "deriv market", "counterparti", "foreign financi",
                       "public sector", "capit requir", "interbank", "libor", "safeti net", "market disciplin",
                       "address issu", "competit environ", "food energi price", "enemploy insur", "burden bank", 
                       "creativ destruct", "bank failur", 
                       "glasssteagal", "glasssteagal act", "lower risk", "bank diversifi", "futur option", "option futur",
                       "futur", "moral hazard", "invest bank", "concern must", "benefit risk", "instabl bank", 
                       #"prohibit", "panic", "risk appetit", "asset portfolio", "higher risk", "riskbas",
                       "riskbas capit system", "insur depositori", "subsidiari", "creditworthi", "technolog",
                       "reassur", "equiti market", "volatil price", "key role", "can tailor", "settlement system",
                       "fraud", "deception", "efficient", "contribut high", "market hypoth", "bank run",
                       "nonbank", "secur power", "effic market", "new power", "expand", "undermin", "organ",
                       "technolog chang", "arbitrag", "advanc technolog", "cftc", "financi innov")

#interestingList = list("free trad", "bank")

length(unlist(interestingList))


#unlist(interestingList)[3]
aggregateResults = list(1000)


aggregateResults = list(10000)
assocResults = list(10000)
#DTM_agg = list(100)

home = "/home/kingfish"
homePath = paste(home, "/gReenspanCorpus/", sep="")
homePath = "/home/kingfish/gReenspanCorpus"
i = 1

image_index = 1

zero_pad_index <- function(image_Index) {
  if (image_Index >= 100 && image_Index < 1000)
    padded <- paste("0", as.character(image_Index), sep="")
  else if (image_Index >= 10 && image_Index <= 99) 
    padded <- paste("00", as.character(image_Index), sep="")
  else if (image_Index >= 0 && image_Index < 10)
    padded <- paste("000", as.character(image_Index), sep="")
  else as.character(image_Index)
}


plotDefaultYearByYear <- function(years)
{
  home = "/home/kingfish"
  homePath = paste(home, "/gReenspanCorpus/", sep="")
  yearStr = as.character(years[i])
  print("##########################")
  print(yearStr)
  print("##########################")
  wd = paste(homePath, yearStr, sep="")
  setwd(wd)
  setwd(paste(homePath, yearStr, sep=""))
  text <- system.file("texts", "txt", package="tm");
  corpus <- Corpus(DirSource('.'))
  corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  yourTokenizer <- function(x) RWeka::NGramTokenizer(x, Weka_control(min=1, max=3))
  tdm <- TermDocumentMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), tokenize=yourTokenizer, stopwords = TRUE))
  tdm <- removeSparseTerms(tdm, .98)
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), tokenize=yourTokenizer, stopwords = TRUE))
  dtm <- removeSparseTerms(dtm, .98)
  print("##### we now have a tdm")
  interesting_word_list_size <- length(unlist(interestingList))
  #word_index <- 1
  print("#################################")
  print("###### interesting word loop")
  print("#################################")
  for (word_index in 1:interesting_word_list_size) {
    interestingWord = ""
    interestingWord = unlist(interestingList)[word_index]
    print("##########")
    associations = list()
    try(associations <- names(findAssocs( tdm, unlist(interestingList)[word_index], 0.5)))
    try(a <- associations)
    #try(avals <- findAssocs(tdm, unlist(interestingList)[word_index], .5))
    try
    if (class(unlist(names(associations)[word_index])) != "try-error" || unlist(names(associations)[word_index] != 'NULL') ) {
      if (class(associations) != "try-error" || associations != 'NULL') {
        association_values <- associations
        association_size <- length(unlist(associations))
        print("#################################")
        print("###### association loop")
        print("#################################")
        # association_index <- 1
        t <- list()
        for (association_index in 1:association_size) {
          # try(print(paste(unlist(interestingList)[word_index], " ---> ", unlist(associations)[association_index], sep="")))
          if (class(unlist(interestingList)[word_index]) != "try-error")
          { #t=unlist(a)[word_index]
            #print(terms)
            #print(associations)
            try(t<-list(unlist(t), unlist(a)[association_index]))
            if (class(t) != "try-error")
            {
              #print(avals)
              ew <- associations
              if (length(unlist(t)) == 15) {
                #print(t)
                print(association_values[1:7])
                t_time <- unlist(t)[1:7]
                plot_title = paste("gReenspanCoRpus [St. Louis Fed/FRASER] - ", yearStr, "term: <", interestingWord, ">")
                #plot(dtm, terms<-t_time, corThreshold=.8, main=plot_title)
                #print(ew)
                setwd("/home/kingfish/gReenspanCorpus/MOVEE")
                png(paste("GREENSPAN_", zero_pad_index(image_index),".png", sep=""), width=1024, height=768);
                image_index = image_index+1
                
                plot(dtm, corThreshold = 0.75, terms = t_time,
                     attrs=list(node=list(shape="ellipse",
                                          fixedsize=FALSE,
                                          label="courier",
                                          fillcolor="red"),
                                edge=list(color="black",
                                          penwidth=1.5), #association_values[1:8])
                                graph=list(rankdir="TB")),
                     main=plot_title)
                dev.off()
                #Sys.sleep(time=1)
                #pause()
              }
            }
            #class(t)
            #plot(dtm, terms = t, legend=yearStr)
          }
          
        }
        # association_index <- association_index + 1
      }
      # word_index <- word_index + 1
    }
  }
}

lapply(years, plotDefaultYearByYear)
