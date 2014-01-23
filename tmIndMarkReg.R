
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
# TITLE: tmIndMarkReg.R
#
# Purpose: convert collection of texts, coded for date of publication, into time zeries object
#
#########################################################################

Sys.setenv(NOAWT = TRUE)
library(Snowball)
library(SnowballC)
library(tm)
library(lsa)
library(topicmodels)
library(lda)
library(RWeka)

home <- "/home/kingfish"
#home <- "/home/hinckley"
homePath = paste(home, "/greenspanC", sep="")

#homePath = paste(home, "/eccles2", sep="")
setwd(paste(homePath, sep=""))
text <- system.file("texts", "txt", package="tm");
corpus <- Corpus(DirSource())
#corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
print("removing stopwords")
tail(corpus[[5]])
corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
tail(corpus[[5]])
print("removing punctuation")
corpus <- tm_map(corpus, removePunctuation)
print("removing numbers")
corpus <- tm_map(corpus, removeNumbers)
print("making copy of corpus to complete stems from common original forms")
corpus_orig <- tm_map(corpus, tolower)
#dtm_corpus_orig <- DocumentTermMatrix(corpus_orig)
#vocab <- dtm_corpus_orig$dimnames
print("putting in lower case")
corpus <- tm_map(corpus, tolower)
print("stripping whitespace")
corpus <- tm_map(corpus, stripWhitespace)
print("stemming")
#corpus <- tm_map(corpus, SnowballStemmer, language="english")

corpus <- tm_map(corpus, stemDocument, language = "english")

#print("completing stems")
#corpus <- tm_map(corpus, stemCompletion, dictionary=vocab)

#BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, Weka_control(min = 1, max = 2))

ngrams <- RWeka::NGramTokenizer(corpus, Weka_control(min=2, max=2))
ngrams
# get all 1-gram and 2-gram word counts
#tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
#corpus <- tm_map(corpus, lazy=TRUE, stemCompletion)

#dtm <- DocumentTermMatrix(corpus)
#BiGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm <- DocumentTermMatrix(corpus, control = list( ngrams, weighting = weightTf))
dtm$dimnames
dtm <- removeSparseTerms(dtm, .99)
dtm$dimnames

library(stringr)


# script to obtain more prompting terms for MOVEE
i = 0L
ngramLength <- length(dtm$dimnames[[2]])
for (i in 1:ngramLength) { 
  i <- i + 1L
  try(  if (stringr::str_detect(string = dtm$dimnames[[2]][[i]], pattern = "market")) #glasssteagal
  {     
    word <- dtm$dimnames[[2]][[as.numeric(i)]] 
    print("###################################")
    print(word)
    print("###################################")
    words <- findAssocs(dtm, word, .1)
    print(words)
  }  
  )  
}

# retir definedbenefit unfund babyboom unforeseen unsustain disincent deficitreduct discretionari beneficiari
# glasssteagal
# conundrum shortcircuit bottleneck slack subportfolio underwrit markmarket reacceler rosenberg 
#boom 

#create topic model using VEM, Gibbs sampling, fixed VEM
print("setting topic # or K, and seed of random gen")
K <- 10
SEED <- 167
print("LDA ifying the DTM")

print("performing VEM, Gibbs, VEM_fixed on DTM")
#This part can take a while, depending on how many documents you have
greenspan_TM <-
  list(VEM = LDA(dtm, k = K, control = list(seed = SEED)), #note that DTM is required to be weighted with weightTf, term frequency  
       Gibbs = LDA(dtm, k = K,
                   control = list(seed = SEED)),
       VEM_fixed = LDA(dtm, k = K,
                       control = list(estimate.alpha = TRUE, seed = SEED))) 
#print("performing Gibbs sampling")
#greenspan_TM <-      
#  list(Gibbs = LDA(dtm, k = K,
#                   control = list(seed = SEED)))
print("processing Topic Model")
sapply(greenspan_TM[1:2], slot, "alpha")
sapply(greenspan_TM, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))))
greenspan_TM$Gibbs

print("topics for corpus")
Topic <- topics(greenspan_TM[["Gibbs"]], 1)
print("terms for corpus")
Terms <- terms(greenspan_TM[["Gibbs"]], 20) #I sometimes do 10, depending on size of corpus
                                            

#lda <- LDA(dtm, control = list(alpha = 0.1), K)
Terms[,1] #view terms to see how "clean" the topics are, adjust K, seed, and other params as needed to obtain a clean topic set
Terms[,2]
Terms
gammaDF <- as.data.frame(greenspan_TM$Gibbs@gamma) #key step for gathering topic proportions!

#######################
#set topic names from common terms






#########################
# build time series for each topic in topic model from Greenspan corpus and write to CSV

gts_home_path <- "/home/kingfish/gts/"
#ets_home_path <- "/home/kingfish/ets/"
setwd(ets_home_path)

doc_num <- 0L
topicMatrix = matrix(nrow=1, ncol=2)
for (topic in 1:K) {
  topicName <- NULL
  filename <- NULL
  topicName <- "eccles_"
  for (i in 1:length(Terms[,topic])) {
    topicName <- paste(topicName, paste(Terms[,topic][[i]], collapse="", sep="_"), sep="")}
  
  for (doc in 1:dtm$nrow) 
  { 
    filename = paste(ets_home_path, topicName, ".csv")
    topicScore = gammaDF[[topic]][[doc]]
    print(topicScore)
    print(dtm$dimnames[[1]][[doc]])
    timeStr = substr(dtm$dimnames[[1]][[doc]], 11, 18)
    print(timeStr)
    yearStr = substr(timeStr, 0, 4)
    monthStr = substr(timeStr, 5, 6)
    dayStr = substr(timeStr, 7, 8)
    newTimeStr = paste(yearStr, monthStr, dayStr, sep="-")
    date <- as.Date(newTimeStr, format='%yyyy%mm%dd')
    print(paste(yearStr, monthStr, dayStr, sep="-"))
    print(doc)
    print(topicName)
    print(topic)
    newLine = paste(newTimeStr, topicScore, sep=",") 
    print(newLine)
    topicMatrix$topicName <-  cbind(newTimeStr, topicScore)
    write.table(newLine, file = filename, append = TRUE, sep = ",",
                row.names=FALSE, col.names=FALSE, quote=FALSE)
    print(doc_num <- doc_num + 1L)
  } 
  write.csv(topicMatrix$topicName, file = filename)
}

###########################
# same step as above, but lazily and hastily created for another corpus


ets_home_path <- "/home/kingfish/ets/"
setwd(ets_home_path)

doc_num <- 0L
topicMatrix = matrix(nrow=1, ncol=2)
for (topic in 1:K) {
  topicName <- NULL
  filename <- NULL
  for (i in 1:length(Terms[,topic])) {
    topicName <- paste(topicName, paste(Terms[,topic][[i]], collapse="", sep="_"), sep="")}
  
  for (doc in 1:dtm$nrow) 
  { 
    filename = paste(ets_home_path, topicName, ".csv")
    topicScore = gammaDF[[topic]][[doc]]
    print(topicScore)
    print(dtm$dimnames[[1]][[doc]])
    timeStr = substr(dtm$dimnames[[1]][[doc]], 8, 15)
    print(timeStr)
    yearStr = substr(timeStr, 0, 4)
    monthStr = substr(timeStr, 5, 6)
    dayStr = substr(timeStr, 7, 8)
    newTimeStr = paste(yearStr, monthStr, dayStr, sep="-")
    date <- as.Date(newTimeStr, format='%yyyy%mm%dd')
    print(paste(yearStr, monthStr, dayStr, sep="-"))
    print(doc)
    print(topicName)
    print(topic)
    newLine = paste(newTimeStr, topicScore, sep=",") 
    print(newLine)
    topicMatrix$topicName <-  cbind(newTimeStr, topicScore)
    write.table(newLine, file = filename, append = TRUE, sep = ",",
                row.names=FALSE, col.names=FALSE, quote=FALSE)
    print(doc_num <- doc_num + 1L)
  } 
  #write.csv(topicMatrix$topicName, file = filename)
}




corpus[[2]]
###########################
#build label for specific topic model from common terms in each model
#
#

library(zoo)
library(sqldf)


#################
# Read topic proportion time series from CSVs and plot as time series
#################

topic1_ets <- read.csv("/home/kingfish/ets/war_econom_nation_income.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x1 <- topic1_ets[2][1]
dt1 <- topic1_ets[1][,1]
ts1 <- zoo(x1, dt1)
ts1
plot.ts(ts1)
plot(ts1)

topic2_ets <- read.csv("/home/kingfish/ets/govern_credit_money.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x2 <- topic2_ets[2][1]
dt2 <- topic2_ets[1][,1]
ts2 <- zoo(x2, dt2)
ts2
plot.ts(ts2)
plot(ts2)

#########################################




library(zoo)
library(sqldf)

topic1_ts <- read.csv("/home/kingfish/gts/topic_1_bank_insurance.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x1 <- topic1_ts[2][1]
dt1 <- topic1_ts[1][,1]
ts1 <- zoo(x1, dt1)
ts1
plot.ts(ts1)

topic2_ts <- read.csv("/home/kingfish/gts/topic_2_bank_risk.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x2 <- topic2_ts[2][1]
dt2 <- topic1_ts[1][,1]
ts2 <- zoo(x2, dt2)
ts2
plot.ts(ts2)


topic3_ts <- read.csv("/home/kingfish/gts/topic_3_budget_will_save.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x3 <- topic3_ts[2][1]
dt3 <- topic3_ts[1][,1]
ts3 <- zoo(x3, dt3)
ts2
plot.ts(ts3)

topic4_ts <- read.csv("/home/kingfish/gts/topic_4_credit_market_risk.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x4 <- topic4_ts[2][1]
dt4 <- topic4_ts[1][,1]
ts4 <- zoo(x4, dt4)
ts4
plot.ts(ts4)

topic5_ts <- read.csv("/home/kingfish/gts/topic_5_financial_system.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x5 <- topic5_ts[2][1]
dt5 <- topic5_ts[1][,1]
ts5 <- zoo(x5, dt5)
ts5
plot.ts(ts5)

topic6_ts <- read.csv("/home/kingfish/gts/topic_6_product_increase.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x6 <- topic6_ts[2][1]
dt6 <- topic6_ts[1][,1]
ts6 <- zoo(x6, dt6)
ts6
plot.ts(ts6)


topic7_ts <- read.csv("/home/kingfish/gts/topic_7_market_future_price.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x7 <- topic7_ts[2][1]
dt7 <- topic7_ts[1][,1]
ts7 <- zoo(x7, dt7)
ts7
plot.ts(ts7)


topic8_ts <- read.csv("/home/kingfish/gts/topic_8_measure_value_price.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x8 <- topic8_ts[2][1]
dt8 <- topic8_ts[1][,1]
ts8 <- zoo(x8, dt8)
ts8
plot.ts(ts8)


topic9_ts <- read.csv("/home/kingfish/gts/topic_9_policy_rate_monetary.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x9 <- topic9_ts[2][1]
dt9 <- topic9_ts[1][,1]
ts9 <- zoo(x9, dt9)
ts9
plot.ts(ts9)

topic10_ts <- read.csv("/home/kingfish/gts/topic_10_price_oil_year_product.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x10 <- topic10_ts[2][1]
dt10 <- topic10_ts[1][,1]
ts10 <- zoo(x10, dt10)
ts10
plot.ts(ts10)


topic11_ts <- read.csv("/home/kingfish/gts/topic_11_product_technology_economy.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x11 <- topic11_ts[2][1]
dt11 <- topic11_ts[1][,1]
ts11 <- zoo(x11, dt11)
ts11
plot.ts(ts11)
plot(ts11)


topic12_ts <- read.csv("/home/kingfish/gts/topic_12_rate_internal.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x12 <- topic12_ts[2][1]
dt12 <- topic12_ts[1][,1]
ts12 <- zoo(x12, dt12)
ts12
plot.ts(ts12)
plot(ts12)
##################3
#
# turn DJIA into a time series and plot

djia_ts <- read.csv("/home/kingfish/DJIA.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
# http://research.stlouisfed.org/fred2/series/DJIA/downloaddata
# weekly, ending Friday
# 1987-01-01 to 2006-12-31
#text, comma separated
x <- djia_ts[2][1]
dt <- djia_ts[1][,1]
ts <- zoo(x, dt)
ts
plot.ts(ts)





summary(ts1)
tcf(ts1, ts9)
ccf(djia_ts, topic9_ts)

ccf(topic9_ts, topic1_ts)

diff(ts1, ts2)

library(stringr)

###############
# IGNORE THIS
###############
i = 0L
ngramLength <- length(dtm$dimnames[[2]])
for (i in 1:ngramLength) { 
  i <- i + 1L
  try(  if (stringr::str_detect(string = dtm$dimnames[[2]][[i]], pattern = "drivingfinancialinnov"))
  {     
    word <- dtm$dimnames[[2]][[as.numeric(i)]] 
    print("###################################")
    print(word)
    print("###################################")
    words <- findAssocs(dtm, word, .4)
    print(words)
  } 
  )  
}





library(zoo)
library(sqldf)
library(xts)
#print("printing terms from topic model")
# Terms


#emptyTS <- xts(0[seq(1:6642)],seq(from=as.Date('1987-09-09'), to=as.Date('2005-11-14'), by=1))


##########################
##########################
# padding a time series (one way to do it)
###########################
###########################


##########################
##########################
## data.table and sqldf goodness
##########################
##########################
library(xts)
emptyTS <- xts(,seq(from=as.Date('1987-09-09'), to=as.Date('2005-11-14'), by=1))

summary(emptyTS)
emptyTS


tbill_ts <- read.csv("/var/www/GreenspanData/ten_year_t_bill_data_raw.csv", sep = ",", header=FALSE, stringsAsFactor=FALSE)
tbill_ts$V1
tbill_x <- tbill_ts[2][1]
tbill_x
tbill_dt <- tbill_ts$V1 #as.Date(tbill_ts[1][,1], format= "YYYY-MM-DD")
tbill_dt <- as.Date(tbill_ts$V1, format = "YYYY-MM-DD")
    #### problem converting from character to date  ######PROBLEM_PROBLEM_PROBLEM
tbill_dt
class(tbill_dt)
tbill_zoo <- zoo(tbill_x, tbill_dt)
plot(tbill_zoo)
tbill_zoo
#####         na.locf(merge ...) is very handing for padding time zeries
tbill_newt <-  na.locf(merge(tbill_zoo, emptyTS, all=TRUE))
tbill_newt
plot(tbill_newt)

length(tbill_newt)

plot(tbill_zoo)
summary(tbill_zoo)

length(tbill_zoo)


djia_ts <- read.csv("/home/hinckley/DJIA-percent-change-avg-Friday.csv", sep = ",", header=TRUE, stringsAsFactor=FALSE)


#### MAJOR improvement with this version of --------use header=FALSE!!!
topic_12 <- read.csv("/home/kingfish/gts/topic_12_rate_internal.csv", header=FALSE)
tx = topic_12[2][1]
tdt = as.Date(topic_12[1][,1])
t12ts <- zoo(tx, tdt)
plot.ts(t12ts)
plot(t12ts)
summary(t12ts)



library(sqldf)
library(data.table)
t_twelve <- dput(t12ts)
#t12 <- as.data.table(t12ts)
t12 <- as.data.table(cbind(tx, tdt)) 
t12
tbill_data <- as.data.table(cbind(tbill_x, tbill_dt))
tbill_data

# do not run this - this is a cartesian join - it will hang your machine
#sqldf("
#      SELECT * 
#      FROM t12 as t12, tbill_data as b, t12 as t12_plus1
#      ")

#print the current and the day before the next Fed statement
sqldf("SELECT t12_1.tdt, t12_2.tdt-1 
      FROM t12 as t12_1, 
      t12 as t12_2 
      WHERE t12_2.tdt > t12_1.tdt 
      AND t12_2.tdt = (SELECT MIN(t12_3.tdt) AS tdt 
                         FROM t12 as t12_3 
                        WHERE t12_3.tdt > t12_1.tdt)")

#how many days between Fed statements, max? 
sqldf("SELECT MAX(t12_2.tdt-1 - t12_1.tdt)
         FROM t12 as t12_1, 
              t12 as t12_2 
        WHERE t12_2.tdt > t12_1.tdt 
          AND t12_2.tdt = (SELECT MIN(t12_3.tdt) AS tdt 
                             FROM t12 as t12_3 
                            WHERE t12_3.tdt > t12_1.tdt)")
#107 for Greenspan, given robustness of scraper for Greenspan corpus

#how many days min? 
sqldf("SELECT MIN(t12_2.tdt-1 - t12_1.tdt)
         FROM t12 as t12_1, 
              t12 as t12_2 
        WHERE t12_2.tdt > t12_1.tdt 
          AND t12_2.tdt = (SELECT MIN(t12_3.tdt) AS tdt 
                             FROM t12 as t12_3 
                            WHERE t12_3.tdt > t12_1.tdt)")

#0 ------> could be a problem

#how many days avg? 
sqldf("SELECT AVG(t12_2.tdt-1 - t12_1.tdt)
         FROM t12 as t12_1, 
              t12 as t12_2 
      WHERE t12_2.tdt > t12_1.tdt 
      AND t12_2.tdt = (SELECT MIN(t12_3.tdt) AS tdt 
      FROM t12 as t12_3 
      WHERE t12_3.tdt > t12_1.tdt)")

# ~13.4 avg



#get a table consisting of a date and a rolling average from t_bill data
tbill_rolling_avg <- sqldf("SELECT DATE(t12_1.tdt) AS dt, 
                                   AVG(tb.V2) AS rolling_avg
                              FROM t12 as t12_1, 
                                   t12 as t12_2,
                                   tbill_data as tb
                             WHERE t12_2.tdt > t12_1.tdt 
                               AND tb.tbill_dt BETWEEN t12_1.tdt  AND t12_2.tdt-1
                               AND t12_2.tdt = (SELECT MIN(t12_3.tdt) AS tdt 
                                                  FROM t12 as t12_3 
                                                 WHERE t12_3.tdt > t12_1.tdt)
                             GROUP BY t12_1.tdt")

tbill_rolling_avg

#clumsy, embarassing attempt to deal with R syntax
#R assigns names V1, V2, investigate
tbill_rolling_avg_dates <- as.data.table(cbind(tbill_rolling_avg$rolling_avg, tbill_rolling_avg$dt))

tbill_rolling_avg_dates

head(sqldf("SELECT *
      FROM tbill_rolling_avg_dates as tb"))
# make sure that dates are the same, same #
new_tbill <- sqldf("SELECT tb.V1 AS rolling_avg, t12.tdt
                   FROM tbill_rolling_avg_dates as tb,
                         t12
                   WHERE tb.V2 = t12.tdt
                   ")



new_t12 <- sqldf("SELECT t12.tdt, t12.V2
                 FROM tbill_rolling_avg_dates as tb,
                      t12
                 WHERE tb.v2 = t12.tdt
                 ")

# now plot!
model1 <- lm(coredata(new_t12$V2) ~ coredata(new_tbill$rolling_avg))
plot(fitted(model1))
model2 <- lm(coredata(new_tbill$rolling_avg) ~ coredata(new_t12$V2))

library(car)
#bring in other topics, other macroeconomic indicators, etc. at this point
#make code elegant

dwtest(model1)
dwtest(model2)



plot(lm(diff(coredata(new_t12$V2)) ~ diff(coredata(new_tbill$rolling_avg))))

cor.test(coredata(new_t12$V2), coredata(new_tbill$rolling_avg), method="spearman")
plot(lm(coredata(new_t12$V2)~ coredata(new_tbill$rolling_avg)))

model <- lm(coredata(new_t12$V2) ~ coredata(new_tbill$rolling_avg))

plot(y=coredata(new_t12$V2), x=coredata(new_tbill$rolling_avg), ylab="topic proportion", xlab="tbill")

plot( glm(coredata(diff(new_t12$V2)) ~ coredata(diff(new_tbill$rolling_avg))))

as.Date(tbill_rolling_avg$dt)
plot(tbill_rolling_avg)
tbill_ts <- xts(x=tbill_rolling_avg$rolling_avg, as.Date(tbill_rolling_avg$dt))
plot(tbill_ts)
plot(diff(tbill_ts))
t12ts_new <- window(t12ts, start=as.Date("1987-09-09"), end=as.Date("2005-11-03"))

length(tbill_ts)
length(t12ts_new)
coredata(tbill_ts)
coredata(t12ts_new)

model <- lm(coredata(diff(tbill_ts)) ~ coredata(t12ts_new))
dwtest(model)

sqldf("SELECT * FROM tbill_data")


tbill_x

ttt12 <- as.table(tx)
sqldf("
      SELECT * 
      FROM t12 ")



t12ts_newt <-  na.locf(merge(t12ts, emptyTS, all=TRUE))

plot(t12ts_newt)
t12ts_newt
length(t12ts_newt)

emptyTS
length(emptyTS)
newt12 <- na.locf(merge(t12ts, emptyTS, all=TRUE))
newt12
ts.plot(newt12)
length(newt12)
orig_x <- djia_ts[2][1]

dt <- as.Date(djia_ts[1][,1])

ts <- zoo(orig_x, dt)
newts <- na.locf(merge(ts, emptyTS, all=TRUE))
plot(newts)
length(newts)

summary(newts)


dates <- seq(from=as.Date('1990-01-02'), to=as.Date('2000-12-31'), by=1)
newts_sub <- newts[dates]
newt12_sub <- diff(newt12[dates])
tbill_sub <- tbill_newt[dates]

length(newts_sub)
newts_sub
length(newt12_sub)
newt12_sub
length(tbill_sub)

ccf(newt12_sub, newts_sub)
ccf(newt12_sub, tbill_sub)
ccf(newts_sub, tbill_sub)
ccf(newts_sub, newt12_sub)

plot(lm(coredata(tbill_sub) ~ coredata(newts_sub)))
plot(lm(coredata(tbill_sub) ~ coredata(newt12_sub)))


dates <- seq(as.D)
length(newts) #6648
newts
length(newt12) #6572 --- 05-11-14
length(tbill_newt)


# ------------> sqldf

library(xts)
library(rjson)

range(ts)
summary(ts)
rjson::toJSON(t)
#extractRelevantDatesFromSpeechesTimeSeriesFuncWithDJIA
#lapply(speechDateList, ts)


#for all Greenspan speeches
#1.) loop through term list for speech and identify (%IN%) and tally occurrences of terms from current topic from DTM, log of sum of which defines the score for each topic for each speech
#----2.) get sum of DJIA change from time series given date of speech 
#3.) store the topic tally and DJIA calculation post-speech with date from #2 for further analysis

#gatherDataFunc

#input: Term list
#input: dtm
#input: ts
#output: list consisting of speeches, their respective speech dates in proper date format, the topic index, scores for each topic for that speech and the DJIA score


#  TermScore <- lapply(greenspanDataFrame, getTermScoreFromSpeechDTM)
# topicScore <- sum of TermScore[]


newts

#regressTopicDJIAFunc
#input: results from gatherDataFunc
#method: linear regression
#output: score for each topic (ANOVA?)






as.zoo(ts)


# apply.monthly(ts, f)
#create time series for topic model from topic tally
#normalize with date of speeches


#plot DJIA and perform padding as needed


