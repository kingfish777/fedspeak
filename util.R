library(wordnet)
system("javac -version")
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)

library("rJava")
library("RWeka")
library("RWekajars")
#library("Rgraphviz")
#library("MASS")
#library("slam")
#library("igraph")
library("wordnet")
setDict("C:/Program Files (x86)/WordNet/2.1/dict")
if(initDict("C:/Program Files (x86)/WordNet/2.1/dict"))
  getDict()
#wordnet::getDict()
interestingList <- as.list(synonyms('prevarication', pos='VERB'))
synonyms('betray', pos='VERB')
synonyms('prevaricate', pos='VERB')
resultPrevarication <- synonyms('cheat', pos='VERB')
mode(resultPrevarication)
greenspan <- function(x) { synonyms(x, pos='VERB') }
gr<-as.list(resultPrevarication)
lapply(gr, greenspan)
RecGreenspan <- function(x) { if (length(x) > 1) { break }
                              x <- synonyms(x, pos='VERB')
                              x<-as.list(x)
                              #RecGreenspan(x)
                              #print('#######################')
                              #print(length(x))
                              print(x)
                              #x<-lapply(x, RecGreenspan)
                              #print('#######################')
                              #return(x)
}
a<-RecGreenspan('betray')
a
unlist(a)

