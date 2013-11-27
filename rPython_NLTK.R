#python nltk integration --- using Marti Hearst's textiling algorithm
# textiling uses a roving text window to  identify breaks in the topical structure within a text
# http://people.ischool.berkeley.edu/~hearst/research/tiling.html

# http://clover.slavic.pitt.edu/sam/propp/praxis/results.html#final

# http://www.lrec-conf.org/proceedings/lrec2012/pdf/876_Paper.pdf




library(rPython)
python.exec("import nltk")
python.exec("import nltk.corpus")
python.exec("from nltk.corpus import PlaintextCorpusReader")
python.exec("from urllib import urlopen")
python.exec("import numpy")
python.exec("corpus_root = '/home/hinckley/Public/corpora/transcripts/1936'")
python.exec("corpus = PlaintextCorpusReader(corpus_root, '.*')")
python.get("corpus.fileids()")
python.get("corpus.open('fomcropa19361120.txt')")
python.exec("url = \"/home/hinckley/Public/corpora/transcripts/1936/fomcropa19361120.txt\"")
python.exec("raw = urlopen(url).read()")
#python.exec("alice = nltk.corpus.gutenberg.raw('carroll-alice.txt')")
#python.get("alice")
python.exec("ttt = nltk.tokenize.TextTilingTokenizer()")
#c <- unlist(sapply(corpus[[1]], paste))
python.exec("corpus")
#python.assign("corpus", c)
python.get("corpus")
#python.exec(c("text = """"", c, """"))
python.get("raw")
python.exec(paste("tiles = ttt.tokenize(raw)"))
zip <- python.get("tiles[2]")
python.get("tiles[4]")
text <- paste("espeak -p 99 \"", zip, "\"", sep="")
text
system(text)

python.exec("nltk.tokenize.texttiling.demo(text=None)")


