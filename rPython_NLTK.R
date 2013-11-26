#python nltk integration --- using Marti Hearst's textiling algorithm
# textiling uses a roving text window to  identify breaks in the topical structure within a text
# http://people.ischool.berkeley.edu/~hearst/research/tiling.html
# http://clover.slavic.pitt.edu/sam/propp/praxis/results.html#final

# http://www.lrec-conf.org/proceedings/lrec2012/pdf/876_Paper.pdf


library(rPython)
python.exec("import nltk")
python.exec("import numpy")
python.exec("alice = nltk.corpus.gutenberg.raw('carroll-alice.txt')")
python.get("alice")
python.exec("ttt = nltk.tokenize.TextTilingTokenizer()")
python.assign("alice", c(corpus[[1]], sep=""))
python.get("alice")
python.exec(paste("tiles = ttt.tokenize(alice)"))
python.get("t1 = tiles[-1]")
python.exec("t1")
python.get("tiles[-2]")

