fedspeak
========

pittsburgh_r_useR_group_Nov_4_2013 (http://www.meetup.com/Pittsburgh-useR-Group/)

Purpose: analysize Fedspeak

Fedspeak/Greenspeak Definition: 
	“Language of purposeful obfuscation to avoid certain questions coming up where saying 'no comment' is in 
 	 fact an answer. So I proceed with 4 or 5 sentences which become increasingly obscure and the Senator 
	 thinks his questions has been answered.” 
                  - Alan Greenspan, somewhere on youtube -> http://www.youtube.com/watch?v=9JHctWztRww
                  
Goods: 

 * created time series from topic model proportions using R and imported these results into d3.js
 * rPython_NLTK.R R to Python interface to access implementation of Marti Hearst's Textiling algorithm, auto-segmentation of texts using roving windows and cosine similarity metrics to identify topical cleavages/breaks, i.e. paragraphs or narrames: http://people.ischool.berkeley.edu/~hearst/research/tiling.html
 * tmIndMarkReg.R is the heart of the project and it is also the most messy, code-wise
 

TO DO: 

 * clean up process of generating time series from topic model proportions (*apply-family of f(x))
 * create annotated d3.js (http://d3js.org/) /rickshaw.js (http://code.shutterstock.com/rickshaw/) application
