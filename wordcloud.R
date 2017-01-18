#WORDCLOUD (source: https://rpubs.com/chirag/WordCloud)


#create corpus
us_text<- sapply(allTAlist,function(x) x$getText())
us_corpus<- Corpus(VectorSource(us_text))

#cleanup
us_corpus <- tm_map(us_corpus,
                    content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                    mc.cores=1
)
lords <- tm_map(us_corpus, stripWhitespace, mc.cores=1)
lords <- tm_map(lords, content_transformer(tolower), mc.cores=1)
lords <- tm_map(lords, removePunctuation, mc.cores=1)
lords <- tm_map(lords, removeNumbers, mc.cores=1)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
lords <- tm_map(lords, toSpace, "http\\S+\\s*", mc.cores=1)
lords <- tm_map(lords, toSpace, "http\\S+\\s*", mc.cores=1)
lords <- tm_map(lords, toSpace, "trumpsamerica", mc.cores=1)
lords <- tm_map(lords, toSpace, "#", mc.cores=1)
lords <- tm_map(lords, toSpace, "http\\w+", mc.cores=1)
lords <- tm_map(lords, toSpace, "@\\w+", mc.cores=1)
lords <- tm_map(lords, toSpace, "https\\w+", mc.cores=1)
lords <- tm_map(lords, toSpace, "uselect", mc.cores=1)
lords <-tm_map(lords, removeWords, c(stopwords("english"),"shaunking","trump","donald"))

?tm_map

wordcloud(lords,random.order=F,min.freq=10,max.words=100,colors=brewer.pal(8, "Dark2"))

