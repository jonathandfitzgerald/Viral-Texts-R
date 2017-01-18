#Required packages
library(plyr)
library(dplyr)
library(RCurl)
library(rjson)
library(tidyr)
library(ggplot2)
library(tm)
library(rlist)
library(RSQLite)
library(devtools)
library(e1071)
library(coreNLP)
#library(bookworm)
library(mallet)
library(pamr)
library(lava)
library(hunspell)

#library(ggbiplot)


#Get Words function for PCA

getWords = function(data.frame) {
  #Get list of words associated with URL
  clusterWords = df %>% group_by(V1) %>% do({
    text = .$V9 %>% as.character
    words = text %>% 
      strsplit("[^#A-Za-z]") %>%  # Might want to change this eventually into something more elaborate... try to split on spaces and delete punctuation, got Error: Duplicate identifiers for rows
      unlist
    clusterWords = data.frame(word=words,stringsAsFactors = FALSE) 
    # %>% gsub("[\\//“”\";:.,!?\\-]","",.)  
    return(clusterWords)
  })
  
}

#Not sure this is working, or it is but there's not enough data.
getWords2 = function(data.frame) {
  #Get list of words associated with URL
  poetryWords = dfPoems %>% group_by(V1) %>% do({
    text = .$V9 %>% as.character
    words = text %>% 
      strsplit("[^#A-Za-z]") %>%  # Might want to change this eventually into something more elaborate... try to split on spaces and delete punctuation, got Error: Duplicate identifiers for rows
      unlist
    poetryWords = data.frame(word=words,stringsAsFactors = FALSE) 
    # %>% gsub("[\\//“”\";:.,!?\\-]","",.)  
    return(poetryWords)
  })
  
}

#More PCA stuff (?)

allWordsIntoTD = function (clusterWords,normalized=F) {
  
  #New data.frame without spaces
  td = counts %>% group_by(word) %>% 
    filter(sum(count)>wordCutoff,word!="",word!="br",word!="the",word!="of",word!="and",word!="to",word!="in") %>%
    ungroup %>% 
    spread(word,count,fill=0)
  
  if (normalized) {
    # allow normalization as an argument
    normalized = counts %>% mutate(ratio = count/sum(count)) %>% ungroup
    norm_data_frame = normalized %>% 
      group_by(word) %>% 
      filter(sum(count)>wordCutoff,word!="",word!="br",word!="the",word!="of",word!="and",word!="to",word!="in") %>% 
      select(-count)
    td  = norm_data_frame %>% ungroup %>% spread(key=word,value=ratio,fill=0)
  }
  
  return(td)
  
}

#dunning.log
dunning.log = function(set1,set2) {
  wordlist =  merge(set1,set2,by=intersect(names(set1),names(set2)[grep("WordCount",names(set2),invert=T)]),all.x=T,all.y=T)
  #takes a data frame with columns "word," "count.x" and "count.y"
  #Formula (whence variable names) taken from http://wordhoard.northwestern.edu/userman/analysis-comparewords.html
  wordlist$WordCount.x[is.na(wordlist$WordCount.x)] = .5
  wordlist$WordCount.y[is.na(wordlist$WordCount.y)] = .5
  
  wordlist$count.x = wordlist$WordCount.x
  wordlist$count.y = wordlist$WordCount.y  
  attach(wordlist)
  
  wordlist[wordlist==0] = .5
  c = sum(count.x); d = sum(count.y); totalWords = c+d
  wordlist$count.total = count.x+count.y
  wordlist$exp.x = c*(wordlist$count.total)/(totalWords)
  wordlist$exp.y = d*(wordlist$count.total)/(totalWords)
  wordlist$over.x = wordlist$count.x - wordlist$exp.x
  wordlist$over.y = wordlist$count.y - wordlist$exp.y
  
  wordlist$score = 2*(
    (wordlist$count.x*log(
      wordlist$count.x/wordlist$exp.x)) + 
      wordlist$count.y*log(wordlist$count.y/wordlist$exp.y))
  #This makes it negative if the second score is higher
  wordlist$score = wordlist$score * ((wordlist$over.x > 0)*2-1)
  detach(wordlist)
  dunning = wordlist$score
  
  names(dunning) = apply(
    wordlist[,names(set1)[grep("[Cc]ount",names(set1),invert=T),drop=F],drop=F],1,paste,collapse=" ")
  
  data.frame(word = names(dunning),largerIn = ifelse(dunning>0,"set1","set2"),dunning=abs(dunning),group1=wordlist$count.x,group2=wordlist$count.y)
}


