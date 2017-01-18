
#======================#
#Words to dataframe

getusersWords = function(data.frame) {
  #Get list of words associated with URL
  allDataWords = data.frame %>% group_by(cluster) %>% do({
    text = .$text %>% as.character
    words = text %>% 
      strsplit("[^#A-Za-z]") %>%  # Might want to change this eventually into something more elaborate... try to split on spaces and delete punctuation, got Error: Duplicate identifiers for rows
      unlist
    allDataWords = data.frame(word=words,genre=.$genre,stringsAsFactors = FALSE) 
    # %>% gsub("[\\//“”\";:.,!?\\-]","",.)  
    return(allDataWords)
  })
  
}

#Wordcounts

allDataWordsIntoTD = function (allDataWords,normalized=F) {
  
  #New data.frame without spaces
  td = counts %>% group_by(word) %>% 
    filter(sum(count)>wordCutoff,word!="") %>%
    ungroup %>% 
    spread(word,count,fill=0)
  
  if (normalized) {
    # allow normalization as an argument
    normalized = counts %>% group_by(cluster) %>% mutate(ratio = count/sum(count)) %>% ungroup
    norm_data_frame = normalized %>% group_by(word) %>% filter(sum(count)>wordCutoff,word!=" ") %>% select(-count)
    td  = norm_data_frame %>% ungroup %>% spread(key=word,value=ratio,fill=0)
  }
  
  return(td)
  
}



wordCutoff = 100
df = allData
allDataWords = getusersWords(df)
counts = allDataWords %>% filter(word!="") %>% mutate(word=tolower(word)) %>% group_by(cluster,word,genre) %>% summarize(count=n()) %>% ungroup
td = allDataWordsIntoTD(allDataWords,normalized = T) 

#check words against dictionary
testwords = allDataWords %>% filter(word!="")
testwords = testwords$word %>% as.String()
badwords = hunspell(testwords)
testwords = removeWords(testwords, badwords)
print(badwords[[2]])
hunspell(allDataWords$word)

#PCA
justaMatrix = td %>% select(-cluster, -genre)
model = prcomp(justaMatrix)
prediction = predict(model)
prediction = prediction %>% 
  as.data.frame %>% mutate(cluster=td$cluster %>% as.character, genre=td$genre %>% as.character) %>% 
  select(cluster,genre,PC1,PC2,PC3)


#======================#