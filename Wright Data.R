#Working with Wright Data

#make a list of all files
WRIGHT = list.files("data/wright-txt",full.names = TRUE)

#function to read them in
readWRIGHT = function(file) {
  message(file)
  text = paste(scan(file, sep="\n",what="raw",strip.white = TRUE),collapse = "/n")
  WRIGHT = data.frame(filename=file,text=text,stringsAsFactors = FALSE)  
  return(WRIGHT)
}

#run the function
allWRIGHT = data.frame(filename=WRIGHT,stringsAsFactors=FALSE) %>% 
  group_by(filename) %>% 
  do(readWRIGHT(.$filename))

#remove everything above Full Text
allWRIGHT$text = gsub("/n", "", allWRIGHT$text)
#this removes line breaks
allWRIGHT$text = gsub("\r?\n|\r", "\\s", allWRIGHT$text)
allWRIGHT$text = gsub(".*?----FULL TEXT----", " ", allWRIGHT$text)

#sample 8 in an effort to get a smaller corpus
#allWRIGHT <- allWRIGHT[sample(1:nrow(allWRIGHT), 8,replace=FALSE),]

#convert text to string and chunk by character count. Convert to df
allWRIGHT_string = paste(unlist(allWRIGHT$text), collapse =" ")
allWRIGHT_string = substring(allWRIGHT_string,seq(1,nchar(allWRIGHT_string),2000),seq(2000,nchar(allWRIGHT_string),2000))
allWRIGHT2 = allWRIGHT_string %>% as.data.frame()

# Use Lincoln's update of tokenizers to chunk text
allWRIGHT2 = chunk_text(allWRIGHT$text, chunk_size = 300, doc_id = allWRIGHT$filename) %>% as_data_frame()
allWRIGHT2 = allWRIGHT2 %>% gather(id, text)

allWRIGHT2 = allWRIGHT2 %>% as.data.frame()
allWRIGHT2 = allWRIGHT2 %>% mutate(genre="fiction")
allWRIGHT2 = allWRIGHT2 %>% mutate(cluster=paste("wright",1:nrow(allWRIGHT2), sep = "_"))
names(allWRIGHT2)[names(allWRIGHT2)=="."] <- "text"
allWRIGHT2 = allWRIGHT2[,c("cluster","text","genre")]
allWRIGHT2 <- allWRIGHT2[sample(1:nrow(allWRIGHT2), 200,replace=FALSE),]
allWRIGHT2$text = allWRIGHT2$text %>% as.character() 


#add genre column and convert filename to cluster
allWRIGHT = allWRIGHT %>% mutate(genre="fiction")
names(allWRIGHT)[names(allWRIGHT)=="filename"] <- "cluster"
allWRIGHT = allWRIGHT %>% as.data.frame()
#allWRIGHT <- allWRIGHT[sample(1:nrow(allWRIGHT), 8,replace=FALSE),]

#read in fiction found with "by"
allFiction = read.csv("data/fiction-with-by.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
allFiction = allFiction %>% mutate(genre = "fiction")
allFiction = allFiction[,c("cluster","text","genre")]


#read in news
allNews = read.csv("output/justNews-5-23-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
names(allNews)[names(allNews)=="classified_genre"] <- "genre"
names(allNews)[names(allNews)=="Text"] <- "text"
allNews = allNews[,c("cluster","text","genre")]
allNews <- allNews[sample(1:nrow(allNews), 200,replace=FALSE),]
allNews$text = gsub("\r?\n|\r", "\\s", allNews$text)


#read in vignettes
allVignettes = read.csv("data/200vignettes.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
allVignettes = allVignettes[,c("cluster","text","genre")]
allVignettes = allVignettes %>% mutate(genre="unknown")
#allVignettes <- allVignettes[sample(1:nrow(allNews), 75,replace=FALSE),]
allVignettes$text = gsub("\r?\n|\r", "\\s", allVignettes$text)


#combine above
allData = rbind(allWRIGHT2,allNews,allVignettes)




# LET'S COMPARE THE RESULTS WITH OTHER GENRES (ads, poetry)

#poetry
allPoetry = read.csv("data/genreClass-4-13-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
allPoetry = allPoetry %>% filter(classified_genre == "poetry")
names(allPoetry)[names(allPoetry)=="classified_genre"] <- "genre"
names(allPoetry)[names(allPoetry)=="Text"] <- "text"
allPoetry = allPoetry[,c("cluster","text","genre")]
allPoetry = allPoetry %>% mutate(genre="unknown")
allPoetry <- allPoetry[sample(1:nrow(allNews), 200,replace=FALSE),]

#combine above
allData = rbind(allWRIGHT2,allNews,allPoetry)

#ads
allAds = read.csv("output/justAds-5-23-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
allAds = allAds[,c("cluster","text","genre")]
allAds = allAds %>% mutate(genre="unknown")
allAds <- allAds[sample(1:nrow(allNews), 200,replace=FALSE),]


#combine above
allData = rbind(allWRIGHT2,allNews,allAds)


#Trying to find other fiction. Instead of reading in vignettes, read in everything
newData = beginData #from ClusterAnalysis.R
newData$cluster <- as.character(newData$cluster)
newData = newData %>% group_by(cluster) %>% slice(1)
newData = newData %>% mutate(genre="unknown")
newData = newData[,c("cluster","text","genre")] 
newData = newData %>% as.data.frame()
#Sample data down to 5000
#newData <- newData[sample(1:nrow(newData), 5000,replace=FALSE),]

#combine above
allData = rbind(allWRIGHT2,allNews,allVignettes)

