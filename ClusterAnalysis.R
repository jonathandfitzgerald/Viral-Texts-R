#Read in clusters parts
part00000 = read.csv("data/part-00000", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
part00001 = read.csv("data/part-00001", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
part00002 = read.csv("data/part-00002", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
part00003 = read.csv("data/part-00003", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
part00004 = read.csv("data/part-00004", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
beginData = rbind(part00000,part00001,part00002,part00003,part00004)
beginData = beginData[,c("cluster","text","url","size")] 

newBeginData_slim = beginData[,c("cluster","text","url","size")] 

#New Stuff

newPart00000 = read_csv("data/8-3-17-download/part-00000-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00001 = read_csv("data/8-3-17-download/part-00001-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00002 = read_csv("data/8-3-17-download/part-00002-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00003 = read_csv("data/8-3-17-download/part-00003-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00004 = read_csv("data/8-3-17-download/part-00004-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00005 = read_csv("data/8-3-17-download/part-00005-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00006 = read_csv("data/8-3-17-download/part-00006-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00007 = read_csv("data/8-3-17-download/part-00007-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00008 = read_csv("data/8-3-17-download/part-00008-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00009 = read_csv("data/8-3-17-download/part-00009-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00010 = read_csv("data/8-3-17-download/part-00005-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00011 = read_csv("data/8-3-17-download/part-00006-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00012 = read_csv("data/8-3-17-download/part-00007-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00013 = read_csv("data/8-3-17-download/part-00008-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newPart00014 = read_csv("data/8-3-17-download/part-00009-c47c31a1-2923-43fe-a32b-f1b8f5b9b2fb.csv")
newBeginData = rbind(newPart00000,newPart00001,newPart00002,newPart00003,newPart00004,newPart00005,newPart00006,newPart00007,newPart00008,newPart00009,newPart00010,newPart00011,newPart00012,newPart00013,newPart00014)
newBeginData_slim = newBeginData[,c("cluster","text")] 
newBeginData_slim = newBeginData_slim %>% mutate(genre="unknown")
newBeginData_slim$cluster <- newBeginData_slim$cluster %>% as.character()

#select longest witness of each cluster
beginData = beginData %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1)

newBeginData = newBeginData %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1)

newBeginData_slim = newBeginData_slim %>% as_data_frame()

#Read in clusters parts (end of corpus)
part00005 = read.csv("data/part-00995", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
part00006 = read.csv("data/part-00996", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
part00007 = read.csv("data/part-00997", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
part00008 = read.csv("data/part-00998", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
part00009 = read.csv("data/part-00999", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
endData = rbind(part00005,part00006,part00007,part00008,part00009)
endData = endData[,c("cluster","text","url","size")] 

#select longest witness of each cluster
endData = endData %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1)

endData <- endData[sample(1:nrow(endData), 5000,replace=FALSE),]


newData = rbind(beginData, endData)
newData = newData[,c("cluster","text")] 
newData = newData %>% mutate(genre="unknown")
newData$cluster <- as.character(newData$cluster)
newData <- newData %>% as_data_frame()

#sampe data
sampleData <- newData[sample(1:nrow(newData), 20,replace=FALSE),]
write.csv(sampleData, file = paste('output/sampleData-5-2-16.csv',sep=""))

#add in classfiied genres from original set with cluster number transformed to be D1, D2, etc.
oldGenres = read.csv("data/classifiedGenresFromOriginalData.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)

#Read in hand-tagged clusters
allDataGenres = read.csv("data/newdata-5-11-handtagged-genres.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
#This should be Priamry.Genre in most cases
names(allDataGenres)[names(allDataGenres)=="Top.Level"] <- "genre"
allDataGenres = allDataGenres[,c("cluster","text","genre")]
allDataGenres$cluster = as.character(allDataGenres$cluster)

#Read in hand-tagged clusters - Select 5 from each Secondary Genre
allDataGenres = read.csv("data/newdata-5-11-handtagged-genres.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
allDataGenres = allDataGenres %>% group_by(Secondary.Genre) %>% slice(1:5)
names(allDataGenres)[names(allDataGenres)=="Secondary.Genre"] <- "genre"
allDataGenres = allDataGenres[,c("cluster","text","genre")]
allDataGenres$cluster = as.character(allDataGenres$cluster)
allDataGenres = allDataGenres[ which(allDataGenres$genre!=''), ]

#Read in sentimental classifier & merge with newData
sentimental = read.csv("data/sentimental-11-19-16-combined.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
sentimental$cluster = as.character(sentimental$cluster)
allData = newData %>% left_join(sentimental, by="cluster")
allData = allData[,c("cluster","text.x","genre.y")]
names(allData)[names(allData)=="genre.y"] <- "genre"
names(allData)[names(allData)=="text.x"] <- "text"
allData = replace(allData, is.na(allData), "unknown") 


#Merge hand-tagged clusters into alldata
allData <- rbind(newData_slim,allDataGenres)


allData = newData %>% left_join(allDataGenres, by="cluster")
allData = allData[,c("cluster","text.x","genre.y")]
names(allData)[names(allData)=="genre.y"] <- "genre"
names(allData)[names(allData)=="text.x"] <- "text"
#allData <- allData[sample(1:nrow(allData), 2000,replace=FALSE),]
allData = replace(allData, is.na(allData), "unknown") 

# Merge early LJ with alldata
#allData <- rbind(allData,earlyLJ) %>% as_data_frame()
#allData <- allData %>% filter(genre != "prose")

# Merge new genres from PCA with alldata
newData <- allData %>% filter(genre == "unknown")
allData <- rbind(newData,newGenres)
#allData <- inner_join(allParts_short, newGenres)
newGenres$cluster <- newGenres$cluster %>% as.character()
allData <- rbind(newBeginData_slim, newGenres)

#Merge hand-tagged vignettes into alldata
vignettes = read.csv("data/vignettesWithA-7-5-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
vignettes = vignettes[,c("cluster","text","genre")]
vignettes$cluster = as.character(vignettes$cluster)

sampleData = sampleData[,c("cluster","text")]
sampleData = sampleData %>% mutate("genre" = "notVignette")

vignettes = rbind(vignettes,sampleData)

allData = newData %>% left_join(vignettes, by="cluster")
allData = allData[,c("cluster","text.x","genre")]
names(allData)[names(allData)=="text.x"] <- "text"
allData = replace(allData, is.na(allData), "unknown") 

#Merge hand-tagged clusters into alldata (bottom end of data)
newData = newData %>% mutate("genre"=NA)
allData = rbind(newData,allDataGenres)
allData = replace(allData, is.na(allData), "unknown") 

#Read in hand-tagged clusters - JUST PROSE
proseGenres = read.csv("data/newdata-5-11-handtagged-PROSE.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
names(proseGenres)[names(proseGenres)=="Primary.Genre"] <- "genre"
proseGenres = proseGenres[,c("cluster","text","genre")]
proseGenres$cluster = as.character(proseGenres$cluster)
proseGenres = proseGenres %>% group_by(genre) %>% mutate(count=n()) %>% filter(count>5)

#Merge hand-tagged clusters into alldata - JUST PROSE
justProse = justProse %>% mutate(genre="unknown")
justProse = justProse[,c("cluster","text","genre")]
proseGenres = proseGenres[,c("cluster","text","genre")]
allProse = rbind(justProse,proseGenres)

#Remove classified ads from data
allDataNoAds =  allData %>% anti_join(justAds, by="cluster")
allDataNoAds$genre = "unknown"

#Merge hand-tagged clusters into alldataNoAds (remove ads from training data)
allDataGenres = allDataGenres %>% filter(genre!="advertisement")
allDataNoAds = allDataNoAds %>% left_join(allDataGenres, by="cluster")
allDataNoAds = allDataNoAds[,c("cluster","text.x","genre.y")]
names(allDataNoAds)[names(allDataNoAds)=="text.x"] <- "text"
names(allDataNoAds)[names(allDataNoAds)=="genre.y"] <- "genre"
allDataNoAds = replace(allDataNoAds, is.na(allDataNoAds), "unknown") 

#Remove classified news from data
allDataNoNews = genreClass %>% filter(classified_genre!="news")
names(allDataNoNews)[names(allDataNoNews)=="classified_genre"] <- "genre"
names(allDataNoNews)[names(allDataNoNews)=="Text"] <- "text"
allDataNoNews = allDataNoNews[,c("cluster","text","genre")]
allDataNoNews$genre = "unknown"

#Merge hand-tagged clusters into allDataNoNews (remove news from training data)
allDataGenres = allDataGenres %>% filter(genre!="news", genre!="advertisement")
allDataNoNews = rbind(allDataNoNews,allDataGenres)

#Remove classified poetry from data
allDataNoPoetry = genreClass %>% filter(classified_genre!="poetry")
names(allDataNoPoetry)[names(allDataNoPoetry)=="classified_genre"] <- "genre"
names(allDataNoPoetry)[names(allDataNoPoetry)=="Text"] <- "text"
allDataNoPoetry = allDataNoPoetry[,c("cluster","text","genre")]
allDataNoPoetry$genre = "unknown"

#Merge hand-tagged clusters into allDataNoPoetry (remove news from training data)
allDataGenres = allDataGenres %>% filter(genre!="poetry", genre!="advertisement")
allDataNoPoetry = rbind(allDataNoPoetry,allDataGenres)


#Visualize clusters by year
newData %>%
  mutate(year=gsub(".*(\\d{4}).*","\\1",date)) %>%
  mutate("year" = as.numeric(year)) %>%
  group_by(year) %>%
  mutate(count=n()) %>% 
  ungroup %>%
  ggplot() +
  geom_line() +
  aes(x=year,y=count) + 
  geom_point(size=count,alpha=.8) + 
  ggtitle("Number of clusters by year in newData")


#Visualize genres by year
justEverything = read.csv("output/genreRank-6-7-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
justEverything$cluster = as.character(justEverything$cluster)
justEverything = justEverything %>% left_join(newData, by="cluster")


justEverything %>%
  mutate(year=gsub(".*(\\d{4}).*","\\1",date)) %>%
  mutate("year" = as.numeric(year)) %>%
  group_by(year,genre_1) %>%
  mutate(count=n()) %>% 
  ungroup %>%
  filter(year>1800) %>% 
  ggplot() +
  geom_smooth(method = "loess") +
  aes(x=year,y=count,color=genre_1) + 
  ggtitle("Number of genres by year") 

#Visualize Probabilities
tidied %>%
  left_join(newData) %>% 
  mutate(year=gsub(".*(\\d{4}).*","\\1",date)) %>%
  mutate("year" = as.numeric(year)) %>%
  group_by(year,classified_genre) %>%
  mutate(count=n()) %>% 
  mutate(probability_percent=probability*100) %>% 
  ungroup %>%
  filter(year>1800) %>% 
  ggplot() +
  geom_point() +
  aes(x=year,y=probability_percent,color=classified_genre) + 
  ggtitle("Number of genres by year") 


#Delete all data
rm(list=ls())
rm(topic.words)
