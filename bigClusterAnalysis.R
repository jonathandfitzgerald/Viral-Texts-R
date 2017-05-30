
#massive combined file

allParts = read_csv("~/Documents/Northeastern/Viral Texts Project/Viral-Texts-R/data/combined_short.csv")
allPartsDates = read_csv("~/Documents/Northeastern/Viral Texts Project/Viral-Texts-R/data/combined_short2.csv")


#select longest witness of each cluster
allPartsDates = allPartsDates %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1)

allParts <- allParts[,c("cluster","text")] %>% mutate(genre = "unknown")
allParts$cluster <- as.character(allParts$cluster)

allParts_short <- allParts[sample(nrow(allParts), 15000),]


#read in classified clusters from big data
bigClustersClassRaw = read_csv("~/Documents/Northeastern/Viral Texts Project/Viral-Texts-R/output/genreClass-2-19-17.csv")
bigClustersClassRaw <- bigClustersClassRaw[,c("cluster","text","classified_genre", "probability")]

#merge dates from allPartsDates and visualize
bigClustersClass = bigClustersClassRaw %>% inner_join(allPartsDates,by = "cluster")
bigClustersClass <- bigClustersClass[,c("cluster","text.x","classified_genre", "date")]
colnames(bigClustersClass)[colnames(bigClustersClass) == 'text.x'] <- 'text'
colnames(bigClustersClass)[colnames(bigClustersClass) == 'classified_genre'] <- 'genre'

gTime <- bigClustersClass %>%
  mutate(year=gsub(".*(\\d{4}).*","\\1",date)) %>%
  mutate("year" = as.numeric(year)) %>%
  group_by(year,classified_genre) %>%
  mutate(count=n()) %>% 
  ggplot() +
  geom_line(aes(x = year, y = count, color = classified_genre)) 

ggplotly(gTime)
chart_link = plotly_POST(gTime, filename="vt-genres-over-time")
chart_link

gTime <- bigClustersClass %>%
  mutate(year=gsub(".*(\\d{4}).*","\\1",date)) %>%
  mutate("year" = as.numeric(year)) %>%
  group_by(year,classified_genre) %>%
  mutate(count=n()) %>% 
  ggplot() +
  geom_smooth(mapping = aes(x = year, y = count, color = classified_genre), method = loess)




#read in pre-classified clusters

allDataGenres = read_csv("output/genreClass-10-30-16.csv")
#This should be Priamry.Genre in most cases
names(allDataGenres)[names(allDataGenres)=="classified_genre"] <- "genre"
names(allDataGenres)[names(allDataGenres)=="Text"] <- "text"
allDataGenres = allDataGenres[,c("cluster","text","genre")]
allDataGenres$cluster = as.character(allDataGenres$cluster)

# get equal amount of each
allDataGenres <- ddply(allDataGenres, "genre" , function(allDataGenres) allDataGenres[sample(nrow(allDataGenres), 750),])

#Merge hand-tagged clusters into alldata
allData = allParts_short %>% full_join(allDataGenres)
allData = allData[,c("cluster","text.x","genre.y")]
names(allData)[names(allData)=="genre.y"] <- "genre"
names(allData)[names(allData)=="text.x"] <- "text"
#allData <- allData[sample(1:nrow(allData), 2000,replace=FALSE),]
allData = replace(allData, is.na(allData), "unknown") 

# just prose from classication

poetry <- genreClass %>% filter(classified_genre=="poetry")
prose <- genreClass %>% filter(classified_genre=="prose")
ads <- genreClass %>% filter(classified_genre=="advertisement")
news <- genreClass %>% filter(classified_genre=="news")

#create SQL backend (not working)


db <- dbConnect(SQLite(), dbname="vt.sqlite")
dbListTables(db)
dbWriteTable(db, "poetry", poetry)



#for David, getting the dates of the first hand tagged genres
handTagged = read_csv("~/Documents/Northeastern/Viral Texts Project/Viral-Texts-R/data/newdata-5-11-handtagged-genres.csv")
handTaggedJoin = handTagged %>% inner_join(beginData, by = "cluster")
write.csv(handTaggedJoin, file = paste('output/200handTaggedwDates-3-29-17.csv',sep=""))

handTaggedJoin %>%
  mutate(year=gsub(".*(\\d{4}).*","\\1",date)) %>%
  mutate("year" = as.numeric(year)) %>%
  mutate("genre" = `Top Level`) %>%
  group_by(year,genre) %>%
  mutate(count=n()) %>% 
  ggplot() +
  geom_line(aes(x = year, y = count, color = genre))
