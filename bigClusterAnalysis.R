
#massive combined file

allParts = read_csv("~/Documents/Northeastern/Viral Texts Project/Viral-Texts-R/data/combined_short.csv")

#select longest witness of each cluster
allParts = allParts %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1)

allParts <- allParts[,c("cluster","text")] %>% mutate(genre = "unknown")
allParts$cluster <- as.character(allParts$cluster)

allParts_short <- allParts[sample(nrow(allParts), 15000),]


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
