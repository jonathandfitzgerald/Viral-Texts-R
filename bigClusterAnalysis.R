getwd()

#individual files

files <- dir("./")

allParts <- do.call(rbind,lapply(files,read.csv)) %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1)

#massive combined file

allParts = read_csv("~/Documents/Viral-Texts-R/data/2-15-17-download-plus/combined_short.csv")

allParts = read.csv("data/2-15-17-download/combined_short.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)

#select longest witness of each cluster
allParts = allParts %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1)

allParts <- allParts[,c("cluster","text")] %>% mutate(genre = "unknown")

#read in pre-classified clusters

allDataGenres = read_csv("data/genreClass-10-30-16.csv")
#This should be Priamry.Genre in most cases
names(allDataGenres)[names(allDataGenres)=="classified_genre"] <- "genre"
names(allDataGenres)[names(allDataGenres)=="Text"] <- "text"
allDataGenres = allDataGenres[,c("cluster","text","genre")]
allDataGenres$cluster = as.character(allDataGenres$cluster)

