# Read in multiple CSVs from a folder
path2file <- "data/8-3-17-download"
fileList <- list.files(path2file,full.names = TRUE) # Create a list of files in a folder

newData <- lapply(fileList, read_csv) %>% 
  bind_rows() %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1) %>%
  as_data_frame()

# Select just cluster and text columns and add genre = unknown column
newData_slim <- newData[,c("cluster","text")] %>% mutate(genre="unknown")
newData_slim$cluster <- as.character(newData_slim$cluster)

#Read in hand-tagged genres
informational <- read_csv("data/8-8-17-hand-tagged/informational.csv")
literary <- read_csv("data/8-8-17-hand-tagged/literary.csv")
news <- read_csv("data/8-8-17-hand-tagged/news.csv")
ads <- read_csv("data/8-8-17-hand-tagged/ads.csv")
poetry <- read_csv("data/8-8-17-hand-tagged/poetry.csv")

#Add poetry into literary as poetry is not classifying well
poetry$genre <- poetry$genre %>% gsub("poetry","literary")
literary <- rbind(literary,poetry) 

newGenres <- rbind(informational,literary,news,ads)
newGenres$X1 <- NULL
newGenres <- newGenres %>% mutate(cluster = as.character(cluster))

#Select 50 of each
newGenres_slim <- newGenres %>%
  group_by(genre) %>%
  slice(1:50) %>%
  as_data_frame()

# Merge in classified clusters
allData <- rbind(newData_slim, newGenres_slim)

#Visualize clusters by year
newData %>%
  mutate(year=gsub(".*(\\d{4}).*","\\1",date)) %>%
  mutate("year" = as.numeric(year)) %>%
  group_by(year) %>%
  mutate(count=n()) %>% 
  ungroup %>%
  ggplot() +
  geom_bar() +
  aes(x=year) + 
  ggtitle("Number of clusters by year in newData")


#Poems for Ryan
poems4ryan <- read_csv("data/poems4ryan-8-8-17.csv")
poems4ryan_joined <- inner_join(poems4ryan,newData, by = "cluster")
poems4ryan_joined <- poems4ryan_joined %>%
  filter(p1x != "NA") %>%
  group_by(cluster) %>%
  slice(1)


write.csv(poems4ryan_joined, file = paste('output/poems4ryan-8-8-17.csv',sep=""))
