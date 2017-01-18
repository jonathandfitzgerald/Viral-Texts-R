justPoems = read.csv("output/justPoems-5-23-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
justPoems <- justPoems[sample(1:nrow(justPoems), 200,replace=FALSE),]
justPoems = justPoems[,c("cluster","classified_genre","Text")]
names(justPoems)[names(justPoems)=="classified_genre"] <- "genre"
names(justPoems)[names(justPoems)=="Text"] <- "text"


justAds = read.csv("output/justAds-5-23-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
justAds <- justAds[sample(1:nrow(justAds), 200,replace=FALSE),]
justAds = justAds[,c("cluster","genre","text")]

justNews = read.csv("output/justNews-5-23-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
justNews <- justNews[sample(1:nrow(justNews), 200,replace=FALSE),]
justNews = justNews[,c("cluster","classified_genre","Text")]
names(justNews)[names(justNews)=="classified_genre"] <- "genre"
names(justNews)[names(justNews)=="Text"] <- "text"

justVignettes = read.csv("data/200vignettes.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)

sixHundredClusters = rbind(justPoems,justAds,justNews,justVignettes)
sixHundredClusters = sixHundredClusters %>% filter(cluster!=" ")
sixHundredClusters$cluster = as.character(sixHundredClusters$cluster)


#Merge hand-tagged clusters into alldata
allData = newData %>% left_join(sixHundredClusters, by="cluster")
allData = allData[,c("cluster","text.x","genre")]
names(allData)[names(allData)=="text.x"] <- "text"
allData = replace(allData, is.na(allData), "unknown") 
