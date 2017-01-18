#Read in cluster data from server (unzipped to show only first example of each cluster, and broken up as follows)
clusterGroup100000 = read.delim("data/1-100000-cluster.idtab.tsv", header=FALSE, fill = TRUE, quote = "", row.names = NULL, stringsAsFactors = FALSE)

#Read in cluster selection 1-1000
thousandClusters = read.delim("data/clusters1-1000.idtab.tsv", header=FALSE, fill = TRUE, quote = "", row.names = NULL, stringsAsFactors = FALSE)

#Combine all clusters
allClusters = rbind(clusterGroup100000,clusterGroup200000,clusterGroup300000,clusterGroup400000,clusterGroup500000,clusterGroup700000,clusterGroup900000,clusterGroup1100000,clusterGroup1300000,clusterGroup1500000,clusterGroup1800000)

#Select subset of data frame
halfCluster <- clusterGroup100000[1:5000,]
write.csv(halfCluster, file = paste('output/clusterGroup1-50000.csv',sep=""))

fiveHundredClusters = clusterGroup100000[1:500,]
write.csv(fiveHundredClusters, file = paste('output/fiveHundredClusters.csv',sep=""))

clusterGroup100000[97937,]

#Read in hand-tagged genres from halfCluster
halfClusterGenres = read.csv("data/fiveHundredClusters-genre.csv", header=TRUE, fill = TRUE, sep = ",", quote = "", row.names = NULL, stringsAsFactors = FALSE)


#Read in clusters tagged by classifier, round 1 --  bind with halfClusterGenres
newGenreClusters = read.csv("data/genreClass-4-13-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
halfClusterGenres = newGenreClusters
halfClusterGenres = halfClusterGenres[,c("cluster","Text","classified_genre")]
names(halfClusterGenres)[names(halfClusterGenres)=="Text"] <- "text"
names(halfClusterGenres)[names(halfClusterGenres)=="classified_genre"] <- "genre"
halfClusterGenres$cluster = as.character(halfClusterGenres$cluster)

#Merge tagged clusters with the rest of the data, mark those without genres as Unknown
halfCluster = halfCluster %>% mutate(Cluster.Number = V1)
withGenres = halfCluster %>% left_join(halfClusterGenres)
withGenres = replace(withGenres, is.na(withGenres), "Unknown") 
withGenres = withGenres[,c("V1","V9","Genre.1")]
withGenres = withGenres %>% mutate(cluster=V1, text=V9, genre=Genre.1)
withGenres = withGenres[,c("cluster","text","genre")]