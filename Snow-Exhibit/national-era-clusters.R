neClusters <- read_csv("Snow-Exhibit/neclusters.csv")
neClusters$X1 <- NULL
neClusters$cluster <- as.character(neClusters$cluster)

snowFiles <- list.files(path = "Snow-Exhibit/data/")
allSnowClusters <- 
  do.call(rbind, lapply(snowFiles, read_csv))

allSnowClusters$cluster <- as.character(allSnowClusters$cluster)

allNEClusters <- left_join(neClusters, allSnowClusters, by = "cluster")


test <- allNEClusters %>% group_by(cluster) %>% slice(1)

write_csv(allNEClusters, "Snow-Exhibit/ne-clusters.csv")
