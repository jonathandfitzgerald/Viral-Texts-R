#load babynames as tibble
library(babynames)
names <- as_data_frame(babynames) %>% filter(year <= 1899) %>% group_by(name) %>% slice(1)
nameList <- names$name
nameList <- nameList %>% as.list()

# Search through All Vignettes for any that contain names
vigNames <- sapply(nameList, grepl, allVignettes$text) %>% as_data_frame()

# Select only rows with True
vigNames <- which(apply(vigNames, 1, any))

# Filter All Vignettes to include only rows with names
vigNames <- allVignettes[vigNames, ]

# Write to CSV
write.csv(vigNames, file = paste('output/vigNames-5-22-17.csv',sep=""))


ryanClusters = c(1190509, 1200758, 1529575, 154937, 1804493, 1852661, 186033, 1904994, 1963051, 232025, 2730023)
vigNamesSlim <- vigNames %>% filter(cluster %in% ryanClusters )


vigNamesSlim <- vigNamesSlim %>% inner_join(beginData, by = "cluster")
write.csv(vigNamesSlim, file = paste('output/vigNamesSlim-5-23-17.csv',sep=""))
