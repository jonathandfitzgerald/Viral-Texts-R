
library(plotly)
library(viridis)

l <- list(
  font = list(
    family = "sans-serif",
    size = 18,
    color = "#000"),
  bgcolor = "#fff",
  size = 24,
  bordercolor = "#FFFFFF",
  borderwidth = 2)



 
p <- plot_ly(prediction, x = ~PC1, y = ~PC2, z = ~PC3, color = ~genre.x, opacity = .5, text = ~text, colors = viridis_pal(option = "C")(5), marker = list(size = 4)) %>%
  add_markers() %>%
  layout(legend = l, scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))


p


# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="vt-pca-3d-unknown")
chart_link


informational <- prediction %>%
  filter(PC1>=-0.2, PC1<=0, PC2>=-0.1, PC2<=0.25, PC3>=-0.2, PC3<=0.2)
informational <- informational[,c("cluster","text")]
informational <- informational %>% mutate(genre = "informational")

literary <- prediction %>%
  filter(PC1<=0.2, PC2<=-0.3, PC3<=0.5)
literary <- literary[,c("cluster","text")]
literary <- literary %>% mutate(genre = "literary")

# Got poetry from PCA of literary
poetry <- prediction %>%
  filter(PC1<=-0.3, PC2<=-0.45, PC3>=-0.75)
poetry <- poetry[,c("cluster","text")]
poetry <- poetry %>% mutate(genre = "poetry")

# recreate literary to exclude poetry
#literary <- anti_join(literary, poetry, by = "cluster")

news <- prediction %>%
  filter(PC1<=0, PC1>=-0.25, PC2>=0.5, PC3>=0, PC3<=0.15)
news <- news[,c("cluster","text")]
news <- news %>% mutate(genre = "news")

#commerce <- prediction %>%
#  filter(classified_genre=="prose",PC1<=0.2, PC2<0, PC3<0)
#commerce <- commerce[,c("cluster","text")]
#commerce <- commerce %>% mutate(genre = "news")

ads <- prediction %>%
  filter(PC1>=0.6, PC2>=-0.2, PC2<=0.1, PC3>=-0.1, PC3<=0.1)
ads <- ads[,c("cluster","text")]
ads <- ads %>% mutate(genre = "ads")

#news <- rbind(commerce,news)

informational <- informational[sample(1:nrow(informational), 200,replace=FALSE),]
literary <- literary[sample(1:nrow(literary), 200,replace=FALSE),]
news <- news[sample(1:nrow(news), 200,replace=FALSE),]
ads <- ads[sample(1:nrow(ads), 200,replace=FALSE),]
poetry <- poetry[sample(1:nrow(poetry), 200,replace=FALSE),]
#commerce <- commerce[sample(1:nrow(ads), 200,replace=FALSE),]


newGenres <- rbind(informational,literary,news,ads,poetry)
newGenres$cluster <- newGenres$cluster %>% as.character()
write.csv(newGenres, file = paste('output/newGenres-8-2-17.csv',sep=""))

write.csv(informational, file = paste('output/genres/informational.csv',sep=""))
write.csv(literary, file = paste('output/genres/literary.csv',sep=""))
write.csv(news, file = paste('output/genres/news.csv',sep=""))
write.csv(ads, file = paste('output/genres/ads.csv',sep=""))
write.csv(poetry, file = paste('output/genres/poetry.csv',sep=""))

#Read in classified and cleaned poetry
poetryIn <- read.csv("output/PoetryClass-7-29-17-edited.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
poetryIn$X <- NULL

#Inner join with All clusters
poems4ryan <- inner_join(poetryIn,newData)
write.csv(poems4ryan, file = paste('output/poems4ryan-7-29-17.csv',sep=""))
  
#Read back in newGenres
informational <- read_csv("output/genres/informational.csv")
literary <- read_csv("output/genres/literary.csv")
news <- read_csv("output/genres/news.csv")
ads <- read_csv("output/genres/ads.csv")
poetry <- read_csv("output/genres/poetry.csv")
newGenres <- rbind(informational,literary,news,ads)
newGenres$X1 <- NULL

#Just literary classification
poetry <- prediction %>%
  filter(PC1>=0.4)

poetry_slim <- poetry[grep("[A-Z]{3,}", poetry$text, ignore.case=F),]
poetry_slim <- poetry_slim %>% mutate(genre = gsub("literary", "poetry", poetry_slim$genre))
poetry_clusters <- poetry_slim$cluster
poetry_slim = poetry_slim[,c("cluster","genre","text")]

literaryProse <- literaryClass[!(literaryClass$cluster %in% poetry_clusters),]
literaryProse <- literaryProse %>% ungroup() %>% mutate(genre = gsub("literary", "prose", literaryProse$genre))
literaryProse <- literaryProse[sample(1:nrow(literaryProse), 141,replace=FALSE),]
literaryProse = literaryProse[,c("cluster","genre","text")]

poetryANDprose <- rbind(poetry_slim,literaryProse)

literaryClass <- literaryClass %>% as_data_frame()
literaryClass = literaryClass[,c("cluster","genre","text")]
literaryNew <- rbind(poetryANDprose,literaryClass)
literaryNew <- literaryNew %>% ungroup() %>% mutate(genre = gsub("literary", "unknown", literaryNew$genre))
