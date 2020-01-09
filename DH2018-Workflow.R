library(plotly)
library(viridis)
library(RColorBrewer)

colors <- brewer.pal(n = 4, name = "Spectral")

l <- list(
  font = list(
    family = "sans-serif",
    size = 24,
    color = "#000"),
  bgcolor = "#fff",
  size = 44,
  bordercolor = "#efefef",
  borderwidth = 2)

# Use ClusterAnalysis-New.R for reading in clusters and hand-tagged genres.

#PCA Code for TOPICS (This assumes topic modelling has been completed in Topic Modelling Generic.r)

#UNCLASSIFIED TEXTS FIRST 

#Create a dataframe with all topics for clusters that have known genres
unClassifiedTopicsDF = doc.topics %>% 
  as.data.frame() %>% 
  mutate(cluster = input$Cluster, primary_genre = input$Genre) 

#Convert to a matrix
modeling_matrix = unClassifiedTopicsDF %>% select(-primary_genre, -cluster)
modeling_matrix = log(modeling_matrix + .5)

#PCA
model = prcomp(modeling_matrix, scale = TRUE)
prediction = predict(model)
prediction = prediction %>% 
  as.data.frame %>% 
  mutate(cluster=unClassifiedTopicsDF$cluster %>% 
           as.character, genre=unClassifiedTopicsDF$primary_genre %>% 
           as.character) %>% 
  select(cluster,genre,PC1,PC2,PC3)


#vtData$cluster <- vtData$cluster %>% as.character()
prediction <- prediction %>% inner_join(allData, by = "cluster" )
prediction <- prediction[,c("cluster","text","genre.x","PC1","PC2","PC3")]
prediction$genre <- prediction$genre.x 
prediction$genre.x <- NULL

#replace poetry/prose
# prediction <- prediction %>% mutate_if(is.character, str_replace_all, pattern = 'poetry', replacement = 'literary')
# prediction <- prediction %>% mutate_if(is.character, str_replace_all, pattern = 'prose', replacement = 'informational')
# prediction <- prediction %>% mutate_if(is.character, str_replace_all, pattern = 'advertisement', replacement = 'ads')

#Visualize with plotly
unClassPCAViz <- plot_ly(prediction, x = ~PC1, y = ~PC2, z = ~PC3, color = ~genre, symbol = ~genre, symbols = c("cross","circle","square","diamond","circle-open"), opacity = 1, colors = colors, marker = list(size = 6)) %>%
  add_markers() %>%
  layout(legend = l, title ="Principal Component Analysis of Viral Texts Clusters", scene = list(xaxis = list(title = 'PC1'),
                                                                                                 yaxis = list(title = 'PC2'),
                                                                                                 zaxis = list(title = 'PC3')))

unClassPCAViz

# Export to large SVG

library(htmlwidgets)

# Save viewer settings (e.g. RStudio viewer pane)
op <- options()

# Set viewer to web browser
options(viewer = NULL)

# Use web browser to save image
unClassPCAViz %>% htmlwidgets::onRender(
  "function(el, x) {
  var gd = document.getElementById(el.id); 
  Plotly.downloadImage(gd, {format: 'svg', width: 2400, height: 1600, filename: 'pca_noads'});
  }"
)

# Restore viewer to old setting (e.g. RStudio)
options(viewer = op$viewer)


#CLASSIFIED TEXTS FIRST 

#Topic model genreClass first!

#Create a dataframe with all topics for clusters that have known genres
classifiedTopicsDF = doc.topics %>% 
  as.data.frame() %>% 
  mutate(cluster = input$Cluster, primary_genre = input$Genre) 

#Convert to a matrix
modeling_matrix = classifiedTopicsDF %>% select(-primary_genre, -cluster)
modeling_matrix = log(modeling_matrix + .5)

#PCA
model = prcomp(modeling_matrix, scale = TRUE)
prediction = predict(model)
prediction = prediction %>% 
  as.data.frame %>% 
  mutate(cluster=classifiedTopicsDF$cluster %>% 
           as.character, genre=classifiedTopicsDF$primary_genre %>% 
           as.character) %>% 
  select(cluster,genre,PC1,PC2,PC3)


#vtData$cluster <- vtData$cluster %>% as.character()
prediction <- prediction %>% inner_join(allData, by = "cluster" )
prediction <- prediction[,c("cluster","text","genre.x","PC1","PC2","PC3")]
prediction$genre <- prediction$genre.x 
prediction$genre.x <- NULL

#Remove ads
#prediction <- prediction %>% filter(genre != "ads")

#Visualize with plotly

classPCAViz <- plot_ly(prediction, x = ~PC1, y = ~PC2, z = ~PC3, color = ~genre, symbol = ~genre, symbols = c("cross","circle","square","diamond"), opacity = 1, text = ~text, colors = colors, marker = list(size = 6)) %>%
  add_markers() %>%
  layout(legend = l, title ="Principal Component Analysis of Viral Texts Clusters", scene = list(xaxis = list(title = 'PC1'),
                                  yaxis = list(title = 'PC2'),
                                  zaxis = list(title = 'PC3')))


classPCAViz 
options(viewer=NULL)

# Export to large SVG

library(htmlwidgets)

# Save viewer settings (e.g. RStudio viewer pane)
op <- options()

# Set viewer to web browser
options(viewer = NULL)

# Use web browser to save image
classPCAViz %>% htmlwidgets::onRender(
  "function(el, x) {
  var gd = document.getElementById(el.id); 
  Plotly.downloadImage(gd, {format: 'svg', width: 2400, height: 1600, filename: 'pca_noads'});
  }"
)

# Restore viewer to old setting (e.g. RStudio)
options(viewer = op$viewer)