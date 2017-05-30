
library(plotly)



p <- plot_ly(prediction, x = ~PC1, y = ~PC2, z = ~PC3, color = ~genre, opacity = .5, text = ~cluster, marker = list(size = 6)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

p

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="vt-pca-3d")
chart_link


