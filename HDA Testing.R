poemswithBY = beginData[grep("\nBY [A-Z]", beginData$text),] %>% as.data.frame()

grep("true philosophy", ignore.case=TRUE, beginData$text)

beginData[218915,beginDtext]

test = ESQvignettes[,c("cluster", "title", "date")]

gathertest = gather(test, type, value, -title)

spreadtest = gathertest %>% group_by(title)

spreadtest = spread(spreadtest, title, value, fill = NA, drop = TRUE )

rm(gathertest)

#subset
ESQvignettes = subset(beginData, cluster=="321785")
ESQvignettes <- ESQvignettes %>% rbind(subset(beginData, cluster=="71247"))
write.csv(ESQvignettes, file = paste('output/ESQvignettes-1-24-17.csv',sep=""))


"Regularly Sold" 321785
"Religious Courtship" 422907
"The Secret" 355594
"In a Compartment Car" 723792
"True Philosophy - The Farmers" 71247

head(mtcars)
mtcars$car <- rownames(mtcars)
mtcars <- mtcars[, c(12, 1:11)]
mtcarsNew <- mtcars %>% gather(attribute, value, -car)
mtcarsSpread <- mtcarsNew %>% spread(attribute, value)
head(mtcarsSpread)
