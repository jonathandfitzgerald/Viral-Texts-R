
shortstories <- scan(file = 'data/shortstories.txt', blank.lines.skip = TRUE, what = "character", sep = "\n") %>% as.data.frame()
shortstories = shortstories[1:1000,] %>% as.data.frame()
names(shortstories)[names(shortstories)=="."] <- "text"
shortstories = shortstories %>% mutate("cluster" = rep(c(1:200),each=5))
shortstories = shortstories %>% mutate("genre" = "fiction")
shortstories = shortstories[,c("cluster","text","genre")]


shortstories2 = shortstories %>% group_by(cluster) %>% spread(cluster,text)

shortstories = shortstories %>% group_by(cluster) %>% mutate(text2 = paste(text, sep = " ")) %>% slice(1)
write.csv(shortstories, file = paste('output/shortstories-7-28-16.csv',sep=""))

