#Prepare data for Topic Modelling

#Get the Cluster Number and text column from Cluster frame 

#put the dataframe for topic modelling here, and don't change code below

#I removed numbers from stopwords.txt

dataTM = bigClustersClass %>% filter(genre == "prose")

clustersForTM = dataTM[,c("cluster","text","genre","date")] %>% filter(nchar(as.character(text), allowNA=TRUE) > 50) 
clustersForTM = data.frame(Cluster=as.character(dataTM$cluster),Text=as.character(dataTM$text),Genre=as.character(dataTM$genre),Date=as.character(dataTM$date),stringsAsFactors = F)

input=clustersForTM

n.topics=100
SEED = 1789

#Don't change below...

mallet.instances <- mallet.import(input$Cluster, 
                                  input$Text, 
                                  stoplist.file="data/stopwords.txt", 
                                  token.regexp = "\\w+",preserve.case=F)


topic.model <- MalletLDA(num.topics=n.topics)
#topic.model$model$setRandomSeed(as.integer(SEED))
topic.model$loadDocuments(mallet.instances)

#Look at the word frequencies sorted in order.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)

#Some preferences. Inside baseball: see Wallach and Mimno for what's going on.
topic.model$setAlphaOptimization(20, 50)
topic.model$train(300)
#Increase the fit without changing the topic distribution; optional
topic.model$maximize(10)

#Gets a list of the documents and topics
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
#Changes the orientation of that matrix to be horizontal:
topic.docs <- t(doc.topics)

#Gets a list of the top words.
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)


#Assign some labels to the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) {
  topics.labels[topic] <- paste(
    mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" "
  )}
topics.labels
#to look at the labels, type "topics.labels"

rownames(doc.topics) = input$Cluster
colnames(doc.topics) = topics.labels

# Visualize topics over time?

topicDF <- doc.topics %>%
  as_data_frame() %>%
  mutate(Cluster = rownames(doc.topics)) %>%
  gather(topic, proportion, -Cluster)
View(topicDF)


topicDF <- topicDF %>%
  inner_join(clustersForTM)

topicDF <- topicDF %>%
  mutate(year=gsub(".*(\\d{4}).*","\\1",Date)) %>%
  mutate("year" = as.numeric(year))

#all topics
topicDF %>%
  group_by(year) %>%
  ggplot() + 
  geom_smooth(method="loess", se=FALSE) +
  aes(x=year,y=proportion) + 
  facet_wrap(facets = "topic", scales="free")

#particular topic
topicDF %>%
  filter(topic == "majesty tile lord queen emperor") %>%
  group_by(year) %>%
  ggplot() + 
  geom_line(colour="blue") + 
  aes(x=year,y=proportion) + 
  geom_point(size=2,color="blue") + 
  geom_smooth(method="loess")


#particular topics (plural)
sentimentalTopics2viz = c("heart character mind life human",
                          "boy father mother children school",
                          "maid women shillings cents llio",
                          "wife woman lady young husband",
                          "man good day men make",
                          "catholic schools things children johnny")

sentimentalTopics <- topicDF %>%
  filter(topic == sentimentalTopics2viz) %>%
  filter(year >= 1840) %>%
  group_by(year) %>%
  ggplot() + 
  geom_smooth(method="loess", se=FALSE) +
  aes(x=year,y=proportion,color=topic) + 
  facet_wrap(facets = "topic", scales = "free")

ggplotly(sentimentalTopics)

reformTopics2viz = c("free society hear nichols shepard",
                     "drink wine beer glass drunk",
                     "south slavery north southern war",
                     "web mistake brandy murphy fish",
                     "means true result duty pro",
                     "bill senator tariff cent duty")

reformTopics <- topicDF %>%
  filter(topic == reformTopics2viz) %>%
  filter(year >= 1840) %>%
  group_by(year) %>%
  ggplot() + 
  geom_smooth(method="loess", se=FALSE) +
  aes(x=year,y=proportion,color=topic) + 
  facet_wrap(facets = "topic", scales = "free")

ggplotly(reformTopics)
