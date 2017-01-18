if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr)

#getting most/least reprinted
part00000 = part00000[,c("cluster","text")] 
part00000 = part00000 %>% group_by(cluster) %>% slice(1)
part00000 = part00000 %>% mutate(group = "popular")

part00009 = part00009[,c("cluster","text")] 
part00009 = part00009 %>% group_by(cluster) %>% slice(1)
part00009 = part00009[1:468,]
part00009 = part00009 %>% mutate(group = "unpopular")

twoParts = rbind(part00000, part00009)

sentimentTest = twoParts
(out <- with(sentimentTest, sentiment_by(text, list(group))))

plot(out)
plot(uncombine(out))


sentimentTest = allVignettes[198,2]
out = sentiment(sentimentTest)

plot(out)
plot(uncombine(out))


sentimentTest <- c(
  "I haven't been sad in a long time.",
  "I am extremely happy today.",
  "It's a good day.",
  "But suddenly I'm only a little bit happy.",
  "Then I'm not happy at all.",
  "In fact, I am now the least happy person on the planet.",
  "There is no happiness left in me.",
  "Wait, it's returned!",
  "I don't feel so bad after all!"
)


#syuzhet
install.packages("syuzhet")
install.packages("pander")
library(syuzhet)
library(pander)

acc = allVignettes[2,2]

acc = get_text_as_string("~/Desktop/utc.txt") %>% as.character()
class(acc)

acc_sent <- get_sentences(acc)
head(acc_sent)
acc_sent = as.character(acc_sent)

sentiment_vector <- get_sentiment(acc_sent, method="bing")
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

plot(
  sentiment_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)


percent_vals <- get_percentage_values(sentiment_vector)
plot(
  percent_vals, 
  type="l", 
  main="TEWWG Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 6, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="TEWWG using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

nrc_data <- get_nrc_sentiment(acc_sent)

angry_items <- which(nrc_data$anger > 0)
acc_sent[angry_items]

joy_items <- which(nrc_data$joy > 0)
acc_sent[joy_items]

pander::pandoc.table(nrc_data[, 1:8])

barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in TEWWG", xlab="Percentage"
)
