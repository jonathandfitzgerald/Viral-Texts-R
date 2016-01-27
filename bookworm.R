#Attempting to work with Bookworm
library(bookworm)
install.packages("devtools")
library(devtools)
install_github("bmschmidt/Bookworm-Mallet")
?bookworm


totals = bookworm(host="localhost", port = 8005, search_limits=list(),
                  groups="date_year","counttype"=list("TotalTexts"),
                  database="viraltexts_bookworm")

plot(totals[totals$date_year %in% 1700:2000,],type='l',main="Total works in the Hathi Trust Public Domain corpus")

totals = bookworm(host="localhost", port = 8005, search_limits=list(),
                  groups="date_year","counttype"=list("WordCount","TextCount"),
                  database="viraltexts_bookworm")

totals=totals[totals$date_year %in% 1700:2000,]
plot(totals$date_year,totals$WordCount/totals$TextCount,type='l',main="Average length of works in the Hathi Trust Public Domain corpus")

results = bookworm(host="localhost", port = 8005, search_limits=list("word" = list("war","peace")),
                   groups="date_year","counttype"=list("WordsPerMillion"),
                   database="viraltexts_bookworm")

vtBookworm = bookworm(host="localhost", port = 8005, search_limits=list(), "query" = list("text"),
                      groups="cluster","counttype"=list("TotalTexts"),
                      database="viraltexts_bookworm")




plot(results[results$date_year %in% 1700:2000,],main="Usage of 'evolution' in the Hathi Trust Public Domain corpus")