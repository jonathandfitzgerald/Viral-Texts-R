# READ IN AND CLEAN DATA

# vtData = read_csv("~/Documents/Northeastern/Viral Texts Project/Viral-Texts-R/data/combined_short.csv")
# vtDataDates = read_csv("~/Documents/Northeastern/Viral Texts Project/Viral-Texts-R/data/combined_short2.csv")
# vtDataJoined <- inner_join(vtData,vtDataDates, by = "cluster")
# vtDataJoined$cluster <- as.character(vtDataJoined$cluster)

vtData = newData_slim
vtData$cluster <- as.character(vtData$cluster)
vtData <- vtData %>% inner_join(genreClass)
vtData$probability <- NULL

vtData = vtData %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1)

vtData <- vtData %>%
  mutate(year=gsub(".*(\\d{4}).*","\\1",date)) %>%
  mutate("year" = as.numeric(year))

vtData$count = NULL

# vtData %>%
#   group_by(year) %>%
#   mutate(count=n()) %>% 
#   ggplot() +
#   geom_bar(aes(x=year)) + 
#   ggtitle("Number of clusters by year in newData")

# Filter Years
vtData50 <- vtData %>%  filter(year < 1850)
vtData60 <- vtData %>%  filter(year > 1851, year < 1860)
vtData70 <- vtData %>%  filter(year > 1861, year < 1870)
vtData80 <- vtData %>%  filter(year > 1871, year < 1880)
vtData90 <- vtData %>%  filter(year > 1881, year < 1890)
vtData00 <- vtData %>%  filter(year > 1891, year < 1900)

vtData50_short <- vtData50[sample(nrow(vtData50), 500),]
vtData60_short <- vtData60[sample(nrow(vtData60), 500),]
vtData70_short <- vtData70[sample(nrow(vtData70), 500),]
vtData80_short <- vtData80[sample(nrow(vtData80), 500),]
vtData90_short <- vtData90[sample(nrow(vtData90), 500),]
vtData00_short <- vtData00[sample(nrow(vtData00), 500),]

#vtData00$text %>% write_lines('data/vtTest00.txt')


# Write txt files and prepare for word2vec
vtData$text %>% write_lines('data/vtData_text_pre.txt')
vtData50$text %>% write_lines('data/vtData_text50_pre.txt')
vtData60$text %>% write_lines('data/vtData_text60_pre.txt')
vtData70$text %>% write_lines('data/vtData_text70_pre.txt')
vtData80$text %>% write_lines('data/vtData_text80_pre.txt')
vtData90$text %>% write_lines('data/vtData_text90_pre.txt')
vtData00$text %>% write_lines('data/vtData_text00_pre.txt')

prep_word2vec(origin="data/vtData_text_pre.txt",destination="data/vtData_text.txt",lowercase=T,bundle_ngrams=1)
prep_word2vec(origin="data/vtData_text50_pre.txt",destination="data/vtData_text50.txt",lowercase=T,bundle_ngrams=1)
prep_word2vec(origin="data/vtData_text60_pre.txt",destination="data/vtData_text60.txt",lowercase=T,bundle_ngrams=1)
prep_word2vec(origin="data/vtData_text70_pre.txt",destination="data/vtData_text70.txt",lowercase=T,bundle_ngrams=1)
prep_word2vec(origin="data/vtData_text80_pre.txt",destination="data/vtData_text80.txt",lowercase=T,bundle_ngrams=1)
prep_word2vec(origin="data/vtData_text90_pre.txt",destination="data/vtData_text90.txt",lowercase=T,bundle_ngrams=1)
prep_word2vec(origin="data/vtData_text00_pre.txt",destination="data/vtData_text00.txt",lowercase=T,bundle_ngrams=1)



# CREATE MODEL

THREADS <- 3

if (!file.exists('data/vtData_vectors.bin')) {
  vtData_model <- train_word2vec(
    'data/vtData_text.txt',
    output_file='data/vtData_vectors.bin',
    vectors=100,
    threads=THREADS,
    window=12, iter=10, negative_samples=0
  )
} else {
  vtData_model <- read.vectors('data/vtData_vectors.bin')
}

if (!file.exists('data/vtData50_vectors.bin')) {
  vtData_model50 <- train_word2vec(
    'data/vtData_text50.txt',
    output_file='data/vtData50_vectors.bin',
    vectors=100,
    threads=THREADS,
    window=12, iter=10, negative_samples=0
  )
} else {
  vtData_model50 <- read.vectors('data/vtData50_vectors.bin')
}

 if (!file.exists('data/vtData60_vectors.bin')) {
   vtData_model60 <- train_word2vec(
     'data/vtData_text60.txt',
     output_file='data/vtData60_vectors.bin',
     vectors=100,
     threads=THREADS,
     window=12, iter=10, negative_samples=0
   )
 } else {
   vtData_model60 <- read.vectors('data/vtData60_vectors.bin')
 }


 if (!file.exists('data/vtData70_vectors.bin')) {
   vtData_model70 <- train_word2vec(
     'data/vtData_text70.txt',
     output_file='data/vtData70_vectors.bin',
     vectors=100,
     threads=THREADS,
     window=12, iter=10, negative_samples=0
   )
 } else {
   vtData_model70 <- read.vectors('data/vtData70_vectors.bin')
 }


 if (!file.exists('data/vtData80_vectors.bin')) {
   vtData_model80 <- train_word2vec(
     'data/vtData_text80.txt',
     output_file='data/vtData80_vectors.bin',
     vectors=100,
     threads=THREADS,
     window=12, iter=10, negative_samples=0
   )
 } else {
   vtData_model80 <- read.vectors('data/vtData80_vectors.bin')
 }


 if (!file.exists('data/vtData90_vectors.bin')) {
   vtData_model90 <- train_word2vec(
     'data/vtData_text90.txt',
     output_file='data/vtData90_vectors.bin',
     vectors=100,
     threads=THREADS,
     window=12, iter=10, negative_samples=0
   )
 } else {
   vtData_model90 <- read.vectors('data/vtData90_vectors.bin')
 }


 if (!file.exists('data/vtData00_vectors.bin')) {
   vtData_model00 <- train_word2vec(
     'data/vtData_text00.txt',
     output_file='data/vtData00_vectors.bin',
     vectors=100,
     threads=THREADS,
     window=12, iter=10, negative_samples=0
   )
 } else {
   vtData_model00 <- read.vectors('data/vtData00_vectors.bin')
 }

# General searches
vtData_model %>% closest_to("baking", 30)


# Results for "wvSearch"
wvSearch <- "woman"
women50 <- vtData_model50 %>% closest_to(wvSearch, 30) 
women60 <- vtData_model60 %>% closest_to(wvSearch, 30)
women70 <- vtData_model70 %>% closest_to(wvSearch, 30)
women80 <- vtData_model80 %>% closest_to(wvSearch, 30)
women90 <- vtData_model90 %>% closest_to(wvSearch, 30)
women00 <- vtData_model00 %>% closest_to(wvSearch, 30)

# Combine results into dataframe
allWomen <- women50$word %>% as_data_frame()
allWomen$word60=women60$word
allWomen$word70=women70$word
allWomen$word80=women80$word
allWomen$word90=women90$word
allWomen$word00=women00$word
colnames(allWomen)[colnames(allWomen)=="value"] <- "women50"

# Write csv
allWomen %>% write_csv("output/allWomenWords.csv")


# VISUALIZE

vtData_model00 %>% plot(perplexity=5)


vtData_model00 %>% closest_to(~'woman'+'mother',20) %>%
  View()


# ANALOGY

vtData_model00 %>% closest_to(~'man'-'boy'+'girl',20) %>%
  View()

