# READ IN AND CLEAN DATA

vtData = read_csv("~/Documents/Northeastern/Viral Texts Project/Viral-Texts-R/data/combined_short2.csv")
#vtData = dataTM

vtData = vtData %>% 
  group_by(cluster) %>% 
  mutate(count=nchar(text)) %>%
  arrange(-count) %>% 
  slice(1)

vtData <- vtData %>%
  mutate(year=gsub(".*(\\d{4}).*","\\1",date)) %>%
  mutate("year" = as.numeric(year))


vtData25 <- vtData %>% filter(year < 1825)
vtData50 <- vtData %>%  filter(year > 1824, year < 1850)
vtData75 <- vtData %>%  filter(year > 1849, year < 1875)
vtData00 <- vtData %>%  filter(year > 1874, year < 1900)


#vtData25_short <- vtData[sample(nrow(vtData25), 100),]
vtData50_short <- vtData[sample(nrow(vtData50), 200),]
vtData75_short <- vtData[sample(nrow(vtData75), 200),]
vtData00_short <- vtData[sample(nrow(vtData00), 200),]



vtData_text25 <- vtData25$text %>% 
  paste(collapse="") %>% 
  gsub('\n',' ',.) %>% 
  tolower() %>% 
  gsub('[^A-Za-z ]', ' ', .) %>% 
  write_lines('data/vtData_text25.txt')

vtData_text50 <- vtData50$text %>% 
  paste(collapse="") %>% 
  gsub('\n',' ',.) %>% 
  tolower() %>% 
  gsub('[^A-Za-z ]', ' ', .) %>% 
  write_lines('data/vtData_text50.txt')

vtData_text75 <- vtData75$text %>% 
  paste(collapse="") %>% 
  gsub('\n',' ',.) %>% 
  tolower() %>% 
  gsub('[^A-Za-z ]', ' ', .) %>% 
  write_lines('data/vtData_text75.txt')

vtData_text00 <- vtData00$text %>% 
  paste(collapse="") %>% 
  gsub('\n',' ',.) %>% 
  tolower() %>% 
  gsub('[^A-Za-z ]', ' ', .) %>% 
  write_lines('data/vtData_text00.txt')


# CREATE MODEL

THREADS <- 3

if (!file.exists('data/vtData25_vectors.bin')) {
  vtData_model25 <- train_word2vec(
    'data/vtData_text25.txt',
    output_file='data/vtData25_vectors.bin',
    vectors=100,
    threads=THREADS,
    window=12, iter=10, negative_samples=0
  )
} else {
  vtData_model25 <- read.vectors('data/vtData25_vectors.bin')
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

if (!file.exists('data/vtData75_vectors.bin')) {
  vtData_model75 <- train_word2vec(
    'data/vtData_text75.txt',
    output_file='data/vtData75_vectors.bin',
    vectors=100,
    threads=THREADS,
    window=12, iter=10, negative_samples=0
  )
} else {
  vtData_model50 <- read.vectors('data/vtData75_vectors.bin')
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



# VISUALIZE

vtData_model00 %>% plot(perplexity=5)


# EXPERIMENT

women25 <- vtData_model25 %>% closest_to('woman', 30)
women50 <- vtData_model50 %>% closest_to('woman', 30)
women75 <- vtData_model75 %>% closest_to('woman', 30)
women00 <- vtData_model00 %>% closest_to('woman', 30)




allWomen <- women25$word %>% as_data_frame()
allWomen$word50=women50$word
allWomen$word75=women75$word
allWomen$word00=women00$word
colnames(allWomen)[colnames(allWomen)=="value"] <- "women25"


allWomen %>% write_csv("output/allWomenWords.csv")

### SEGMENT BY DECADES ###

#vtData60 <- vtData %>%  filter(year > 1851, year < 1860)
#vtData70 <- vtData %>%  filter(year > 1861, year < 1870)
#vtData80 <- vtData %>%  filter(year > 1871, year < 1880)
#vtData90 <- vtData %>%  filter(year > 1881, year < 1890)
#vtData00 <- vtData %>%  filter(year > 1891, year < 1900)

#vtData60_short <- vtData[sample(nrow(vtData60), 200),]
#vtData70_short <- vtData[sample(nrow(vtData70), 200),]
#vtData80_short <- vtData[sample(nrow(vtData80), 200),]
#vtData90_short <- vtData[sample(nrow(vtData90), 200),]
#vtData00_short <- vtData[sample(nrow(vtData00), 200),]

#vtData_text60 <- vtData60_short$text %>% 
#  paste(collapse="") %>% 
#  gsub('\n',' ',.) %>% 
#  tolower() %>% 
#  gsub('[^A-Za-z ]', ' ', .) %>% 
#  write_lines('data/vtData_text60.txt')

#vtData_text70 <- vtData70_short$text %>% 
#  paste(collapse="") %>% 
#  gsub('\n',' ',.) %>% 
#  tolower() %>% 
#  gsub('[^A-Za-z ]', ' ', .) %>% 
#  write_lines('data/vtData_text70.txt')

#vtData_text80 <- vtData80_short$text %>% 
#  paste(collapse="") %>% 
#  gsub('\n',' ',.) %>% 
#  tolower() %>% 
#  gsub('[^A-Za-z ]', ' ', .) %>% 
#  write_lines('data/vtData_text80.txt')

#vtData_text90 <- vtData90_short$text %>% 
#  paste(collapse="") %>% 
#  gsub('\n',' ',.) %>% 
#  tolower() %>% 
#  gsub('[^A-Za-z ]', ' ', .) %>% 
#  write_lines('data/vtData_text90.txt')

#vtData_text00 <- vtData00_short$text %>% 
#  paste(collapse="") %>% 
#  gsub('\n',' ',.) %>% 
#  tolower() %>% 
#  gsub('[^A-Za-z ]', ' ', .) %>% 
#  write_lines('data/vtData_text00.txt')

# if (!file.exists('data/vtData60_vectors.bin')) {
#   vtData_model60 <- train_word2vec(
#     'data/vtData_text60.txt',
#     output_file='data/vtData60_vectors.bin',
#     vectors=100,
#     threads=THREADS,
#     window=12, iter=10, negative_samples=0
#   )
# } else {
#   vtData_model60 <- read.vectors('data/vtData60_vectors.bin')
# }
# 
# 
# 
# if (!file.exists('data/vtData70_vectors.bin')) {
#   vtData_model70 <- train_word2vec(
#     'data/vtData_text70.txt',
#     output_file='data/vtData70_vectors.bin',
#     vectors=100,
#     threads=THREADS,
#     window=12, iter=10, negative_samples=0
#   )
# } else {
#   vtData_model70 <- read.vectors('data/vtData70_vectors.bin')
# }
# 
# 
# 
# if (!file.exists('data/vtData80_vectors.bin')) {
#   vtData_model80 <- train_word2vec(
#     'data/vtData_text80.txt',
#     output_file='data/vtData80_vectors.bin',
#     vectors=100,
#     threads=THREADS,
#     window=12, iter=10, negative_samples=0
#   )
# } else {
#   vtData_model80 <- read.vectors('data/vtData80_vectors.bin')
# }
# 
# 
# 
# if (!file.exists('data/vtData90_vectors.bin')) {
#   vtData_model90 <- train_word2vec(
#     'data/vtData_text90.txt',
#     output_file='data/vtData90_vectors.bin',
#     vectors=100,
#     threads=THREADS,
#     window=12, iter=10, negative_samples=0
#   )
# } else {
#   vtData_model90 <- read.vectors('data/vtData90_vectors.bin')
# }
# 
# 
# 
# if (!file.exists('data/vtData00_vectors.bin')) {
#   vtData_model00 <- train_word2vec(
#     'data/vtData_text00.txt',
#     output_file='data/vtData00_vectors.bin',
#     vectors=100,
#     threads=THREADS,
#     window=12, iter=10, negative_samples=0
#   )
# } else {
#   vtData_model00 <- read.vectors('data/vtData00_vectors.bin')
# }

# women60 <- vtData_model60 %>% closest_to('woman', 30)
# women70 <- vtData_model70 %>% closest_to('woman', 30)
# women80 <- vtData_model80 %>% closest_to('woman', 30)
# women90 <- vtData_model90 %>% closest_to('woman', 30)
# women00 <- vtData_model00 %>% closest_to('woman', 30)

# allWomen$word60=women60$word
# allWomen$word70=women70$word
# allWomen$word80=women80$word
# allWomen$word90=women90$word
# allWomen$word00=women00$word
# colnames(allWomen)[colnames(allWomen)=="value"] <- "women25"



vtData_model00 %>% closest_to(~'woman'+'mother',20) %>%
  View()


# ANALOGY

vtData_model00 %>% closest_to(~'man'-'boy'+'girl',20) %>%
  View()

