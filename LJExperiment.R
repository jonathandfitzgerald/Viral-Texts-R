# Read in LJ Sources
getwd()
install.packages("pdftools")
library(pdftools)
library(xml2)
# Create a list of PDFs
truestoriesPDF <- list.files(path = "LJ-to-Classify/True-Stories", full.names = TRUE, pattern = "pdf$")

# Create function to read PDFs

getPDFs = function(file) {
  message(file)
  pdf <- pdf_text(file)
  pdfTBL <- tibble(cluster=gsub("LJ-to-Classify/True-Stories/","",file),text=pdf)
  return(pdfTBL)
}

trueStories = tibble(cluster=truestoriesPDF) %>% 
  group_by(cluster) %>% 
  do(getPDFs(.$cluster)) 

trueStories <- trueStories %>% as_data_frame()

#txt <- pdf_text(truestoriesPDF[2])
#txt %>% as.character()
#write.csv(trueStories, file = paste('output/trueStories-6-16-17.csv',sep=""))



# PDFs are a mess. How about reading in a txt file from her collection (https://ia902604.us.archive.org/26/items/athomeandabroad16327gut/16327.txt)

fullerTXT <- read_file("LJ-to-Classify/Fuller/txt/fuller-abroad.txt")
splitz <- fullerTXT %>%
  strsplit("(?=LETTER)",perl = TRUE)


splitme <- function(x) {
  x <- unlist(strsplit(x, "(?=LETTER)", perl=TRUE))
  x <- unlist(strsplit(x, "(?<=LETTER)", perl=TRUE))
  for (i in which(x=="L")) {
    x[i+1] <- paste(x[i], x[i+1], sep="")
  }
  x[-which(x=="L")]
}
Fuller <- splitme(fullerTXT) %>% as_data_frame() %>% mutate("cluster" = paste("fuller",1:38, sep="_"))
Fuller <- Fuller %>% mutate("text" = value)
Fuller$value = NULL
Fuller <- Fuller %>% as_data_frame()



# Create a list of XMLs
fernXML <- list.files(path = "LJ-to-Classify/Fern", full.names = TRUE, pattern = "xml$")
nelsonXML <- list.files(path = "LJ-to-Classify/Nelson", full.names = TRUE, pattern = "html$")

# Create function to read XMLs

getP = function(file) {
  message(file)
  xml <- read_xml(file)
  p <- xml_find_all(xml, "//d1:text") %>% xml_text() %>% as.String() %>% as.character()
  head <- xml_find_all(xml, "//d1:head") %>% xml_text()
  head <- head[2] %>% as.list()
  fernXML <- tibble(filename=gsub("LJ-to-Classify/Fern/","",file),cluster=paste("Fern",as.character(head),collapse = "",sep = "-"),text=p)
  return(fernXML)
}

# Run function to read XMLs
Fern = tibble(filename=fernXML) %>% 
  group_by(filename) %>% 
  do(getP(.$filename)) 


getP2 = function(file) {
  message(file)
  xml <- read_xml(file)
  p <- xml_find_all(xml, "//d1:p") %>% xml_text() %>% as.String() %>% as.character()
  head <- xml_find_all(xml, "//d1:title") %>% xml_text()
  nelsonXML <- tibble(filename=gsub("LJ-to-Classify/Nelson/","",file),cluster=paste("Nelson",as.character(head),collapse = "",sep = "-"),text=gsub("Back to Beginning of Article","",p))
  return(nelsonXML)
}

# Run function to read XMLs
Nelson = tibble(filename=nelsonXML) %>% 
  group_by(filename) %>% 
  do(getP2(.$filename)) 

earlyLJ <- rbind(Fern,Nelson) %>% as_data_frame()
earlyLJ$filename <- NULL

earlyLJ <- rbind(earlyLJ,Fuller,trueStories) %>% as_data_frame %>% mutate("genre" = "LJ")
write.csv(earlyLJ, file = paste('output/earlyLJ-6-16-17.csv',sep=""))

#combine with other tagged genres
allGenres <- rbind(earlyLJ, newGenres)
write.csv(allGenres, file = paste('output/allGenres-6-16-17.csv',sep=""))

#rm(earlyLJ)
