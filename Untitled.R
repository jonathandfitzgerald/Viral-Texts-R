forSarah <- read_tsv("/Users/Fitz/Dropbox/HDAreview/data/wwir/wwir.tsv")
listSarah <- forSarah$fulltext
wordsSarah <- forSarah %>% unnest_tokens(word, fulltext)
wordsSarah <- wordsSarah %>% count(word, sort = TRUE)
wordsSarah %>% write_csv("/Users/Fitz/Dropbox/HDAreview/data/wwir/wordcount.csv")
