library(dplyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(tm)
library(stringr)
library(igraph)
library(ggraph)
library(widyr)
library(lubridate)
library(utf8)
library(textclean)
library(SnowballC)


# read in childrenshealthdefense csv file 
chd.2020<- read.csv(file='chd.2020.csv', stringsAsFactors=FALSE)

chd.2020 <- as_tibble(chd.2020)


##################################################################

#remove repetitive phrases from text
chd.2020$text <- gsub("childrens health defense rights reserved", "", chd.2020$text)
chd.2020$text <- gsub("privacy policy", "", chd.2020$text)
chd.2020$text <- gsub("medical disclaimer", "", chd.2020$text)
chd.2020$text <- gsub("make difference", "", chd.2020$text)
chd.2020$text <- gsub("republishing guidelines", "", chd.2020$text)
chd.2020$text <- gsub("sign free news updates robert", "", chd.2020$text)
chd.2020$text <- gsub("kennedy childrens health defense", "", chd.2020$text)
chd.2020$text <- gsub("planning strategies including legal", "", chd.2020$text)
chd.2020$text <- gsub("effort defend health children", "", chd.2020$text)
chd.2020$text <- gsub("obtain justice injured support", "", chd.2020$text)
chd.2020$text <- gsub("essential chds successful mission", "", chd.2020$text)
chd.2020$text <- gsub("implementing strategies including legal", "", chd.2020$text)
chd.2020$text <- gsub("jquerydocument.*$", "", chd.2020$text)
chd.2020$text <- gsub("functionev.*$", "", chd.2020$text)

#combine words that have same meaning - sarscovid, coronavirus
chd.2020$text <- gsub("coronavirus","covid", chd.2020$text)
chd.2020$text <- gsub("-", "", chd.2020$text)
chd.2020$text <- gsub("sarscov","covid", chd.2020$text)
chd.2020$text <- gsub("sars","covid", chd.2020$text)

# removes carrige returns and new lines from text
chd.2020$text <- gsub("\r?\n|\r", " ", chd.2020$text)

#normalize text so that curly apostrophes are switched to regular single quote character
chd.2020$text <- utf8_normalize(chd.2020$text, map_quote = TRUE)
chd.2020$text <- gsub("[-]", " ", chd.2020$text)
chd.2020$text <- gsub("[.]", "\"", chd.2020$text)

#remove unwanted symbols
chd.2020$text <- strip(chd.2020$text)
chd.2020$text <- iconv(chd.2020$text, "UTF-8", "ASCII", sub="")

# remove list of non-stop words that are common and hold no semantic value
other.words.chd <- c("chd", "jr", "dr", "contact", "state", "year", "years", "people", "time", "includes",
                 "includes", "including", "included", "make", "makes", "author", "authors",
                 "kennedy", "robert")

# clean data
chd.2020$text <- chd.2020$text %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>%
  removeWords(stopwords(source = "smart")) %>%
  removeWords(other.words.chd) %>%
  stripWhitespace()

chd.2020$date <- as_date(chd.2020$date)

chd.2020$month <- month(chd.2020$date)

chd.2020 <- chd.2020[, c("url", "date", "month", "title", "text", "class", "covid", "type")]

write.csv(chd.2020, 'chd.2020.Final.csv')

#######################################################################################
# read in mercola csv file 
mercola.2020<- read.csv(file='mercola.2020.csv', stringsAsFactors=FALSE)

mercola.2020 <- as_tibble(mercola.2020)

#combine words that have same meaning - sarscovid, coronavirus
mercola.2020$text <- gsub("coronavirus","covid", mercola.2020$text)
mercola.2020$text <- gsub("-", "", mercola.2020$text)
mercola.2020$text <- gsub("sarscov","covid", mercola.2020$text)
mercola.2020$text <- gsub("sars","covid", mercola.2020$text)

# removes carrige returns and new lines from text
mercola.2020$text <- gsub("\r?\n|\r", " ", mercola.2020$text)

#normalize text so that curly apostrophes are switched to regular single quote character
mercola.2020$text <- utf8_normalize(mercola.2020$text, map_quote = TRUE)
mercola.2020$text <- gsub("[-]", " ", mercola.2020$text)
mercola.2020$text <- gsub("[.]", "\"", mercola.2020$text)

#remove unwanted symbols
mercola.2020$text <- strip(mercola.2020$text)
mercola.2020$text <- iconv(mercola.2020$text, "UTF-8", "ASCII", sub="")

other.words.mercola <- c("percent", "study", "day", "lower", "reduced", "found", "dr",
                 "including", "reduce", "time", "level", "report", "u.s", "http",
                 "because", "sites", "article", "archive", "levels", "note",
                 "www.mercola.com", "online", "articles.mercola.com", "mercola", 
                 "people", "recommend", "person", "effect", "effects", "effecting",
                 "effected", "increase", "increased", "increasing", "increases", "mercola", 
                 "percent", "study", "day", "lower", "reduced", "found", "dr",
                    "studies")
# clean data
mercola.2020$text <- mercola.2020$text %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>%
  removeWords(stopwords("SMART")) %>%
  removeWords(other.words.mercola) %>%
  stripWhitespace()

mercola.2020$date <- as_date(mercola.2020$date)

mercola.2020$month <- month(mercola.2020$date)

glimpse(mercola.2020)

mercola.2020 <- mercola.2020[, c("url", "date", "month", "title", "text", "class", "covid", "type")]

write.csv(mercola.2020, 'mercola.2020.Final.csv')

#######################################################################################
# read in greenmed csv file 
greenmed.2020<- read.csv(file='greenmed.2020.csv', stringsAsFactors=FALSE)

greenmed.2020 <- as_tibble(greenmed.2020)

#remove repetitive phrases from text
greenmed.2020$text <- gsub("Disclaimer", "", greenmed.2020$text)
greenmed.2020$text <- gsub("dedicated investigating important health environmental issues day special emphasis", "", greenmed.2020$text)
greenmed.2020$text <- gsub("environmental health focused deep research explore ways present condition human body", "", greenmed.2020$text)
greenmed.2020$text <- gsub("directly reflects true state ambient environment pages natural medicine", "", greenmed.2020$text)
greenmed.2020$text <- gsub("alternatives information connect greenmedinfo website", "", greenmed.2020$text)
greenmed.2020$text <- gsub("information purposes providing information contained diagnosing", "", greenmed.2020$text)
greenmed.2020$text <- gsub("treating curing mitigating preventing type disease medical condition", "", greenmed.2020$text)
greenmed.2020$text <- gsub("beginning type natural integrative conventional treatment regimen advisable seek advice", "", greenmed.2020$text)
greenmed.2020$text <- gsub("licensed healthcare professional", "", greenmed.2020$text)
greenmed.2020$text <- gsub("greenmedinfocom", "", greenmed.2020$text)
greenmed.2020$text <- gsub("journal articles", "", greenmed.2020$text)
greenmed.2020$text <- gsub("original owners", "", greenmed.2020$text)
greenmed.2020$text <- gsub("mesh", "", greenmed.2020$text)
greenmed.2020$text <- gsub("nlm", "", greenmed.2020$text)

#combine words that have same meaning - sarscovid, coronavirus
greenmed.2020$text <- gsub("coronavirus","covid", greenmed.2020$text)
greenmed.2020$text <- gsub("-", "", greenmed.2020$text)
greenmed.2020$text <- gsub("sarscov","covid", greenmed.2020$text)
greenmed.2020$text <- gsub("sars","covid", greenmed.2020$text)

# removes carrige returns and new lines from text
greenmed.2020$text <- gsub("\r?\n|\r", " ", greenmed.2020$text)

#normalize text so that curly apostrophes are switched to regular single quote character
greenmed.2020$text <- utf8_normalize(greenmed.2020$text, map_quote = TRUE)
greenmed.2020$text <- gsub("[-]", " ", greenmed.2020$text)
greenmed.2020$text <- gsub("[.]", "\"", greenmed.2020$text)

#remove unwanted symbols
greenmed.2020$text <- strip(greenmed.2020$text)
greenmed.2020$text <- iconv(greenmed.2020$text, "UTF-8", "ASCII", sub="")

other.words.greenmed <- c("percent", "study", "studies", "studied", "copyright", "https", "al",
                          "doi", "greenmedinfo.com", "epub", "pmid", "u.s", "website", "greenmedinfo")

# clean data
greenmed.2020$text <- greenmed.2020$text %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>%
  removeWords(stopwords(source = "smart")) %>%
  removeWords(other.words.greenmed) %>%
  stripWhitespace()

greenmed.2020$date <- as_date(greenmed.2020$date)

greenmed.2020$month <- month(greenmed.2020$date)

greenmed.2020 <- greenmed.2020[, c("url", "date", "month", "title", "text", "class", "covid", "type")]

write.csv(greenmed.2020, 'greenmed.2020.Final.csv')

