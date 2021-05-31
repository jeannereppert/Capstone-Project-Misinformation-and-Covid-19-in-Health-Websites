#load libraries
library(fs)
library(tidyverse)
library(tidytext)
library(widyr)
library(tm)
library(dplyr)
library(readr)
library(ggplot2)
library(SnowballC)
library(stringr)
library(igraph)
library(ggraph)
library(wordcloud)
library(plotly)
library(syuzhet)
library(RColorBrewer)

#############################################################################################
# function to clean Mercola Website files w/unnest_tokens

tidy <- function(mydataset) {
  to_clean <- mydataset
  
  # transform table into one-word-per-line tidytext format
  to_clean <- to_clean %>%
    unnest_tokens(word, text)
  
  #stem words
  to_clean <- to_clean %>%
    mutate(word = wordStem(word))
}



####################################################################
# function to visualize top words

visualize_top_words <- function(cleaned_data, wordamt, title) {
  data <- cleaned_data
  data %>%
    count(word, sort = TRUE) %>%
    filter(n > wordamt) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    ylab(title) +
    coord_flip()
}

############################################################################
# produces visualization of 9 most freqent words and the words that correlate
# with them
############################################################################
visualize_word_cors <- function(cleaned_data, topWords, twTitle, N){
  
  dataset <- cleaned_data
  wordlist <- topWords
  title <- twTitle
  
  word_cors <- dataset %>%
    group_by(word) %>%
    filter(n() >= N) %>%
    pairwise_cor(word, url, sort = TRUE)

  # produce graph comparing 4 words of importance to their most correlated words
  word_cors %>%
    filter(item1 %in% wordlist) %>%
    group_by(item1) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip() +
    ggtitle(title) +
    theme(plot.title = element_text(size = 10, face = "bold"))
}

############################################################################
# produces visualization of 9 most freqent words and the words that correlate
# with them
############################################################################
gm_visualize_word_cors <- function(cleaned_data, topWords, twTitle){
  
  dataset <- cleaned_data
  wordlist <- topWords
  title <- twTitle
  
  word_cors <- dataset %>%
    group_by(word) %>%
    filter(n() >= 600) %>%
    pairwise_cor(word, url, sort = TRUE)
  
  # produce graph comparing 4 words of importance to their most correlated words
  word_cors %>%
    filter(item1 %in% wordlist) %>%
    group_by(item1) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip() +
    ggtitle(title)
}



########################################################################
# function to visualize bigram relationship
########################################################################
count_bigrams <- function(dataset) {
  uni_sw <- data.frame(word = c("percent", "found", "dr", "including", "u.s", "http", "because", "sites", "article", "archive",
                                "www.mercola.com", "online", "articles.mercola.com", "mercola" ))
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word1 %in% uni_sw$word,
           !word2 %in% uni_sw$word
           ) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.03, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = .5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = .5, size=2.3) +
    theme_void()
}


########################################################################
# function to visualize trigram relationship
########################################################################
count_trigrams <- function(dataset) {
  uni_sw <- data.frame(word = c("percent", "found", "dr", "including", "u.s", "http", "because", "sites", "article", "archive",
                                "www.mercola.com", "online", "articles.mercola.com", "mercola" ))
  dataset %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word,
           !word1 %in% uni_sw$word,
           !word2 %in% uni_sw$word,
           !word3 %in% uni_sw$word
    ) %>%
    count(word1, word2, word3, sort = TRUE)
}

visualize_trigrams <- function(trigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.03, "inches"))
  
  trigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, 
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = .5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = .5, size = 2.3) +
    theme_void() 
   # ggtitle(title)
}


########################################################################
# function to visualize quadgram relationship
########################################################################
count_quadgrams <- function(dataset) {
  uni_sw <- data.frame(word = c("percent", "found", "dr", "including", "u.s", "http", "because", "sites", "article", "archive",
                                "www.mercola.com", "online", "articles.mercola.com", "mercola" ))
  dataset %>%
    unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
    separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word,
           !word4 %in% stop_words$word,
           !word1 %in% uni_sw$word,
           !word2 %in% uni_sw$word,
           !word3 %in% uni_sw$word,
           !word4 %in% uni_sw$word
    ) %>%
    count(word1, word2, word3, word4, sort = TRUE)
}

visualize_quadgrams <- function(quadgrams, title) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  quadgrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 2) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=3) +
    theme_void() +
    ggtitle(title)
}


###########################################################################
# Function for Word cloud with Sentiment Analysis
###########################################################################

word_cloud_title <- function(dataset, title, rn){
  data <- dataset
  
  #cleaning
  data$title <- iconv(data$title, "UTF-8", "ASCII", sub="") 
  
  # clean data
  data$title <- data$title %>%
    removePunctuation() %>%
    removeNumbers() %>%
    tolower() %>%
    removeWords(stopwords(source = "smart")) %>%
    #stemDocument() %>%
    stripWhitespace()
  
  
  # Emotions for each tweet using NRC dictionary
  emotions <- get_nrc_sentiment(data$title)
  
  #create word cloud columns
  wordcloud_title = c(
    paste(data$title[emotions$anger > 0], collapse=" "),
    paste(data$title[emotions$anticipation >0], collapse=" "),
    paste(data$title[emotions$disgust > 0], collapse=" "),
    paste(data$title[emotions$fear > 0], collapse=" "),
    paste(data$title[emotions$joy > 0], collapse=" "),
    paste(data$title[emotions$sadness > 0], collapse=" "),
    paste(data$title[emotions$surprise > 0], collapse=" "),
    paste(data$title[emotions$trust > 0], collapse=" ")
  )
  
  # create corpus and covert to matrix
  corpus = Corpus(VectorSource(wordcloud_title))
  tdm = as.matrix(TermDocumentMatrix(corpus))
  tdmnew <- tdm[nchar(rownames(tdm)) < rn,]
  
  # column name binding
  colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  colnames(tdmnew) <- colnames(tdm)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  comparison.cloud(tdmnew, random.order=FALSE,
                   colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                   title.size=1, max.words=500, scale=c(5, 0.3),rot.per=0.4, main="Title")
}

###########################################################################
# Function for Word cloud with Sentiment Analysis for Text
###########################################################################

word_cloud_text <- function(dataset, title, rn){
  data <- dataset
  
  #cleaning
  data$text <- iconv(data$text, "UTF-8", "ASCII", sub="") 
  
  # clean data
  data$text <- data$text %>%
    removePunctuation() %>%
    removeNumbers() %>%
    tolower() %>%
    removeWords(stopwords(source = "smart")) %>%
    #stemDocument() %>%
    stripWhitespace()
  
  
  # Emotions for each tweet using NRC dictionary
  emotions <- get_nrc_sentiment(data$text)
  
  #create word cloud columns
  wordcloud_text = c(
    paste(data$text[emotions$anger > 3], collapse=" "),
    paste(data$text[emotions$anticipation >3], collapse=" "),
    paste(data$text[emotions$disgust > 3], collapse=" "),
    paste(data$text[emotions$fear > 3], collapse=" "),
    paste(data$text[emotions$joy > 3], collapse=" "),
    paste(data$text[emotions$sadness > 3], collapse=" "),
    paste(data$text[emotions$surprise > 3], collapse=" "),
    paste(data$text[emotions$trust > 3], collapse=" ")
  )
  
  # create corpus and covert to matrix
  corpus = Corpus(VectorSource(wordcloud_text))
  tdm = as.matrix(TermDocumentMatrix(corpus))
  tdmnew <- tdm[nchar(rownames(tdm)) < rn,]
  
  # column name binding
  colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  colnames(tdmnew) <- colnames(tdm)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  comparison.cloud(tdmnew, random.order=FALSE,
                   colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                   title.size=1, max.words=200, scale=c(5, 0.3),rot.per=0.4, main="Title")
}
################################################################################
# function to clean Mercola Facebook files w/unnest_tokens
################################################################################

mercola_FBclean_text <- function(mydataset) {
  to_clean <- mydataset
  
  to_clean$Message <- iconv(to_clean$Message, "UTF-8", "ASCII", sub="")
  
  #combine words that have same meaning - sarscovid, coronavirus
  to_clean$Message <- gsub("coronavirus","covid", to_clean$Message)
  to_clean$Message <- gsub("covid-19", "covid", to_clean$Message)
  to_clean$Message <- gsub("-", "", to_clean$Message)
  to_clean$Message <- gsub("sarscov","covid", to_clean$Message)
  to_clean$Message <- gsub("sars","covid", to_clean$Message)
  
  # transform table into one-word-per-line tidytext format
  to_clean <- to_clean %>%
    unnest_tokens(word, Message)
  
  # remove stop words
  data(stop_words)
  to_clean <- to_clean %>%
    anti_join(stop_words)
  
  # remove numbers
  nums <- to_clean %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
  to_clean <- to_clean %>%
    anti_join(nums, by = "word")
  
  # remove other words
  uni_sw <- data.frame(word = c("mercola", "dr", "http", "np", "utm_source", "utm_medium", "utm_content",
                                "utm_campaign", "https", "sites", "click", "bit.ly", "facebookmercola_ranart", "u.", "html",
                                "dont", "mercola", "u.s", "u.s.a", "youve", "rt", "t.co", "twitter", "message",
                               "sharyl", "content", "continue", "select", "szddr5ytl7" ,"pxdfirvo"))
  to_clean <- to_clean %>%
    anti_join(uni_sw, by = "word")
  
  to_clean <- to_clean %>%
    mutate(word = wordStem(word))  

}

################################################################################
# function to clean CHD Facebook files w/unnest_tokens
################################################################################

chd_FBclean_text <- function(mydataset) {
  to_clean <- mydataset
  
  #combine words that have same meaning - sarscovid, coronavirus
  to_clean$Message <- gsub("coronavirus","covid", to_clean$Message)
  to_clean$Message <- gsub("covid-19", "covid", to_clean$Message)
  to_clean$Message <- gsub("-", "", to_clean$Message)
  to_clean$Message <- gsub("sarscov","covid", to_clean$Message)
  to_clean$Message <- gsub("sars","covid", to_clean$Message)
  
  #cleaning out links
  to_clean$Message <- gsub("https://t.co/JME7TkBp57", "", to_clean$Message)
  to_clean$Message <- gsub("https://t.co/JME7TkjOdz", "", to_clean$Message)
  to_clean$Message <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", to_clean$Message)
  to_clean$Message <- iconv(to_clean$Message, "UTF-8", "ASCII", sub="")
  to_clean$Message <- gsub("www.worldmercuryproject.org", "wmp", to_clean$Message)
  to_clean$Message <- gsub("worldmercuryproject.org", "wmp", to_clean$Message)
  to_clean$Message <- gsub("covid19", "covid", to_clean$Message)
  
  # transform table into one-word-per-line tidytext format
  to_clean <- to_clean %>%
    unnest_tokens(word, Message)
  
  # remove stop words
  data(stop_words)
  to_clean <- to_clean %>%
    anti_join(stop_words)
  
  # remove numbers
  nums <- to_clean %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
  to_clean <- to_clean %>%
    anti_join(nums, by = "word")
  
  # remove other words
  uni_sw <- data.frame(word = c("rfk", "dr", "http", "np", "utm_source", "utm_medium", "utm_content",
                                "utm_campaign", "https", "sites", "click", "bit.ly", "kennedy", "u.", "html",
                                "dont", "jr", "u.s", "u.s.a", "youve", "rt", "t.co", "twitter", "message",
                                "free", "content", "continue", "select", "robert", "sign", "chd",
                                "a_bid", "a_aid", "sessioncopycomplete"))
  to_clean <- to_clean %>%
    anti_join(uni_sw, by = "word")
  
  to_clean <- to_clean %>%
    mutate(word = wordStem(word))  
  
}

################################################################################
# function to clean GM Facebook files w/unnest_tokens
################################################################################

gm_FBclean_text <- function(mydataset) {
  to_clean <- mydataset
  
  #combine words that have same meaning - sarscovid, coronavirus
  to_clean$Message <- gsub("coronavirus","covid", to_clean$Message)
  to_clean$Message <- gsub("-", "", to_clean$Message)
  to_clean$Message <- gsub("sarscov","covid", to_clean$Message)
  to_clean$Message <- gsub("sars","covid", to_clean$Message)
  to_clean$Message <- gsub("covid19", "covid", to_clean$Message)
  
  #remove urls
  to_clean$Message <- gsub("bit.ly/2kjN4HH", "", to_clean$Message)
  to_clean$Message <- gsub("goo.gl/vfazJJ", "", to_clean$Message)
  to_clean$Message <- gsub("<[^>]*>","",to_clean$Message)
  
  # remove all urls
  to_clean$Message <- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", to_clean$Message )
  to_clean$Message = gsub("http\\w+", "", to_clean$Message)
  
  #cleaning out links
 # to_clean$Message <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", to_clean$Message)
  to_clean$Message <- iconv(to_clean$Message, "UTF-8", "ASCII", sub="")
  to_clean$Message <- gsub("www", "", to_clean$Message)
  to_clean$Message <- gsub("com", "", to_clean$Message)
  
  other.words <- "http"
  
  # clean data
  to_clean$Message  <- to_clean$Message  %>%
    removePunctuation() %>%
    removeNumbers() %>%
    tolower() %>%
    removeWords(stopwords("SMART")) %>%
    removeWords(other.words) %>%
    stemDocument() %>%
    stripWhitespace()
  
  # transform table into one-word-per-line tidytext format
  to_clean <- to_clean %>%
    unnest_tokens(word, Message)
  
  # remove stop words
  data(stop_words)
  to_clean <- to_clean %>%
    anti_join(stop_words)
  
  # remove numbers
  nums <- to_clean %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
  to_clean <- to_clean %>%
    anti_join(nums, by = "word")
  
  # remove other words
  #uni_sw <- data.frame(word = c("rfk", "dr"))
  
  #to_clean <- to_clean %>%
    #anti_join(uni_sw, by = "word")
  
  to_clean <- to_clean %>%
    mutate(word = wordStem(word))
  
}


############################################################################
# produces visualization of 9 most freqent words and the words that correlate
# with them (for Facebook Posts)
############################################################################
FB_visualize_word_cors <- function(cleaned_data, topWords, twTitle, nwords){
  number <- nwords
  dataset <- cleaned_data
  wordlist <- topWords
  title <- twTitle
  
  word_cors <- dataset %>%
    group_by(word) %>%
    filter(n() >= number) %>%
    pairwise_cor(word, url, sort = TRUE)
  
  # produce graph comparing 4 words of importance to their most correlated words
  word_cors %>%
    filter(item1 %in% wordlist) %>%
    group_by(item1) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip() +
    ggtitle(title)
}

######################################################################
# function to visualize Facebook posts bigram relationships
######################################################################
fb_count_bigrams <- function(dataset) {
  uni_sw <- data.frame(word = c("percent", "found", "dr", "including", "u.s", "http", "because", "sites", "article", "archive",
                                "www.mercola.com", "online", "articles.mercola.com", "mercola", "rt", "htt", "t.co", 
                                "tag", "twit", "https", "page", "posting", "click", "select", "ca"))
  #cleaning
  dataset$Message <- iconv(dataset$Message, "UTF-8", "ASCII", sub="")
  
  dataset %>%
    unnest_tokens(bigram, Message, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word1 %in% uni_sw$word,
           !word2 %in% uni_sw$word,
    ) %>%
    count(word1, word2, sort = TRUE)
}

fb_visualize_bigrams <- function(bigrams, title) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 2) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=3) +
    theme_void() +
    ggtitle(title)
}

###########################################################################
# Function for FB Word cloud with Sentiment Analysis
###########################################################################

FB_word_cloud <- function(dataset, title, n){
  data <- dataset
  
  
  #cleaning
  data$Message <- iconv(data$Message, "UTF-8", "ASCII", sub="") 
  
  
  # clean data
  data$Message <- data$Message %>%
    removePunctuation() %>%
    removeNumbers() %>%
    tolower() %>%
    removeWords(stopwords(source = "smart")) %>%
    stemDocument() %>%
    stripWhitespace()
  
  #cleaning
  data$Message <- iconv(data$Message, "UTF-8", "ASCII", sub="")
  
  # Emotions for each tweet using NRC dictionary
  emotions <- get_nrc_sentiment(data$Message)
  
  #create word cloud columns
  wordcloud_Message = c(
    paste(data$Message[emotions$anger > 0], collapse=" "),
    paste(data$Message[emotions$anticipation >0], collapse=" "),
    paste(data$Message[emotions$disgust > 0], collapse=" "),
    paste(data$Message[emotions$fear > 0], collapse=" "),
    paste(data$Message[emotions$joy > 0], collapse=" "),
    paste(data$Message[emotions$sadness > 0], collapse=" "),
    paste(data$Message[emotions$surprise > 0], collapse=" "),
    paste(data$Message[emotions$trust > 0], collapse=" ")
  )
  
  # create corpus and covert to matrix
  corpus = Corpus(VectorSource(wordcloud_Message))
  tdm = as.matrix(TermDocumentMatrix(corpus))
  tdmnew <- tdm[nchar(rownames(tdm)) < n,]
  
  # column name binding
  colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  colnames(tdmnew) <- colnames(tdm)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, title)
  comparison.cloud(tdmnew, random.order=FALSE,
                   colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                   title.size=1, max.words=250, scale=c(5, 0.3),rot.per=0.4, main="Title")
}

############################################################################
# produces visualization of freqent words and the words that correlate
# with them (for Tweets)
############################################################################
TW_visualize_word_cors <- function(cleaned_data, topWords, twTitle, nwords){
  number <- nwords
  dataset <- cleaned_data
  wordlist <- topWords
  title <- twTitle
  
  word_cors <- dataset %>%
    group_by(word) %>%
    filter(n() >= number) %>%
    pairwise_cor(word, url, sort = TRUE)
  
  # produce graph comparing 4 words of importance to their most correlated words
  word_cors %>%
    filter(item1 %in% wordlist) %>%
    group_by(item1) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip() +
    ggtitle(title)
}


############################################################################
# co-occurance graphs for twitter - username
############################################################################

tw_user_graph <- function(tweets.data, n){
  
# convert to quanteda corpus structure
tweets.corpus <- corpus(tweets.data)

# convert to document feature matrix
tweet.dfm <- dfm(tweets.corpus, remove_punct = TRUE)

# explore top usernames
# extract the most frequently mentioned usernames
user.dfm <- dfm_select(tweet.dfm, pattern = "@*")
topuser <- names(topfeatures(user.dfm, n))


# graph co-occurance of username mentions
# construct feature co-occurance matrix of usernames
user.fcm <- fcm(user.dfm)

# create a graph of co-occurance relationships for mentioned usernames
user.fcm <- fcm_select(user.fcm, pattern = topuser)
user_plot <- textplot_network(user.fcm, min_freq = 0.1, edge_color = "orange", edge_alpha = 0.8, edge_size = 5)
}

############################################################################
# co-occurance graphs for twitter - hashtag
############################################################################

tw_hashtag_graph <- function(tweets.data, n){
  
  # convert to quanteda corpus structure
  tweets.corpus <- corpus(tweets.data)
  
  # convert to document feature matrix
  tweet.dfm <- dfm(tweets.corpus, remove_punct = TRUE)
  
  # top hashtags
  # extract the most frequently used hashtags
  tag.dfm <- dfm_select(tweet.dfm, pattern = ("#*"))
  toptag <- names(topfeatures(tag.dfm, n))
  
  # graph co-occurance of hashtags
  # construct feature co-occurance matrix of hashtags
  tag.fcm <- fcm(tag.dfm)
  
  # create graph of co-occurance relationships for tags
  toptag.fcm <- fcm_select(tag.fcm, pattern = toptag)
  hashtags_plot <- textplot_network(toptag.fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5)
  
}
##########################################################
#Top Pages WordCloud
###########################################################

wc_toppages <- function(dataset, title) {
  set.seed(1234)
  pal = brewer.pal(9,"Dark2")
  wordcloud(names(dataset),as.numeric(dataset), 
            scale=c(.8,.1),min.freq=1,max.words=100, 
            rot.per = FALSE, 
            random.color = T,
            colors = pal,
            random.order=T, 
            family = "mono", font = 2)
}


 




