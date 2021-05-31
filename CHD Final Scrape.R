#####################################################################
# scrapes the entire history
# of CHD's news articles (all pages)
#***This site switched the way it circulates news articles, archives
#are now hidden but this file reflects all news in archives
#Defender site adds articles after the switch which are 
#archived on a separate site, duplicate url's will be removed before
#scraping
#####################################################################

#Load libraries
library(rvest)
library(tidyverse)
library(dplyr)
library(robotstxt)

#####################################################
# Accessing robots.txt and delaying scraping scripts
#####################################################

r_text <- get_robotstxt("https://www.childrenshealthdefense.org")
r_text <- parse_robotstxt(r_text)
r_text

# Show the amount of delay
r_text$crawl_delay$value

# page delay when scraping multiple pages on site
scrape.delay <- 3

##########################################################################################
#
#  CHD News Archives Scrape URL's
#
##########################################################################################

#create empty vector for url's in loop
scrape.urls <- vector()

#variable equal to the total number of pages to scrape
total.pages.chdnews <- c(1:27)

# for loop that iterates across multiple pages
for (i in seq_along(total.pages.chdnews)) {
  
  # using the past function to scrape a list from each page 
  main.url <- read_html(paste0("https://childrenshealthdefense.org/category/news/news/page/", total.pages.chdnews[i]))
  
  # pull the href from each link of interest on the page
  scrape.page <- html_nodes(main.url, xpath='//h5//a') %>%
    html_attr("href")
  
  # add the list of hrefs from page to scrape.list
  scrape.urls <- c(scrape.urls, scrape.page)
  
  # pause scraper for 5 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

#saving a backup for scraped url's in CHD news archives
scrape.urls.tosave <- scrape.urls
#saving a backup file of scraped url's
scrape.urls.tosave<- as.character(scrape.urls.tosave)
scrape.data.tosave <- tibble("url" =scrape.urls.tosave)
write.csv(scrape.data.tosave, 'chdurls.news.csv')

##########################################################################################
#
#  Defender Covid News Scrape URL's
#
##########################################################################################

#variable equal to the total number of pages to scrape
total.pages.defcovnews <- c(1:2)

# for loop that iterates across multiple pages
for (i in seq_along(total.pages.defcovnews)) {
  
  # using the past function to scrape a list from each page 
  main.url <- read_html(paste0("https://childrenshealthdefense.org/defender_category/covid/page/", total.pages.defcovnews[1]))
  
  # pull the href from each link of interest on the page
  scrape.page <- html_nodes(main.url, css='div.defender-post-grid-item div a') %>%
    html_attr("href")
  
  # add the list of hrefs from page to scrape.list
  scrape.urls <- c(scrape.urls, scrape.page)
  
  # pause scraper for 5 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

##########################################################################################
#
#  Defender Big Pharma News Scrape URL's
#
##########################################################################################

#variable equal to the total number of pages to scrape
total.pages.defpharmanews <- c(1:6)

# for loop that iterates across multiple pages
for (i in seq_along(total.pages.defpharmanews)) {
  
  # using the past function to scrape a list from each page 
  main.url <- read_html(paste0("https://childrenshealthdefense.org/defender_category/big-pharma/page/", total.pages.defpharmanews[i]))
  
  # pull the href from each link of interest on the page
  scrape.page <- html_nodes(main.url, css='div.defender-post-grid-item div a') %>%
    html_attr("href")
  
  # add the list of hrefs from page to scrape.list
  scrape.urls <- c(scrape.urls, scrape.page)
  
  # pause scraper for 5 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

##########################################################################################
#
#  Defender Big Energy Scrape URL's
#
##########################################################################################

#variable equal to the total number of pages to scrape
total.pages.defenergynews <- c(1:1)

# for loop that iterates across multiple pages
for (i in seq_along(total.pages.defenergynews)) {
  
  # using the past function to scrape a list from each page 
  main.url <- read_html(paste0("https://childrenshealthdefense.org/defender_category/big-energy/page/", total.pages.defenergynews[i]))
  
  # pull the href from each link of interest on the page
  scrape.page <- html_nodes(main.url, css='div.defender-post-grid-item div a') %>%
    html_attr("href")
  
  # add the list of hrefs from page to scrape.list
  scrape.urls <- c(scrape.urls, scrape.page)
  
  # pause scraper for 5 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

##########################################################################################
#
#  Defender Big Food Scrape URL's
#
##########################################################################################

#variable equal to the total number of pages to scrape
total.pages.deffoodnews <- c(1:1)

# for loop that iterates across multiple pages
for (i in seq_along(total.pages.deffoodnews)) {
  
  # using the past function to scrape a list from each page 
  main.url <- read_html(paste0("https://childrenshealthdefense.org/defender_category/big-food/page/", total.pages.deffoodnews[i]))
  
  # pull the href from each link of interest on the page
  scrape.page <- html_nodes(main.url, css='div.defender-post-grid-item div a') %>%
    html_attr("href")
  
  # add the list of hrefs from page to scrape.list
  scrape.urls <- c(scrape.urls, scrape.page)
  
  # pause scraper for 5 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

##########################################################################################
#
#  Defender Big Tech News Scrape URL's
#
##########################################################################################

#variable equal to the total number of pages to scrape
total.pages.deftechnews <- c(1:1)

# for loop that iterates across multiple pages
for (i in seq_along(total.pages.deftechnews)) {
  
  # using the past function to scrape a list from each page 
  main.url <- read_html(paste0("https://childrenshealthdefense.org/defender_category/big-tech/page/", total.pages.deftechnews[i]))
  
  # pull the href from each link of interest on the page
  scrape.page <- html_nodes(main.url, css='div.defender-post-grid-item div a') %>%
    html_attr("href")
  
  # add the list of hrefs from page to scrape.list
  scrape.urls <- c(scrape.urls, scrape.page)
  
  # pause scraper for 5 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

##########################################################################################
#
#  Defender Big Chemical News Scrape URL's
#
##########################################################################################

#variable equal to the total number of pages to scrape
total.pages.defchemnews <- c(1:1)

# for loop that iterates across multiple pages
for (i in seq_along(total.pages.defchemnews)) {
  
  # using the past function to scrape a list from each page 
  main.url <- read_html(paste0("https://childrenshealthdefense.org/defender_category/big-chemical/page/", total.pages.defchemnews[i]))
  
  # pull the href from each link of interest on the page
  scrape.page <- html_nodes(main.url, css='div.defender-post-grid-item div a') %>%
    html_attr("href")
  
  # add the list of hrefs from page to scrape.list
  scrape.urls <- c(scrape.urls, scrape.page)
  
  # pause scraper for 5 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

##########################################################################################
#
#  Defender Lawsuit News Scrape URL's
#
##########################################################################################

#variable equal to the total number of pages to scrape
total.pages.deflawnews <- c(1:1)

# for loop that iterates across multiple pages
for (i in seq_along(total.pages.deflawnews)) {
  
  # using the past function to scrape a list from each page 
  main.url <- read_html(paste0("https://childrenshealthdefense.org/defender_category/lawsuits/page/", total.pages.deflawnews[i]))
  
  # pull the href from each link of interest on the page
  scrape.page <- html_nodes(main.url, css='div.defender-post-grid-item div a') %>%
    html_attr("href")
  
  # add the list of hrefs from page to scrape.list
  scrape.urls <- c(scrape.urls, scrape.page)
  
  # pause scraper for 5 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

##########################################################################################
## Delete duplicate URL's - this has occurred because of site transition (some archived
## articles were reprinted in new Defender newsletter)
scrape.urls <- scrape.urls[!duplicated(scrape.urls)]

##########################################################################################
## Save Final list of all URL's

#saving a backup for scraped url's
scrape.urls.tosave <- scrape.urls
#saving a backup file of scraped url's
scrape.urls.tosave<- as.character(scrape.urls.tosave)
scrape.data.tosave <- tibble("url" =scrape.urls.tosave)
write.csv(scrape.data.tosave, 'chdurls.csv')

##########################################################################################
#
# Scrape pages to get Text/Date/Author/Title Info (2 loops used - one for new Defender
# website, and one for archives because html was different in each set)
#
##########################################################################################

#############################################################################################
# Collecting from CHD Defender News articles using rvest
#
#####################################################################

##reload chdurls.csv
scrape.urls <- vector()
scrape.urls <- read.csv('chdurls.csv', header = FALSE, stringsAsFactors=FALSE)[,-1]

##remove header
scrape.urls <- scrape.urls[-1]

# Create a empty vectors that will be filled data by the 'for loop' below
page.text <- vector()
page.date <- vector()
page.subject <- vector()

#select only url's from Defender Newsletter (/defender/ in chdurls)
scrape.urls.def <- scrape.urls[grep("/defender/", scrape.urls)]

# The for loop visits each URL in scrape.urls.def and then collects content from each page
for (i in seq_along(scrape.urls.def)) {
  new.url <- read_html(scrape.urls.def[i])
  
  #Collects text content from pages
  text.add <- html_nodes(new.url, '.post-content') %>%
    html_text()
  
  #Collapses all the separate text content into one string of text
  text.add <- paste(text.add, collapse=" ")
  
  #Collects the date from pages
  date.add <- html_nodes(new.url, '.date') %>%
    html_text()
  
  #Collects the title from pages
  subject.add <- html_nodes(new.url, '.post-title') %>%
    html_text()
  
  page.text <- c(page.text, text.add)
  page.date <- c(page.date, date.add)
  page.subject <- c(page.subject, subject.add)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

scrape.urls.def <- as.character(scrape.urls.def)

# Using tibble, the list of URLs is combined with other scraped dataL to create dataframe
scrape.data.def <- tibble('url' = scrape.urls.def, 'subject' = page.subject, 'date' = page.date, 'text'=page.text)

# Save dataframe as a CSV file
write.csv(scrape.data.def, 'chddef.csv')

#saving a backup for scraped text
scrape.text.tosave <- page.text
#saving a backup file of scraped text
scrape.text.tosave<- as.character(scrape.text.tosave)
scrape.text.tosave <- tibble(scrape.text.tosave)
write.csv(scrape.text.tosave, 'chdtextdef.csv')

#saving a backup for scraped dates
scrape.date.tosave <- page.date
#saving a backup file of scraped dates
scrape.date.tosave<- as.character(scrape.date.tosave)
scrape.date.tosave <- tibble(scrape.date.tosave)
write.csv(scrape.date.tosave, 'chddatedef.csv')

#saving a backup for scraped titles
scrape.titles.tosave <- page.subject
#saving a backup file of scraped titles
scrape.titles.tosave<- as.character(scrape.titles.tosave)
scrape.titles.tosave <- tibble(scrape.titles.tosave)
write.csv(scrape.titles.tosave, 'chdtitlesdef.csv')

#############################################################################################
# Collecting from CHD the remaining articles using rvest
#
# reload chdurls
scrape.urls <- vector()
scrape.urls <- read.csv('chdurls.csv', header = FALSE, stringsAsFactors=FALSE)[,-1]

#remove header
scrape.urls <- scrape.urls[-1]

# Creates empty vectors to be filled data by the 'for loop' below
page.text <- vector()
page.date <- vector()
page.subject <- vector()

#selects all urls that are not part of the Defender newsletter (the remaining urls)
scrape.urls.allother <- scrape.urls[lapply(scrape.urls,function(x) length(grep("/defender/",x,value=FALSE))) == 0]

# The for loop visits each URL in scrape.urls.allother and then collects content from each page
for (i in seq_along(scrape.urls.allother)) {
  new.url <- read_html(scrape.urls.allother[i])
  
  #Collects text content from pages
  text.add <- html_nodes(new.url, '.grid8 p') %>%
    html_text()
  
  #Collapses all the separate <p> text content into one string of text
  text.add <- paste(text.add, collapse=" ")
  
  #Collects the date from pages
  date.add <- html_nodes(new.url, '.grid8 .date') %>%
    html_text()
  
  #Collects the title from pages
  subject.add <- html_nodes(new.url, 'h1') %>%
    html_text()
  
  page.text <- c(page.text, text.add)
  page.date <- c(page.date, date.add)
  page.subject <- c(page.subject, subject.add)
  
  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

###Deleting 4 records that do not represent titles in page.subject
page.subject <- page.subject[- c(118, 254, 414, 584)]

scrape.urls.allother <- as.character(scrape.urls.allother)

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe
scrape.data.allother <- tibble('url' = scrape.urls.allother, 'subject' = page.subject, 'date' = page.date, 'text'=page.text)

# Save dataframe as a CSV file
write.csv(scrape.data.allother, 'chdallother.csv')

#saving a backup for scraped text
scrape.text.tosave <- page.text
#saving a backup file of scraped text
scrape.text.tosave<- as.character(scrape.text.tosave)
scrape.text.tosave <- tibble(scrape.text.tosave)
write.csv(scrape.text.tosave, 'chdtextallother.csv')

#saving a backup for scraped dates
scrape.date.tosave <- page.date
#saving a backup file of scraped dates
scrape.date.tosave<- as.character(scrape.date.tosave)
scrape.date.tosave <- tibble(scrape.date.tosave)
write.csv(scrape.date.tosave, 'chddateallother.csv')

#saving a backup for scraped titles
scrape.titles.tosave <- page.subject
#saving a backup file of scraped titles
scrape.titles.tosave<- as.character(scrape.titles.tosave)
scrape.titles.tosave <- tibble(scrape.titles.tosave)
write.csv(scrape.titles.tosave, 'chdtitlesallother.csv')
