library(rvest)
library(tidyverse)

# page delay when scraping multiple pages on site
# when checking patch.com's robots.txt they did not have a delay preference listed
scrape.delay <- 3

# created a variable equal to the total number of pages to scrape
total.pages <- c(0:8)

# empty vector for scrape.list
scrape.list <- vector()

# for loop that iterates across multiple pages 
for (i in seq_along(total.pages)) {

  # using the past function to scrape a list from each page of greensboro list
  main.url <- read_html(paste0("https://www.greenmedinfo.com/gmi-blogs?page=", total.pages[i]))

  # pull the href from each link of interest on the page
  scrape.page <- html_nodes(main.url, 'div.views-field.views-field-title span a') %>%
    html_attr("href")

  # add the list of hrefs from page to scrape.list
  scrape.list <- c(scrape.list, scrape.page)

  # pause scraper for 5 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

# combines the data scraped from the link href URLs 
for (i in seq_along(scrape.list)) {
  scrape.list[i] <- paste0('https://www.greenmedinfo.com/', scrape.list[i])
}

# empty vector to collect page text scraped from each page visited in loop
page.text <- vector()

# empty vector to collect page date scraped from each page visisted in loop
page.date <- vector()
# empty vector to collect page title scraped from each page visisted in loop
page.subject <- vector()

# The for loop visits each URL in scrape.list and then collects the text content from each page, creating a new list
for (i in seq_along(scrape.list)) {
  new.url <- read_html(scrape.list[i])

  #Collects text content from pages
  #text.add <- html_nodes(new.url, xpath='//p') %>%
    #html_text()

  #Collapses all the separate <p> text content into one string of text
  #text.add <- paste(text.add, collapse=" ")

  #Collects the date from pages
 # date.add <- html_nodes(new.url, xpath = '//*[@id="node-119968"]/div[3]/div[1]/div/div/div[1]/div[4]/div[1]/text()') %>%
   # html_text()
  
  #Collects the title from pages
  subject.add <- html_nodes(new.url, '#node-119968 > div.container.onecol-sidebar > div.col-md-7 > div > div > div.nd-region-header.clear-block > div.field.field-title > h1') %>%
    html_text()

  #page.text <- c(page.text, text.add)
  #page.date <- c(page.date, date.add)
  page.subject <- c(page.subject, subject.add)

  # pause scraper for 3 seconds before visiting and scraping next page
  Sys.sleep(scrape.delay)
}

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data <- tibble('url'=scrape.list, 'subject' = page.subject, 'date'=page.date, 'text'=page.text)

# Save dataframe as a CSV file
write.csv(scrape.data, 'greenmedinfo.csv')



