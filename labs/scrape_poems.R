library(tidyverse) 
library(rvest)
library(robotstxt)

url <- "https://en.wikipedia.org/wiki/List_of_Emily_Dickinson_poems"
poems <- url %>% 
  read_html() %>% 
  html_nodes("table")
poems_list <- html_table(poems[[1]])

path <- "C:/Users/seanwei/Desktop/STAT231-swei1999/labs"
write_csv(x = all_poems, path = paste0(path, "/scrape_poems.csv"))
