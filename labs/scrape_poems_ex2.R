# scrape Emily Dickinson poems from Wikipedia
# first, get list of Dickinson poems
# then, identify patterns in Wikipedia page links for the poems
# then, scrape text on each relevant page

# APPROACH (2): using tapply or ldply instead of for loop

library(tidyverse) 
library(rvest)
library(robotstxt)
library(janitor)

# website that contains table with links to many of Dickinson's poems
ed_poems <- read_html("https://en.wikipedia.org/wiki/List_of_Emily_Dickinson_poems")

# based on the first line of poem, create a variable containing
# what is likely the URL that contains the poem's text
ed_poems_list <- (ed_poems %>%
                    html_nodes("table"))[[1]] %>%
  html_table() %>%
  clean_names() %>%
  mutate(# spaces in first line appear as underscores in URL
    firstline_a = str_replace_all(first_line_often_used_as_title, " ", "_")
    # apostraphe's appear as %27 in URL
    , firstline_b = str_replace_all(firstline_a, "'", "%27")
    , webname = paste0("https://en.wikisource.org/wiki/", firstline_b)
    # some pages end with a dash (this seems random/can't tell the pattern)
    , webname2 = paste0(webname, "_-"))

# vector with web addresses to loop through
urls1 <- ed_poems_list$webname 
head(urls1)
# vector with back-up web addresses to loop through
urls2 <- ed_poems_list$webname2 
head(urls2)

# get all the text (lapply stores it in list format)

# define function to pass through lapply
scrape_pages_func <- function(url){
  tryCatch(
    # This is what I want to do...
    {
      (url %>%               
         read_html() %>%
         html_nodes("div p") %>%   
         html_text)[1]
    },
    # ... but if an error occurs, set to Missing and keep going 
    error=function(error_message) {
      return("Missing")
    }
  )
}

all_poems_list <- lapply(urls1, scrape_pages_func)

# convert the list to a dataframe
# ldply is a function from plyr package  
all_poems2a <- plyr::ldply(.data = all_poems_list
                           , .fun = data.frame)

head(all_poems2a)

DickinsonPoems_ex2 <- all_poems2a %>%
  rename(text = "X..i..") %>%
  separate(text, into = c("first_line", "other")
           , sep = "\n", remove = FALSE) %>%
  select(first_line, other)

head(DickinsonPoems_ex2)

path_out <- "C:/Users/kcorreia/Dropbox (Amherst College)/Teaching/Fall 2020/Stat231/R Labs"
write_csv(DickinsonPoems_ex2, file = paste0(path_out,"/DickinsonPoems_ex2.csv"))
