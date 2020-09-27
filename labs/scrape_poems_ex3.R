# scrape Emily Dickinson poems from Wikipedia
# first, get list of URLs of Dickinson poem pages
# then, scrape text on each relevant page

# APPROACH (3): using an html_node that pulls the correct link directly

library(tidyverse) 
library(rvest)
library(robotstxt)
library(janitor)

# check that we have permission to scrape
paths_allowed("https://en.wikipedia.org")

# website that contains table with links to many of Dickinson's poems
ed_poems <- read_html("https://en.wikipedia.org/wiki/List_of_Emily_Dickinson_poems")

# pull URL directly (don't use table node)
# used CSS selectorGadget to identify "td a" as relevant selector

# (ed_poems %>% html_nodes("td a"))[[1]]

# html_attr() pulls specific attributes; in this case, we want the href
# which provides the URL
ed_urls <- ed_poems %>%
  html_nodes("td a") %>%
  html_attr('href')

ed_titles <- ed_poems %>%
  html_nodes("td a") %>%
  html_attr('title')


## use ldply directly so get into data frame 
all_poems_text <- plyr::ldply(.data = ed_urls
                              , .fun = function(url){
                                tryCatch(
                                  # This is what I want to do...
                                  {(url %>%               
                                   read_html() %>%
                                   html_nodes("div p") %>%   
                                   html_text)[1]
                                  },
                                  # ... but if an error occurs, set to Missing and keep going 
                                  error=function(error_message) {
                                    return("Missing")
                                  }
                                )
                              })


# add first line (often used as title)
DickinsonPoems_ex3 <- all_poems_text %>%
  mutate(title = str_sub(ed_titles,start=3)) %>%
  select(title, text=V1)

head(DickinsonPoems_ex3)

path_out <- "C:/Users/kcorreia/Dropbox (Amherst College)/Teaching/Fall 2020/Stat231/R Labs"
write_csv(DickinsonPoems_ex3, path = paste0(path_out,"/DickinsonPoems.csv"))
