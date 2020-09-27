# scrape Emily Dickinson poems from Wikipedia
# first, get list of Dickinson poems
# then, identify patterns in Wikipedia page links for the poems
# then, scrape text on each relevant page

# APPROACH (1): identify pattern, then use for loop to loop through URLs

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

# initialize data frame where will store poem text
all_poems <- data.frame(name = ed_poems_list$first_line_often_used_as_title
                        , text = ""
                        , stringsAsFactors = FALSE)

for (i in 1:length(urls1)){
  
  # try to get poem text via URL based on"poem_names" vector
  all_poems[i,2] <- tryCatch(
    # This is what I want to do...
    {
      (urls1[i] %>%               
         read_html() %>%
         html_nodes("div p") %>%   
         html_text)[1]
    },
    # ... but if an error occurs, set to Missing and keep going 
    error=function(error_message) {
      return("Missing")
    }
  )
  
  # if missing the text, try pulling from back-up URL 
  if(all_poems[i,2]=="Missing"){
    all_poems[i,2] <- tryCatch(
      # This is what I want to do...
      {
        (urls2[i] %>%               
           read_html() %>%
           html_nodes("div p") %>%   
           html_text)[1]
      },
      # ... but if an error occurs, set to Still Missing and keep going 
      error=function(error_message) {
        return("Still Missing")
      }
    )
  }
  
}

# see how many are still missing
test1 <- which(all_poems$text == "Still Missing")
length(test1)

# merge in year first published
DickinsonPoems_ex1 <- ed_poems_list %>%
  select(name = first_line_often_used_as_title
         , pub_year = x1st) %>%
  inner_join(all_poems, by = "name")
             
head(DickinsonPoems_ex1)

path_out <- "C:/Users/kcorreia/Dropbox (Amherst College)/Teaching/Fall 2020/Stat231/R Labs"
write_csv(DickinsonPoems_ex1, file = paste0(path_out,"/DickinsonPoems_ex1.csv"))
