url <- "https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States"
justices <- url %>% 
  read_html() %>% 
  html_nodes("table")
justices <- html_table(justices[[2]], fill = TRUE)

path <- "/Users/seanwei/Desktop/STAT231-swei1999/homeworks/ps5B"
write_csv(x = justices, path = paste0(path, "/justices.csv"))