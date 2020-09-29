humor_quotes <- read_html("https://www.brainyquote.com/topics/humor-quotes")

quotes <- humor_quotes %>%
  html_nodes(".oncl_q") %>%
  html_text()

person <- humor_quotes %>%
  html_nodes(".oncl_a") %>%
  html_text()

# put in data frame with two variables (person and quote)
humor_quotes_final <- data.frame(person = person, quote = quotes
                         , stringsAsFactors = FALSE) %>%
  mutate(together = paste('"', as.character(quote), '" --'
                          , as.character(person), sep=""))

path <- "/Users/seanwei/Desktop/STAT231-swei1999/homeworks/ps5B"
write_csv(x = humor_quotes_final, path = paste0(path, "/quotes.csv"))
