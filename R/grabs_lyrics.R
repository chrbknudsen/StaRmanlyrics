library(tidyverse)
library(rvest)
library(xml2)

# URL til sangteksten
url <- "https://genius.com/David-bowie-space-oddity-lyrics"

# Hent websiden


lyrics_from_url <- function(url){
  page <- read_html(url)
  page %>% html_nodes("body") %>%
  html_nodes("#application") %>%
  html_nodes("#lyrics-root") %>%
  html_nodes('[data-lyrics-container="true"]') %>%
  as.character() %>%
  str_remove_all(pattern = "<a href.*?>") %>%
  str_remove_all(pattern = "<\\a>") %>%
  str_remove_all(pattern = "<span.*?>") %>%
  str_remove_all(pattern = "<\\span>") %>%
  paste0(collapse = "\n") %>%
  read_html() %>%
  html_text2(preserve_nbsp = TRUE) %>%
  str_split("\\n") %>%
    unlist()}

lyrics_til_oprensning <- lyrics_from_url(url)


<span.*?>

  clipr::write_clip()
  str_remove_all(pattern = "")


  # Vi laver noderne om til HTML-strenge
  paste0(collapse = "\n") %>%
#   str_replace_all("<br\\s*/?>", "\n") %>%





