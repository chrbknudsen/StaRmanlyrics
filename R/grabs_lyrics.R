library(rvest)
library(xml2)

# URL til sangteksten
url <- "https://genius.com/David-bowie-a-better-future-lyrics"

# Hent websiden
page <- read_html(url)

page %>% html_nodes("body") %>%
  html_nodes("#application") %>%
  html_nodes("#lyrics-root") %>%
  html_nodes('[data-lyrics-container="true"]') %>%
    html_text2(preserve_nbsp = T)


