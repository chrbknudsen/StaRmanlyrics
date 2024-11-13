
# packages og library -----------------------------------------------------

library(tidyverse)
library(rvest)
library(xml2)


# høste sangtekster -------------------------------------------------------

#url til en test sang
eksempel_sang_url <- "https://genius.com/David-bowie-space-oddity-lyrics"

#indlæser siden med sang teksten
sang_html <- read_html(eksempel_sang_url)

# henter relevante div med sangtekst
xml_div <- sang_html %>%
  html_nodes("div[data-lyrics-container = 'true']")



#%>% html_text2() %>% writeLines()

div_sang_html <- sang_html %>%
  html_nodes("div[data-lyrics-container = 'true']") %>%
  html_text2() %>% writeLines()

span_sang_html <- sang_html %>%
  html_nodes("div[data-lyrics-container = 'true']") %>%
  html_nodes("span[class = 'ReferentFragmentdesktop__Highlight-sc-110r0d9-1 jAzSMw']") %>%
  html_text2() %>% writeLines()


div_sang_html
span_sang_html


