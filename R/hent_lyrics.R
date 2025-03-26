library(tidyverse)
library(rvest)
library(xml2)
library(httr)
library(spotifyr)
library(rgenius)
library(tidytext)

# vi har diskografien.csv.. Den er genereret fra data-raw/releases_wiki.R.
# Den har vi fået ved rgenius::diskografien <- get_genius_artist_songs(9534)

# og den kan indlæses ved:
disko <- read_csv("data-raw/diskografien.csv")

# a vector containing all song-ids. Vi skal bruge walk-funktionen.
song_id <- 236734
# Det kan vi bruge til at hente metadata.

hemmeligheder <- readRDS("hemmeligheder.Rds")

Sys.setenv(GENIUS_API_TOKEN = hemmeligheder$genius)

# henter metadata om en sang
response <- GET(url = str_glue("https://api.genius.com/songs/{song_id}"),
                query = list(access_token = Sys.getenv('GENIUS_API_TOKEN')), quiet = TRUE)

#viser indholdet af song_meta_data
song_meta_data <- response %>%
  content()

# gemmer meta-data som rds.fil
song_meta_data %>%
  as_tibble() %>%
  write_rds(str_glue("data-raw/meta-raw/meta_{song_id}.rds"))

# og i disse metadata vi også finde albumnavnet. Så det skal vi have trukket ud.
# det skal vi have gemt til en dataframe
#
# blandt disse meta data er der en relativ sti til genius-hjemmesiden

# viser album-titlen uden kunstneren i parentes
# song_meta_data$response$song$album$name

# absolut sti til en unik sang.
song_path <- paste0("https://genius.com",song_meta_data$response$song$path)

# her har vi en funktion, der ud fra en url kan trække sangteksten
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

# sangtekst
song_lyrics <- lyrics_from_url(song_path)

# sangteksten skrives til en fil, som ligger i data-raw/lyrics-raw
write(song_lyrics, tolower(str_glue("data-raw/lyrics-raw/{song_meta_data$response$song$id}_{song_meta_data$response$song$title}.txt")))


