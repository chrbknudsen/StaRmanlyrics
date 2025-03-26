library(tidyverse)
library(rvest)
library(xml2)
library(httr)
library(spotifyr)
library(rgenius)
library(tidytext)

# Henter hemmeligheder til spotify
# Denne fil er på gitignore. Ønsker du selv at lave
# den, skal du lave en liste med client_id og client_secret,
# og gemme den med saveRDS()
# HUSK HUSK HUSK AT SÆTTE DEN PÅ GITIGNORE!!!!!
# Ellers ender den på github. Det er bøvlet at fjerne den derfra igen.
# Og så vil vi skulle oprette nye spotify hemmeligheder...
hemmeligheder <- readRDS("hemmeligheder.Rds")

# grabser tabeller fra wikipedia. ----
# der er behov for en del oprydning

diskografi <- "https://en.wikipedia.org/wiki/David_Bowie_discography"
tabeller <- read_html(diskografi) %>%
  html_table()

primary_studio_albums                  <- tabeller[[2]]
unreleased_studio_albums               <- tabeller[[3]]
re_recorded_studio_albums              <- tabeller[[4]]
studio_albums_as_member_of_tin_machine <- tabeller[[5]]
live_albums                            <- tabeller[[6]]
live_albums_as_member_of_tin_machine   <- tabeller[[7]]
soundtrack_albums                      <- tabeller[[8]]
compilation_albums_1970s               <- tabeller[[9]]
compilation_albums_1980s               <- tabeller[[10]]
compilation_albums_1990s               <- tabeller[[11]]
compilation_albums_2000s               <- tabeller[[12]]
compilation_albums_2010s               <- tabeller[[13]]
era_box_sets                           <- tabeller[[14]]
other_box_sets                         <- tabeller[[15]]
ep                                     <- tabeller[[16]]
singles_1960s                          <- tabeller[[17]]
singles_1970s                          <- tabeller[[18]]
singles_1970s_promotional              <- tabeller[[19]]
singles_1980s                          <- tabeller[[20]]
singles_1990s                          <- tabeller[[21]]
singles_2000s                          <- tabeller[[22]]
singles_2010s                          <- tabeller[[23]]
singles_2020s                          <- tabeller[[24]]
singles_as_member_of_tin_machine       <- tabeller[[25]]
studio_contributions                   <- tabeller[[26]]
live_contributions                     <- tabeller[[27]]
guest_apperances                       <- tabeller[[28]]
remixes_and_alternate_versions         <- tabeller[[29]]


# Data primære studie albums. ----

# Isolerer år og titel mhp at trække data fra spotify
# Manuel redigering af "Toy" som Spotify mener er fra 2022
# Af "Pin Ups" som de kalder for "PinUps" og
# "Let's Dance", der skal søges som "lets dance". Formentlig fordi
# apostroffen giver bøvl.
# Og så er der The Buddha of Suburbia, hvor Spotify ikke anerkender "the"

test <- primary_studio_albums %>%
  select(Title, details=`Album details`) %>%
  filter(str_detect(details, "Released")) %>%
  mutate(year = str_extract(details, "\\d{4}")) %>%
  mutate(year = case_when(
    Title == "Toy" ~ "2022",
    .default = year)) %>%
  mutate(Title = case_when(
    Title == "Pin Ups" ~ "pinups",
    Title == "Let's Dance" ~ "lets dance",
    Title == "The Buddha of Suburbia" ~ "Buddha of Suburbia",
    .default = Title)
    )



# search_spotify() ----
# test indeholder nu de data vi kan trække fra spotify (på studiealbum)
# Genererer Spotify søgestreng, og søger i Spotify
test <- test %>%
  mutate(spot_string = str_c("year:",year, " artist:David Bowie album:",Title )) %>%
  mutate(spot_data = map(spot_string, search_spotify, type = "album",
                         authorization = get_spotify_access_token(client_id = hemmeligheder[["client_id"]],
                                                                  client_secret = hemmeligheder[["client_secret"]])))






test %>%
  unnest(spot_data) %>%
  filter(album_type %in% c("album", "compilation"))

# henter de individuelle sange fra et givet album med metadata og andet godt. ----
# album id kommer fra spot_data kolonnen i test. Den ligger i et felt der hedder id.
res <- spotifyr::get_album_tracks("4h9rWFWhgCSSrvIEQ0YhYG",
                           authorization = get_spotify_access_token(client_id = hemmeligheder[["client_id"]],
                                                                    client_secret = hemmeligheder[["client_secret"]]))
TMWSTW <- res



# nu gør vi ting på genius. ----
# bowies artist id hos genius 9534
# her tjekker vi at vi har en api-token liggende. Den skal sættes som
# system variabel. der er nok mere fikse måder.

hemmeligheder <- readRDS("hemmeligheder.Rds")


Sys.setenv(GENIUS_API_TOKEN = hemmeligheder$genius)



disk <- get_discography_lyrics('9534')

# er det her noget der fejler. Eller
# er der bare overhovedet ikke tålmodighed nok i Christian
# disk <- get_discography_lyrics('9534')
# eller

#oplysninger om bowie
# noget <- get_genius_artist('9534')
get_discography_lyrics

# kan vi få diskografien?
# det kan vi. Man skal bare have 42% mere tålmodighed end
# Christian har.
# Man skal også undgå at blive ratelimited...
diskografien <- get_genius_artist_songs(9534)

# write_csv(diskografien, "data-raw/diskografien.csv")
read_csv("data-raw/diskografien.csv")
# Og har vi så links til lyrics siderne. Som vi pt. ikke har adgang til, fordi
# vi ikke er lige så høflige når vi høster som Søren W


# Nu gør vi det selv!
song_id <-'4193889'


response <- GET(url = str_glue("https://api.genius.com/songs/{song_id}"),
                query = list(access_token = Sys.getenv('GENIUS_API_TOKEN')), quiet = TRUE)
response %>% content()

# Det bør give os et resultat hvor vi har en url til siden med
# sangteksten

eget_kald <- response %>% content()

# dette ser ud til at være den relative sti til lyrics hos genius.
eget_kald$response$song$path
paste0("https://genius.com",eget_kald$response$song$path)



eget_kald$response$song
class(eget_kald$response$song$path)
# Kigger man nærmere på koden i rgenius kan man se at lyrics bliver trukket
# ud ved at webscrape. Hvis genius har ændret på deres hjemmeside struktur
# inden for de seneste to år (og det har de nok...), så må vi forvente at det
# er derfor vi, når det går bedst, får "producer" ud som sangtekst.


# vi vil gerne have samme struktur som i taylor pakken.
library(taylor)
taylor::taylor_all_songs %>%
#  filter(str_detect(track_name, "Fearless")) %>%
  pull(lyrics) %>%
  bind_rows() %>% str()

# så vi skal frem til at lyrics er en list-column, med en tibble pr. sang.
# Og fire kolonner:
#   line int
# lyric chr
# element chr
# element_artist chr
# vi kan notere at hos taylor pakken er der ingen andre element artister end taylor.


# status ----


# Vi kan få fat på alle urls til de bowie sange vi kan få hos genius. Vi skal måske
# lige tale med Søren W om hvordan vi gør det uden at blive ratelimited...

# vi har faktisk alle de urls liggende i diskografien.

# Vi skal derefter have skrabet lyrics fra de urls, og gemt dem i en tibble med
# ovenstående struktur

# Det gør vi nok med fordel én gang, gemmer og arbejder videre...

# hver af de tibbler skal så ind i en list column i en tibble med disse kolonner:

# [1] "album_name"          "ep"                  "album_release"       "track_number"        "track_name"
# [6] "artist"              "featuring"           "bonus_track"         "promotional_release" "single_release"
# [11] "track_release"       "danceability"        "energy"              "key"                 "loudness"
# [16] "mode"                "speechiness"         "acousticness"        "instrumentalness"    "liveness"
# [21] "valence"             "tempo"               "time_signature"      "duration_ms"         "explicit"
# [26] "key_name"            "mode_name"           "key_mode"            "lyrics"


# har vi noget vi kan komme videre med? ----
TMWSTW
library(spotifyr)
hm <- spotifyr::get_artist_audio_features(artist = "David Bowie", authorization = get_spotify_access_token(client_id = hemmeligheder[["client_id"]],
                                                                                                     client_secret = hemmeligheder[["client_secret"]]))
# og det betyder faktisk at vi har størstedelen af data på bowie sangene nu.
hemmeligheder
install.packages("spotifyr")
view(hm)
library(tidyverse)
