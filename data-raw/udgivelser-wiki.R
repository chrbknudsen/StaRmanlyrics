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

# grabser tabeller fra wikipedia.
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


# Primære studie albums.

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



# search_spotify()
# Genererer Spotify søgestreng, og søger i Spotify
test <- test %>%
  mutate(spot_string = str_c("year:",year, " artist:David Bowie album:",Title )) %>%
  mutate(spot_data = map(spot_string, search_spotify, type = "album",
                         authorization = get_spotify_access_token(client_id = hemmeligheder[["client_id"]],
                                                                  client_secret = hemmeligheder[["client_secret"]])))







test %>%
  unnest(spot_data) %>%
  filter(album_type %in% c("album", "compilation")) %>%
  view()

# henter de individuelle sange fra et givet album med metadata og andet godt.
res <- spotifyr::get_album_tracks("4h9rWFWhgCSSrvIEQ0YhYG",
                           authorization = get_spotify_access_token(client_id = hemmeligheder[["client_id"]],
                                                                    client_secret = hemmeligheder[["client_secret"]]))
TMWSTW <- res
view(TMWSTW)



# bowies artist id hos genius 9534
# her tjekker vi at vi har en api-token liggende. Den skal sættes som
# system variabel. der er nok mere fikse måder.
Sys.getenv('GENIUS_API_TOKEN')



Sys.setenv(GENIUS_API_TOKEN = "")
# er det her noget der fejler. Eller
# er der bare overhovedet ikke tålmodighed nok i Christian
# disk <- get_discography_lyrics('9534')
# eller

#oplysninger om bowie
noget <- get_genius_artist('9534')
get_discography_lyrics

# kan vi få diskografien?
# det kan vi. Man skal bare have 42% mere tålmodighed end
# Christian har.
diskografien <- get_genius_artist_songs('9534')

write_csv(diskografien, "data-raw/diskografien.csv")
diskografien %>% view()



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

eget_kald$response$song
class(eget_kald$response$song$path)
# Kigger man nærmere på koden i rgenius kan man se at lyrics bliver trukket
# ud ved at webscrape. Hvis genius har ændret på deres hjemmeside struktur
# inden for de seneste to år (og det har de nok...), så må vi forvente at det
# er derfor vi, når det går bedst, får "producer" ud som sangtekst.

