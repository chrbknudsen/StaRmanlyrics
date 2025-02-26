vi har diskografien.csv.
Den har vi fået ved rgenius::diskografien <- get_genius_artist_songs(9534)

og den kan indlæses ved:
disko <-   read_csv("data-raw/diskografien.csv")

der er et sang-id.

Det kan vi bruge til at hente metadata.

response <- GET(url = str_glue("https://api.genius.com/songs/{song_id}"),
                query = list(access_token = Sys.getenv('GENIUS_API_TOKEN')), quiet = TRUE)


response %>% content()

og i disse metadata vi også finde albumnavnet. Så det skal vi have trukket ud.
det skal vi have gemt til en dataframe

blandt disse meta data er der en relativ sti til genius-hjemmesiden
eget_kald$response$song$path
den kan vi lave til en absolut sti.
paste0("https://genius.com",eget_kald$response$song$path)

og så kan vi bruge
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

til at få teksten ud.
