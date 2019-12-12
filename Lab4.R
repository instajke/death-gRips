# loading required packages
library(spotifyr)
library(plyr)
library(tidyverse)
library(httr)
library(rvest)
library(stringr)
library(ggthemes)
library(tidytext)
library(wordcloud)
library(ggridges)
library(RCRColorBrewer)
library(yarrr)
library(knitr)
library(kableExtra)
library(radarchart)

# set up Spotify client ID and client secret
Sys.setenv(SPOTIFY_CLIENT_ID = '4ee962d67fd240c0a46582d29db19012')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '7edfb180a5c746e2855d3b1a6ae9a760')


#using spoitfyr
dg <- get_artist_audio_features('death grips')

dg <- dg %>% filter(album_name == "The Money Store" | album_name == "No Love Deep Web" |
                              album_name == "Government Plates" | album_name == "The Powers That B" |
                              album_name == "Bottomless Pit" | album_name == "Year Of The Snitch")
# The Spotify data for Death Grips changed a little in the week between my pulling it and posting this code.



# Getting artist ID on Genius
token <- 'KNLeChZJwMONT6PEVk4IJiVowXtz2iCwtjF1SwOHvRnmRnF3-DOA7U_plSuGVSyT'

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q='
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('death grips')



# Getting track urls
baseURL <- 'https://api.genius.com/artists/'
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}



# Filtering to get urls only for tracks on which Death Grips is the primary artist
filtered_track_lyric_urls <- c()
filtered_track_lyric_titles <- c()
index <- c()


for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$primary_artist$name == "Death Grips") {
    filtered_track_lyric_urls <- append(filtered_track_lyric_urls, track_lyric_urls[[i]]$url)
    filtered_track_lyric_titles <- append(filtered_track_lyric_titles, track_lyric_urls[[i]]$title)
    
    index <- append(index, i)
  }
}

filtered_track_lyric_titles[4] <- "Artificial Death In The West"
filtered_track_lyric_titles[5] <- "Bass Rattle Stars Out The Sky"
filtered_track_lyric_titles[20] <- "Bootleg (Don't Need Your Help)"
filtered_track_lyric_titles[24] <- "Bubbles Buried In This Jungle"
filtered_track_lyric_titles[25] <- "Centuries Of Damn"
filtered_track_lyric_titles[26] <- "Come Up And Get Me"
filtered_track_lyric_titles[43] <- "Feels Like a Wheel"
filtered_track_lyric_titles[56] <- "Have A Sad Cum BB"
filtered_track_lyric_titles[59] <- "Hunger Games"
filtered_track_lyric_titles[61] <- "I Break Mirrors With My Face In The United States"
filtered_track_lyric_titles[62] <- "I'm Overflow"
filtered_track_lyric_titles[70] <- "I've Seen Footage"
filtered_track_lyric_titles[76] <- "Linda's In Custody"
filtered_track_lyric_titles[78] <- "Lock Your Doors"
filtered_track_lyric_titles[90] <- "Ring A Bell"
filtered_track_lyric_titles[120] <- "This Is Violence Now (Don't Get Me Wrong)"                                                                                       
filtered_track_lyric_titles[121] <- "Three Bedrooms In A Good Neighborhood"
filtered_track_lyric_titles[131] <- "Whatever I Want (Fuck Who's Watching)"
filtered_track_lyric_titles[133] <- "Why A Bitch Gotta Lie"
filtered_track_lyric_titles[134] <- "World Of Dogs"
filtered_track_lyric_titles[136] <- "You Might Think He Loves You for Your Money but I Know What He Really Loves You for It's Your Brand New Leopard Skin Pillbox Hat"



dg_lyrics <- data.frame(filtered_track_lyric_urls, filtered_track_lyric_titles)
dg_lyrics <- dg_lyrics[filtered_track_lyric_titles %in% dg$track_name, ]

dg_lyrics$filtered_track_lyric_urls <- as.character(dg_lyrics$filtered_track_lyric_urls)
dg_lyrics$filtered_track_lyric_titles <- as.character(dg_lyrics$filtered_track_lyric_titles)

# Webscraping lyrics using rvest
lyric_text <- rep(NA, nrow(dg_lyrics))

lyric_scraper <- function(url) {
  read_html(url) %>%
    html_node('.lyrics p') %>%
    html_text
}

for (i in 1:nrow(dg_lyrics)) {
  lyric_text[i] <- lyric_scraper(dg_lyrics$filtered_track_lyric_urls[i])
}

# Cleaning and standardizing lyrics
for (i in 1:nrow(dg_lyrics)) {
  lyric_text[i] <- gsub("([a-z])([A-Z])", "\\1 \\2", lyric_text[i])
  lyric_text[i] <- gsub("\n", " ", lyric_text[i])
  lyric_text[i] <- gsub("\\[.*?\\]", " ", lyric_text[i])
  lyric_text[i] <- tolower(lyric_text[i])
  lyric_text[i] <- gsub("[ [:punct:] ]", " ", lyric_text[i])
  lyric_text[i] <- gsub(" {2,}", " ", lyric_text[i])
}

genius_data <- data.frame(track_name = dg_lyrics$filtered_track_lyric_titles, lyrics = lyric_text)
genius_data$track_name <- as.character(genius_data$track_name)
genius_data$lyrics <- as.character(genius_data$lyrics)

# joining Spotify and Genius data
spotify_genius <- full_join(genius_data, dg, by = "track_name")

# adding "ordered_albums", with album names as factors
ordered_albums <- factor(spotify_genius$album_name)
ordered_albums <- factor(ordered_albums,levels(ordered_albums)[c(4,3,2,5,1,6)]) 
spotify_genius$ordered_albums <- ordered_albums

# valence ridge plot (I used fig.height = 6, fig.width = 6 in an rmd)
spotify_genius %>% ggplot(aes(x = valence, y = ordered_albums, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.9) +
  scale_fill_gradient(low = "white", high = "maroon3") +
  theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0,1) +
  theme(legend.position = "none")

# table: album by mean valence
spotify_genius %>%
  group_by(album_name) %>%
  summarise(mean(valence)) %>%
  arrange(desc(`mean(valence)`)) %>%
  kable() %>%
  kable_styling("striped", full_width = F, position = "left") %>%
  row_spec(row = 1:6, bold=T, color = "white", background = "#D7261E")

# table: top 5 songs by valence
spotify_genius %>%
  select(track_name, album_name, valence) %>%
  top_n(5) %>%
  arrange(-valence) %>%
  kable() %>%
  kable_styling("striped", full_width = F, position = "left") %>%
  row_spec(row = 1:5, bold=T, color = "white", background = "#D7261E")

# sonic score graph
pirateplot(valence + danceability + energy ~ ordered_albums, spotify_genius,
           pal = "southpark",
           xlab = "album", ylab = "sonic score",
           theme = 0, point.o = 0.7, avg.line.o = 1, jitter.val = .05,
           bty = "n", cex.axis = 0.6, xaxt = "n")
axis(1, cex.axis = 0.6, lwd = 0)
legend("bottomleft", c("1: The Money Store", "2:  No Love Deep Web", "3: Government Plates", "4: The Powers That B", "5: Bottomless Pit", "6: Year Of The Snitch"), bty = "n", cex = 0.6)

# The Money Store sonic scores
spotify_genius %>%
  group_by(track_name, album_name) %>%
  mutate(sonic_score = valence + danceability + energy) %>%
  select(album_name, track_name, sonic_score) %>%
  arrange(desc(sonic_score)) %>%
  filter(album_name == "The Money Store") %>%
  kable() %>%
  kable_styling(full_width = F, position = "left") %>%
  row_spec(row = 1:13,  bold=T, color = "white", background = "#D7261E")

# YOTS sonic scores
spotify_genius %>%
  group_by(track_name, album_name) %>%
  mutate(sonic_score = valence + danceability + energy) %>%
  select(album_name, track_name, sonic_score) %>%
  arrange(desc(sonic_score)) %>%
  filter(album_name == "Year Of The Snitch") %>%
  kable() %>%
  kable_styling(full_width = F, position = "left") %>%
  row_spec(row = 1:13,bold=T, color = "white", background = "#D7261E")

# album by energy
spotify_genius %>%
  group_by(album_name) %>%
  summarise(mean(energy)) %>%
  arrange(desc(`mean(energy)`)) %>%
  kable() %>%
  kable_styling(full_width = F, position = "left") %>%
  row_spec(row = 1, bold=T, color = "white", background = "#D7261E")

# tokenized and cleaned datasets of lyrics for textual analysis
tidy_dg <- spotify_genius %>% unnest_tokens(word, lyrics)
tidier_dg <- tidy_dg %>% anti_join(rbind(stop_words[1], 
                                         "uh", "yeah", "hey", "baby", "ooh", "wanna", 
                                         "gonna", "ah", "ahh", "ha", "la", "mmm", "whoa", 
                                         "haa", "dah", "di", "blo", "doe", "aye", "poe", 
                                         "yo", "gotta", "em", "wah", "til"))
tidier_dg$word[tidier_dg$word == "don" | tidier_dg$word == "didn"] <- NA
tidier_dg$word[tidier_dg$word == "ain"] <- NA
tidier_dg$word[tidier_dg$word == "isn"] <- NA
tidier_dg$word[tidier_dg$word == "usin"] <- "using"
tidier_dg$word[tidier_dg$word == "wouldn"] <- "wouldn't"
tidier_dg$word[tidier_dg$word == "couldn"] <- "couldn't"
tidier_dg$word[tidier_dg$word == "shouldn"] <- "shouldn't"
tidier_dg$word[tidier_dg$word == "won"] <- "won't"
tidier_dg$word[tidier_dg$word == "ve" | tidier_dg$word == "ll"] <- NA

word_count <- tidier_dg %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ungroup()

word_count <- na.omit(word_count)

wordcloud(words = word_count$word, freq = word_count$n,
          max.words=100, random.order=FALSE,
          colors= brewer.pal(n = 8, name = "Dark2"))

# how many tracks does the word "fuck" appear in?
tidier_dg %>%
  select(track_name, word) %>%
  filter(word == "fuck") %>%
  unique() %>%
  select(track_name)

# wordcloud: The Money Store
word_count_ms <- tidier_dg %>%
  filter(album_name == "The Money Store") %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ungroup()

word_count_ms <- na.omit(word_count_ms)

wordcloud(words = word_count_ms$word, freq = word_count_ms$n,
          max.words=25, random.order=FALSE,
          colors= brewer.pal(n = 8, name = "Dark2"))

# wordcloud: Year Of The Snitch
word_count_yots <- tidier_dg %>%
  filter(album_name == "Year Of The Snitch") %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ungroup()

word_count_yots <- na.omit(word_count_yots)

wordcloud(words = word_count_yots$word, freq = word_count_yots$n,
          max.words=25, random.order=FALSE,
          colors= brewer.pal(n = 8, name = "Dark2"))

tidier_dg$album_release_year <- as.character(tidier_dg$album_release_year)
tidier_dg$album_release_year <- as.numeric(substr(tidier_dg$album_release_year, 1, 4))

tidy_dg$album_release_year <- as.character(tidy_dg$album_release_year)
tidy_dg$album_release_year <- as.numeric(substr(tidy_dg$album_release_year, 1, 4))

# creating a "lexical diversity" dataset
lexical_diversity <- tidy_dg %>% group_by(track_name, ordered_albums) %>%
  mutate(lex_div = length(unique(word))/length(word)) %>%
  select(track_name, lex_div, ordered_albums) %>%
  distinct()

# lexical diversity plot
pirateplot(lex_div ~ ordered_albums, lexical_diversity,
           pal = "basel",
           xlab = "album", ylab = "lexical diversity",
           theme = 0, point.o = 0.5, avg.line.o = 1, jitter.val = .05,
           bty = "n", cex.axis = 0.6, xaxt = "n")
axis(1, cex.axis = 0.6, lwd = 0)
legend("topleft", c("1: The Money Store", "2:  No Love Deep Web", "3: Government Plates", "4: The Powers That B", "5: Bottomless Pit", "6: Year Of The Snitch"), bty = "n", cex = 0.6)

# least lexically diverse tracks
tidy_dg %>% group_by(track_name, album_name) %>%
  mutate(lex_div = length(unique(word))/length(word)) %>%
  select(track_name, lex_div, album_name) %>%
  arrange(lex_div) %>%
  distinct() %>%
  head(5) %>%
  kable() %>%
  kable_styling(full_width = F, position = "left") %>%
  row_spec(row = 1:5, bold=T, color = "white", background = "#D7261E")

# most lexically diverse tracks
tidy_dg %>% group_by(track_name, album_name) %>%
  mutate(lex_div = length(unique(word))/length(word)) %>%
  select(track_name, lex_div, album_name) %>%
  arrange(desc(lex_div)) %>%
  distinct() %>%
  head(5) %>%
  kable() %>%
  kable_styling(full_width = F, position = "left") %>%
  row_spec(row = 1:5, bold=T, color = "white", background = "#D7261E")

# joining the tokenized, tidied lyric dataset with sentiment lexicons
dg_nrc_sub <- tidier_dg %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

dg_AFINN <- tidier_dg %>%
  inner_join(get_sentiments("afinn"))

dg_bing <- tidier_dg %>%
  inner_join(get_sentiments("bing"))

# sentiment scores using AFINN
dim <- dg_AFINN %>%
  count(album_name)

dg_AFINN %>%
  group_by(ordered_albums) %>%
  summarise(sum(value)) %>%
  mutate(scaled = `sum(value)` * 229 / dim$n) %>%
  ggplot(aes(x = ordered_albums, y = scaled, fill = ordered_albums)) +
  geom_bar(stat = "identity") +
  ylim(-1000, 1000) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = brewer.pal(n = 6, name = "Dark2")) +
  theme(legend.position="none")

# The Money Store pyramid plot
sent_dg_ms <- dg_bing %>%
  unique() %>%
  group_by(track_name, sentiment, album_name) %>%
  count(track_name, sentiment) %>%
  filter(album_name == "The Money Store")

for(i in 1:nrow(sent_dg_ms)) {
  if(sent_dg_ms$sentiment[i] == "negative")
    sent_dg_ms$n[i] <- -sent_dg_ms$n[i]
}

sent_dg_ms %>%
  ggplot(aes(x = track_name, y = n, fill = sentiment)) +
  geom_bar(subset = .(sentiment == "positive"), stat = "identity") +
  geom_bar(subset = .(sentiment == "negative"), stat = "identity") +
  scale_y_continuous(breaks = seq(-20, 20, 5)) +
  coord_flip() +
  theme_fivethirtyeight() +
  ylim(-35,10) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = brewer.pal(n = 2, name = "Set1")) +
  theme(legend.position="none")

# all-album radar chart
sentiment_nrc <- dg_nrc_sub %>%
  group_by(ordered_albums, sentiment) %>%
  count(ordered_albums, sentiment) %>%
  select(ordered_albums, sentiment, sentiment_total = n)

album_nrc <- dg_nrc_sub %>%
  count(ordered_albums) %>%
  select(ordered_albums, album_total = n)

radar_chart <- sentiment_nrc %>%
  inner_join(album_nrc, by = "ordered_albums") %>%
  mutate(percent = round((sentiment_total/album_total * 100), 3)) %>%
  select(-sentiment_total, -album_total) %>%
  spread(ordered_albums, percent)

radar_chart <- radar_chart[c(2,7,5,8,4,3,1,6), c(1, 7:2)]

chartJSRadar(radar_chart, polyAlpha = 0.1, lineAlpha = 0.8, maxScale = 25,
             colMatrix = matrix(c(0, 255, 255, 255, 185, 15, 139, 0, 139,
                                  255, 0, 0, 201, 167, 198, 0, 0, 0), byrow = F, nrow = 3))
