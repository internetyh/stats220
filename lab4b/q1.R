library(tidyverse)
library(jsonlite)

song_data <- fromJSON("https://stat.auckland.ac.nz/~fergusson/stats220_S124/data/lab4B.json")

genre_data <- song_data %>%
  separate_rows(artist_genre, sep = ",") %>%
  group_by(artist_genre) %>%
  filter(n() >= 10)

summarised_data <- genre_data %>% group_by(artist_genre) %>% summarise(mean_danceability = mean(danceability, na.rm=TRUE))

ggplot() +   geom_point(data = summarised_data,
                        aes(x = mean_danceability,
                            y = reorder(artist_genre, mean_danceability)),
                        shape = 17,
                        colour = "red",
                        size = 4) +
geom_jitter(data = genre_data,
            aes(x = danceability,
                y = artist_genre),
            height = 0.3,
            colour = "pink",
            size = 2) +
  labs(title = "Comparing danceability of songs by genre",
      y = "Genre of song artist",
      x = "Danceability of song") +
  theme_minimal()