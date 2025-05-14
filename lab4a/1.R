library(tidyverse)
library(jsonlite)
song_data <- fromJSON("https://stat.auckland.ac.nz/~fergusson/stats220_S124/data/lab4A.json")

summarised_data <- song_data %>%
mutate(song_speed = ifelse(tempo > 120, 'fast', 'slow')) %>%
  group_by(song_speed) %>%
  summarise(n = n())

ggplot(data = summarised_data) +
  geom_col(aes(x = song_speed, y = n, fill = song_speed)) +
  labs(title = 'Comparing the speed of songs', x = 'Speed of song based on tempo', y = 'Number of songs')