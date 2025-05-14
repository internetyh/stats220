library(tidyverse)
library(jsonlite)
song_data <- fromJSON("https://stat.auckland.ac.nz/~fergusson/stats220_S124/data/lab4A.json")

summarised_data <- song_data %>%
  mutate(valence_group = case_when(
    valence < 0.33 ~ 'sad',
    valence < 0.67 ~ 'OK',
    TRUE ~ 'happy')) %>%
  group_by(mode_name, valence_group) %>%
  summarise(mean_tempo = mean(tempo, na.rm = TRUE))

summarised_data %>%
  ggplot() +
geom_point(aes(x = mean_tempo, y = mode_name, color = valence_group), size = 5)