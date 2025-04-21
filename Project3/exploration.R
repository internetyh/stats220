library(tidyverse)
library(httr)
library(magick)

# function to find the most common color from a hex string
most_color <- function(hex_str) {
  
  # removes the # at the start of a hex string
  hex_str <- substr(hex_str, 2, 7)
  
  # getting the parts of the hex string
  str_red <- substr(hex_str, 1, 2)
  str_blue <- substr(hex_str, 3, 4)
  str_green <- substr(hex_str, 5, 6)
  
  # turning the parts into base 10
  red <- as.numeric(as.hexmode(str_red))
  blue <- as.numeric(as.hexmode(str_blue))
  green <- as.numeric(as.hexmode(str_green)) 
  
  # getting the most common color and the position to return the color
  colors <- c(red, blue, green)
  most_color <- max(colors)
  return (most_color / sum(colors))
  
}


api_key <- "2Mqpbjk1WNNf3bsNN5cU67pthlZPNBi5MiKs8uUnwscAlQ1FpNrL2zXb"

url <- "https://api.pexels.com/v1/search?query=crispy%20potatoes&per_page=80"

response <- httr::GET(url, 
                      add_headers(Authorization = api_key))

data <- httr::content(response, 
                      as = "parsed", 
                      type = "application/json")

photo_data <- tibble(photos = data$photos) %>%
  unnest_wider(photos) %>%
  unnest_wider(src)

# variable 1 - display ratio (width / height)
# variable 2 - if the alt string (describes the image) has the word fries
# variable 3 - the percentage of the average color that belongs to the most common color
# filters based on images with an average color that is at least 44% the most common color
selected_photos <- photo_data %>% 
  group_by(avg_color) %>%
  mutate(
    display_ratio = width / height, 
    alt_has_fries = str_detect(str_to_lower(alt), "fries"),
    most_color = most_color(avg_color)
    ) %>%
  filter(most_color > 0.44)

write_csv(selected_photos, "selected_photos.csv")

