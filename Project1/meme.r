library(magick)

add_text <- function(image, textColor) {
  image2 <- image %>% 
    image_annotate("Why can't you think of a meme Mark", size = 70, gravity = "North", color = textColor) %>%
    image_annotate("Think Mark Think!", size = 70, gravity = "South", color = textColor)
  
  return(image2)
}

## |> piping makes it much easier since its all just one object anyway
image <- image_read("https://i.kym-cdn.com/entries/icons/facebook/000/037/158/thinkmarkthumbnail.jpg")
image <- add_text(image, "White")

image %>% image_write("my_meme.png")
