library(magick)

## |> piping makes it much easier since its all just one object anyway
image_read("https://i.kym-cdn.com/entries/icons/facebook/000/037/158/thinkmarkthumbnail.jpg") |>
  image_annotate("Why can't you think of a meme Mark", size=70, gravity="North", color="White") |>
  image_annotate("Think Mark Think!", size=70, gravity="south", color="White") |>
  image_write("my_meme.png")
