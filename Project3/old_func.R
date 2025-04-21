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
  index <- match(most_color, colors)
  
  # returning the most common color
  if (index == 1) {
    return("red")
  } else if (index == 2) {
    return("blue")
  } else if (index == 3) {
    return("green")
  } else {
    return("N/A")
  }
  
}