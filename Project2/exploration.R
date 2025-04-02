library(tidyverse)

logged_data <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSUiUAyo4i76UMx-gIXi7-JKDLv_loy-CZcT8fJquZIumCmjUC9iiKCreEJC4WMFfwFpMOKnBZa-hrr/pub?output=csv')

latest_data <- logged_data %>%
  rename(time = 2) %>%
  rename(total_posts = 3) %>%
  rename(elon_posts = 4) %>%
  rename(trump_posts = 5) %>%
  rename(upvotes = 6) %>%
  rename(type_of_post = 7)

# bar chart of total posts colored by time
latest_data %>%
  ggplot() +
    geom_bar(aes(x = total_posts, fill=time)) + 
    labs(title = "Total posts colored by the time they appeared on /r/all") + 
    xlab("Total posts") + 
    ylab("Number of posts")

# bar chart of elon posts colored by time
latest_data %>%
  ggplot() +
    geom_bar(aes(x = elon_posts, fill=time)) + 
    labs(title = "Posts about Elon Musk colored by the time they appeared on /r/all") + 
    xlab("Posts about Elon Musk") + 
    ylab("Number of posts")

# bar chart of trump posts colored by time
latest_data %>%
  ggplot() +
    geom_bar(aes(x = trump_posts, fill=time)) + 
    labs(title = "Posts about Donald Trump colored by the time they appeared on /r/all") + 
    xlab("Posts about Donald Trump") + 
    ylab("Number of posts")

# bar chart of posts that were tangentially related to American politics colored by time
latest_data %>%
  ggplot() +
    geom_bar(aes(x = (total_posts - elon_posts - trump_posts), fill=time)) + 
    labs(title = "Posts tangentially related to American politics by the time they appeared on /r/all") + 
    xlab("Tangentially related posts") + 
    ylab("Number of posts")


# bar chart of the upvotes the most upvoted post got, colored by the type of post it was
latest_data %>%
  ggplot() + 
    geom_bar(aes(x = upvotes, fill=type_of_post)) + 
    labs(title="Upvotes the most popular political post got, colored by the type of post it was") + 
    xlab("Number of upvotes") + 
    ylab("Number of posts") + 
    theme(axis.text.x = element_text(angle=45, hjust=1))

# mean and var of total posts
summary(latest_data[3])[4]
var(latest_data[3])

# mean and var of elon posts
summary(latest_data[4])[4]
var(latest_data[4])

# mean and var of trump posts
summary(latest_data[5])[4]
var(latest_data[5])



#latest_data %>%
#  ggplot() +
#    geom_bar(aes(x = (total_posts - elon_posts - trump_posts), fill=time)) + 
#    labs(title = "Posts tangentially related to American politics by the time they appeared on /r/all") + 
#    xlab("Tangentially related posts") + 
#    ylab("Number of posts")
#
#latest_data %>%
#  ggplot() + 
#    geom_bar(aes(x = upvotes, fill=type_of_post)) + 
#    labs(title="Upvotes the most popular political post got, colored by the type of post it was") + 
#    xlab("Number of upvotes") + 
#    ylab("Number of posts") + 
#    theme(axis.text.x = element_text(angle=45, hjust=1))
#
#mean(latest_data[["trump_posts"]])
#mean(latest_data[["total_posts"]])