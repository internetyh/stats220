library(tidyverse)
library(lubridate)

logged_data <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSUiUAyo4i76UMx-gIXi7-JKDLv_loy-CZcT8fJquZIumCmjUC9iiKCreEJC4WMFfwFpMOKnBZa-hrr/pub?output=csv')

my_colours = c("#e41a1c", "#377eb8", "#4daf4a", "#c994c7", "#984ea3", "#ff7f00", "#f7f7f7")
my_theme <-  theme(
  plot.background = element_rect(fill = "black", color = NA),
  panel.background = element_rect(fill = "black", color = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  
  axis.text = element_text(color = "white"),
  axis.title = element_text(color = "white"),
  plot.title = element_text(color = "white", face = "bold", size = 14),
  plot.subtitle = element_text(color = "white", size = 12),
  plot.caption = element_text(color = "white", size = 10),
  
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect(fill = "black", color = NA),
  legend.text = element_text(color = "white"),
  legend.title = element_text(color = "white"),
  
  strip.background = element_rect(fill = "black", color = NA),  # for facets
  strip.text = element_text(color = "white"),
  
  text = element_text(color = "white"),
  plot.margin = margin(1, 1, 1, 1, "cm")
)

# renaming all the columns in the spreadsheet for ease of use
logged_data <- logged_data %>%
  rename(`upvotes` = `Exact number of upvotes`) %>%
  rename(`upvote_group` = `How many upvotes does the most upvoted post have`) %>%
  rename(`total_posts` = `How many posts relate to American politics`) %>%
  rename(`elon_posts` = `How many posts were about Elon Musk`) %>%
  rename(`trump_posts` = `How many posts were about the Trump Administration`) %>%
  rename(`post_class` = `Is the most upvoted post about domestic U.S. affairs or related to American politics?`)

# most common type of post is trump if trump_posts > elon_posts and we also have
# trump_posts > total_posts - elon_posts - trump_posts (total_posts - elon_posts - trump_posts = general_posts)
# -> 2*trump_posts + elon_posts > total_posts
# same principle applies to elon_posts
# when we have trump_posts = elon_posts = general_posts, just say most common type is general_post
data_with_types <- logged_data %>%
  mutate(type_of_post = case_when(
    (elon_posts > trump_posts) & (2*elon_posts + trump_posts > total_posts) ~ 'Elon',
    (trump_posts > elon_posts) & (2*trump_posts + elon_posts > total_posts) ~ 'Trump',
    TRUE ~ 'General'
  ))

# first data frame
# time stamp/number of posts (colored by type of post)
summarised_data <- data_with_types %>% 
  mutate(
    date_post = mdy_hms(Timestamp, tz = "UTC") %>% 
      with_tz("Pacific/Auckland"),
    time_only = format(date_post, "%H:%M:%S")
  ) %>%
  group_by(total_posts, type_of_post) %>%
  summarise(time_only)

# total number of posts vs time of day
# colored by type of post
plot1 <- summarised_data %>%
  ggplot() + 
  geom_point(aes(x = time_only, y = total_posts, color  = type_of_post), size=5) +  
  labs(colour="Most common post", x = "Time of day (00:00 - 24:00)", y = "Total number of posts", title="When are people on reddit engaging with American politics?") + 
  my_theme + 
  theme(axis.text.x = element_blank()) + 
  scale_color_manual(values = c(my_colours[1], my_colours[2], my_colours[5]))

# creating boxplot with the data_with_types frame grouped by class of post
plot2 <- data_with_types %>%
  ggplot() +
  geom_boxplot(aes(x = post_class, y = as.numeric(upvotes), fill=post_class)) + 
  labs(title = "How popular are political posts?", x = "Post class", y = "Number of Upvotes", fill="Post Class") + 
  my_theme + 
  scale_fill_manual(values = c(my_colours[1], my_colours[2]))

# total posts ~ upvotes
# grouped by class of post
plot3 <- data_with_types %>%
  ggplot(aes(x = upvotes, y = total_posts, color = post_class)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "white") + 
  facet_wrap(vars(post_class), scale="free") +
  labs(title = "The relationship between total posts and upvotes", x="Highest upvotes a post", y = "Total Posts") + 
  theme(legend.position="none") + 
  my_theme + 
  scale_color_manual(values = c(my_colours[1], my_colours[2]))

ggsave("plot1.png", plot = plot1, width=11, height=7)
ggsave("plot2.png", plot = plot2, width=11, height=7)
ggsave("plot3.png", plot = plot3, width=11, height=7)

