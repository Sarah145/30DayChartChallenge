library(tidyverse)
library(rtweet)
library(ggtext)

sysfonts::font_add_google('Playfair Display')
sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# get tweets
tweets <- get_timeline(user = 'dog_rates', n=4000)
tweets <- tweets %>% filter(is_retweet == F)

# get ratings
ratings <- tweets %>% filter(str_detect(text, '[0-9]+/10')) %>%
  mutate(rating = str_extract(text, '[0-9]+/10'),
         num_rating = as.numeric(str_extract(rating, '[^/]+')))

# exclude this one because it wasn't actually a rating
ratings <- ratings %>% filter(status_id != '1280674673684955136')


# annotations
label_df <- data.frame(x = c(0, 10, 15),
                       y = c(45, 200, 50),
                       label = c('Lowest rating - 0/10 for this robodog',
                                 'The most common rating is 13/10',
                                 'Rusty was one of 15 dogs to achieve 15/10'))
img_df <- data.frame(x = c(0, 15), y = c(100, 110), 
                     label = c('<img src="robodog.jpeg" width="150"/>',
                               '<img src="rusty.jpeg" width="130"/>'))
# img sources: https://pbs.twimg.com/media/ERP8JE7XYAwHygA?format=jpg&name=large, https://pbs.twimg.com/media/Eu7jWNCXAAMR-R6?format=jpg&name=medium

# plot
ggplot(ratings, aes(x = num_rating)) +
  geom_bar(fill = '#4CE0B3', col = '#AD343E', size = 1, alpha = 0.5) +
  geom_label(data = label_df, aes(x = x, y = y, label = str_wrap(label, width = 15)),
            col = '#AD343E', family = 'Montserrat', size = 7, label.size = NA, fill = alpha('white', 0.6)) +
  geom_curve(aes(x = 10, xend = 12.5, y = 215, yend = 250), curvature = -0.25, size = 0.5, colour = "#AD343E", arrow = arrow(length = unit(0.025, "npc"))) +
  geom_richtext(data = img_df, aes(x = x, y = y, label = label), fill = NA, label.color = NA) +
  scale_x_continuous(breaks = seq(0, 15), limits = c(-1, 16)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 300), breaks = seq(0,300,50)) +
  labs(title = 'Distribution of dog ratings awarded by @dog_rates on Twitter\n',
       x = '\nRating (out of 10)', y = 'Frequency\n', caption = '\nData: scraped from @dog_rates using {rtweet} | @sarahe145 | #30DayChartChallenge') +
  theme_minimal(base_size = 23) +
  theme(plot.title = element_text(family = 'Playfair Display', hjust = 0.5),
        plot.title.position = 'plot',
        axis.title = element_text(family = 'Playfair Display', size = 22),
        axis.text = element_text(family = 'Montserrat', color = '#AD343E', face = 'bold'),
        plot.caption = element_text(family = 'Playfair Display', size =13),
        plot.margin = margin(20,20,20,20)) +
  coord_cartesian(clip = 'off')

ggsave('08-animal.png', width = 16, height = 10)
