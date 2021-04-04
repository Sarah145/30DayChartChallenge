library(tidyverse)
library(ggfx)
library(gganimate)

sysfonts::font_add_google(name = "Montserrat", regular.wt = 300)
showtext::showtext_auto()

# read in data from IMDb:https://datasets.imdbws.com/title.basics.tsv.gz
df <- read.csv('title.basics.tsv', sep = '\t')
df <- df %>% filter(titleType == 'movie')

# get number of fantasy/sci-fi movies released each decade
fantasy <- df %>% filter(str_detect(genres, '(Sci-Fi)|(Fantasy)')) %>% 
  filter(startYear != '\\N') %>%
  mutate(decade = 10 * (as.numeric(startYear) %/% 10)) %>% 
  group_by(decade) %>% 
  tally() 

# make labels for plot
fantasy <- fantasy %>% mutate(decade_label = as.factor(ifelse(decade == 2020, 2020, paste0(decade, '-', decade + 10))),
                              decade_dummy = decade_label,
                              n_label = prettyNum(n, big.mark = ','))

p <- ggplot(fantasy, aes(x = decade_label, y = n, size = n)) +
  with_blur(geom_point(aes(size = n +1000), pch = 8, col = '#FAF58A', show.legend = F),
            sigma = 1) +
  with_blur(geom_point(aes(size = n + 1000), col = '#FFFFFF', show.legend = F),
            sigma = 4) +
  geom_point(data = fantasy %>% select(-decade_label), aes(x = decade_dummy), 
             col = '#FAF58A', show.legend = F) +
  scale_x_discrete(breaks = levels(fantasy$decade_label), limits = levels(fantasy$decade_label), expand = c(0,0), labels = unique(fantasy$decade)) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_area(max_size = 20) +
  labs(x = NULL, y = NULL, title = 'Number of Sci-Fi and Fantasy movies released each decade since 1900\n', 
       subtitle = 'Decade: {closest_state}, Number of releases: {fantasy$n_label[fantasy$decade_label == closest_state]}\n',
       caption = '\nData: IMDb | @sarahe145 | #30DayChartChallenge') +
  theme_minimal(base_size = 18) +
  theme(panel.background = element_rect(color = '#5A5475', fill = '#5A5475'),
        plot.background = element_rect(color = '#5A5475', fill = '#5A5475'),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(linetype = 4, size = 0.1, color = 'gray95'),
        axis.text = element_text(color = 'gray95', family = 'Montserrat'),
        axis.text.y = element_text(size = 11),
        plot.margin = margin(20,40,20,20),
        plot.title = element_text(size = 16.75, color = 'gray95', family = 'Montserrat'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(color = 'gray95', family = 'Montserrat', size = 12),
        plot.caption = element_text(color = 'gray85', family = 'Montserrat', size = 8, hjust = 0.5)) +
  coord_cartesian(clip = 'off') + 
  transition_states(decade_label, wrap = T, transition_length = 1, state_length = 3) + 
  enter_fade() + 
  exit_shrink()

animate(p, height = 500, width = 850, fps = 7)

anim_save('04-magical.gif')
