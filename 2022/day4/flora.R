library(tidyverse)
library(ggforce)

sysfonts::font_add_google(name = "Montserrat", regular.wt = 300)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 400)

# data downloaded from OWID https://ourworldindata.org/grapher/sunflower-seed-production?tab=chart&
sun <- read.csv('sunflower-seed-production.csv')

# filter to worldwide data and calculate total in each decade
world_sun  <- sun %>% filter(Entity == 'World') %>%
  mutate(decade = 10 * (as.numeric(Year) %/% 10)) %>%
  group_by(decade) %>% 
  mutate(decade_total = sum(Crops...Sunflower.seed...267...Production...5510...tonnes)) 
  
# make plot
ggplot(world_sun %>% select(decade, decade_total) %>% group_by(decade) %>% mutate(index = seq(1:length(decade))) %>%
         mutate(index_scaled = (((index-min(index))/(max(index) - min(index))) * 10) + 1), 
       aes(x0=index_scaled, y0=decade_total/2, a = 0.5, b = decade_total/2, angle = 0, m1 = 2.6)) +
  geom_ellipse(col = '#FDCC00', fill = '#FDCC00', size = 0.2) + 
  geom_point(aes(x=1, y=0, fill = NULL, size = decade_total), col = '#821E12') +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_x_continuous(limits = c(0.25, 11.5)) +
  scale_size(range = c(2,9)) +
  labs(title = 'WORLDWIDE SUNFLOWER SEED PRODUCTION',
       subtitle = str_wrap('Each flower represents the amount of sunflower seed produced (in millions of tonnes) per decade. The number of petals on each flower represents how many years for which there was data available in each decade.\n', width = 78),
       caption = 'Data: OWID | @sarahe145 | #30DayChartChallenge') +
  coord_polar() +
  facet_wrap(~decade, nrow = 2, strip.position = 'bottom') +
  theme_minimal() +
  theme(panel.grid = element_line(colour = alpha('gray70', 0.5)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(colour = '#528635', face = 'bold', size = 14, family = 'Montserrat'),
        plot.title = element_text(colour = '#528635', face = 'bold', size = 21, family = 'Montserrat'),
        plot.subtitle = element_text(family = 'Montserrat', size = 13, margin = margin(b = 10)),
        plot.caption = element_text(family = 'Montserrat', hjust = 0.5, colour = '#000000', size = 13),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none',
        plot.margin = margin(10,10,10,10))

ggsave('flora.png', width = 8, height = 7, bg = '#ffffff', dpi = 400)
# annotate axis in Inkscape
