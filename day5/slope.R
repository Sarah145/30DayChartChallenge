library(tidyverse)
library(AppleHealthAnalysis)
library(lubridate)
library(ggfx)
library(ggtext)

health_data <- ah_import_xml("export.xml")
steps <- health_data %>% 
  filter(type == 'StepCount') %>% 
  group_by(date) %>% 
  summarise(steps = sum(value)) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_steps = mean(steps))

cols <- PNWColors::pnw_palette('Bay', 3)
title <- paste0('My average number of daily steps in <span style=color:', cols[1], '>', steps$year[1], 
                '</span>, <span style=color:', cols[2], '>', steps$year[2],'</span> and ',
                '<span style=color:', cols[3], '>', steps$year[3], '</span>')
  
ggplot(steps, aes(x = year, y = mean_steps)) +
  geom_hline(yintercept = seq(2500, 5500, 500), color = 'gray80') +
  with_variable_blur(geom_path(), x_sigma = 0.5, y_sigma = 0.5, x_scale = 5, y_scale = 5) +
  geom_path() +
  geom_vline(xintercept = unique(steps$year), lty = 2) +
  geom_point(col = 'gray20', size = 14, pch = 16, show.legend = F) +
  geom_point(col = 'white', size = 11, pch = 16, show.legend = F) +
  geom_point(aes(col = as.factor(year)), size = 9, show.legend = F) +
  labs(x = NULL, y = NULL, title = title) +
  scale_x_continuous(breaks = seq(2019, 2021)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(2500, 6000, 500), limits = c(2500, 5500)) +
  scale_color_manual(values = PNWColors::pnw_palette('Bay', 3)) +
  theme_minimal(base_size = 22) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = 'black', size = 21),
        plot.title = element_textbox(hjust = 0.5),
        plot.title.position = 'plot',
        plot.margin = margin(20,20,20,20))

ggsave('05-slope.png', width = 12, height = 10, dpi = 100)
