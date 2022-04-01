library(tidyverse)
library(lubridate)
library(emojifont)

showtext::showtext_opts(dpi = 320)

df <- data.frame(date = seq.Date(from = as.Date('2021-04-01'), to = as.Date('2021-04-30'), by = 'day')) %>%
  mutate(plot = day(date) %in% c(1,2,4,5,6,7,8,10,12,13,16),
         dateCol = ymd(date),
         weekday = wday(dateCol, label = T, week_start = 1),
         date = day(dateCol),
         monthweek = c(rep(1, 4), rep(seq(2, 4), each = 7), rep(5, 5)))

ggplot(df, aes(x = weekday, y = -monthweek)) +
  geom_tile(fill='transparent', col = '#ffffff', size = 1) +
  geom_text(data = df %>% filter(plot == T), col = '#FFE8C2', label = fontawesome('fa-star'), family = 'fontawesome-webfont', size = 17, alpha = 1) +
  geom_text(data = df %>% filter(plot == T), col = '#FFC15E', label = fontawesome('fa-star'), family = 'fontawesome-webfont', size = 15)  + 
  geom_text(label = as.character(df$date), aes(col = plot), show.legend = F, size = 6) +
  scale_colour_manual(values = c('#ffffff', '#000000')) +
  labs(title = 'My contributions to #30DayChartChallenge in 2021', 
       subtitle = paste0(fontawesome('fa-star'), paste0(rep(' ', 55), collapse = ' '), fontawesome('fa-star')),
       caption = '\n@sarahe145 | #30DayChartChallenge') +
  theme_void(base_size = 16) +
  theme(
    axis.text.x = element_text(colour = '#ffffff'),
    plot.title = element_text(colour = '#ffffff', hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, family = 'fontawesome-webfont',  margin = margin(t = -17, b = 10), colour = '#FFC15E'),
    plot.caption = element_text(colour = 'grey95', size = 12),
    panel.background = element_rect(fill = 'grey10', color='transparent'),
    plot.background = element_rect(fill = 'grey10', color='transparent'),
    plot.margin = margin(10,10,10,10)) +
  coord_cartesian(clip = 'off')

ggsave('01-part-to-whole.png', width = 6.9, height = 5.5, dpi = 320)


df1 <- data.frame(date = seq.Date(from = as.Date('2022-04-01'), to = as.Date('2022-04-30'), by = 'day')) %>%
  mutate(plot = day(date) %in% c(1),
         dateCol = ymd(date),
         weekday = wday(dateCol, label = T, week_start = 1),
         date = day(dateCol),
         monthweek = c(rep(1, 3), rep(seq(2, 4), each = 7), rep(5, 6)))

ggplot(df1, aes(x = weekday, y = -monthweek)) +
  geom_tile(fill='transparent', col = '#ffffff', size = 1) +
  geom_text(data = df1 %>% filter(plot == T), col = '#FFE8C2', label = fontawesome('fa-star'), family = 'fontawesome-webfont', size = 17, alpha = 1) +
  geom_text(data = df1 %>% filter(plot == T), col = '#FFC15E', label = fontawesome('fa-star'), family = 'fontawesome-webfont', size = 15)  + 
  geom_text(label = as.character(df1$date), aes(col = plot), show.legend = F, size = 6) +
  scale_colour_manual(values = c('#ffffff', '#000000')) +
  labs(title = 'My contributions to #30DayChartChallenge in 2022', 
       subtitle = paste0(fontawesome('fa-star'), paste0(rep(' ', 55), collapse = ' '), fontawesome('fa-star')),
       caption = '\n@sarahe145 | #30DayChartChallenge') +
  theme_void(base_size = 16) +
  theme(
    axis.text.x = element_text(colour = '#ffffff'),
    plot.title = element_text(colour = '#ffffff', hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, family = 'fontawesome-webfont',  margin = margin(t = -17, b = 10), colour = '#FFC15E'),
    plot.caption = element_text(colour = 'grey95', size = 12),
    panel.background = element_rect(fill = 'grey10', color='transparent'),
    plot.background = element_rect(fill = 'grey10', color='transparent'),
    plot.margin = margin(10,10,10,10)) +
  coord_cartesian(clip = 'off')

ggsave('01-part-to-whole_1.png', width = 6.9, height = 5.5, dpi = 320)
