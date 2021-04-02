library(ggtext)
library(tidyverse)

sysfonts::font_add_google(name = "Roboto Mono")
sysfonts::font_add_google(name = "Roboto", regular.wt = 100)
showtext::showtext_auto()
sym <- "<img src='syringe.png' width='35'/>"

# create dataframe - data from @COVID19DataIE on Twitter
df <- tribble(
  ~dose, ~number,
  "First Dose", 577641,
  "Second Dose", 224841,
)

plot_df <- df %>% 
  mutate(n_sym = ceiling(number/20000)) %>% 
  group_by(dose) %>%
  expand(dose, 1:n_sym) %>%
  rename(n_sym = `1:n_sym`)

ggplot(plot_df, aes(y= n_sym, x = dose, label = sym)) +
  geom_richtext(fill = NA, color = 'black', label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_text(data = data.frame(x = 0.6, y = 33, label = str_wrap('*each symbol = 20,000 doses', width = 20)), 
            aes(x,y,label = label),
            family = 'Roboto', size = 6) +
  geom_text(data = data.frame(x = df$dose, y = df$number, label = prettyNum(df$number, big.mark = ',')),
           aes(x = x, y = y/20000 + 2.2, label = label),
           family = "Roboto Mono",
           size = 6,
           color = '#de3028') +
  geom_text(data = data.frame(x = 2, y = (700000/20000)+1, label = 'Target'), aes(x = x, y = y, label = label), family = 'Roboto Mono', color = '#de3028', size = 7.7) +
  geom_segment(aes(x = 1.75, xend = 2.25, y = 700000/20000, yend = 700000/20000), lty = 2, col = '#de3028') +
  scale_x_discrete(limits = c('First Dose', 'Second Dose')) +
  scale_y_continuous(expand = c(0,0), limits = c(0,37.5)) +
  labs(title = 'Slow Pokes.', 
       subtitle = str_wrap('In Q1 of 2021, Ireland administered a total of 802,502 doses of COVID19 vaccines. This means that 577,641 people received at least one dose and 224,861 people were fully vaccinated. On the 13th of January, the Minister for Health mentioned that the target was to have 700,000 people fully vaccinated by the end of March.', width = 90),
       x = NULL, y = NULL,
       caption = 'Data: @COVID19DataIE | @sarahe145 | #30DayChartChallenge') +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = 'black'),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Roboto Mono", size = 19, color = 'black', hjust = 0.5),
        plot.title = element_text(family = "Roboto Mono", color = '#de3028', size = 23),
        plot.title.position = 'plot',
        plot.subtitle = element_text(family = "Roboto", hjust = 0, size = 18),
        plot.caption = element_text(family = "Roboto", size = 14, color = 'gray20', hjust = 0.5),
        plot.margin = margin(t = 20, b = 20, l = 20, r = 20)) +
  coord_cartesian(clip = 'off')
ggsave('02-pictogram.png', width = 13, height = 14.2, dpi = 75)

