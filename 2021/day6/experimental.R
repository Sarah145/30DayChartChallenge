library(tidyverse)
library(lubridate)
library(patchwork)
options(scipen=999)

sysfonts::font_add_google('Walter Turncoat')
sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 500)

data <- readr::read_delim('http://www.nxn.se/single-cell-studies/data.tsv', delim = '\t')

data <- data %>% filter(!is.na(`Reported cells total`)) %>%
  mutate(year = year(paste0(str_sub(Date, 1, 4), '-', str_sub(Date, 5, 6), '-', str_sub(Date, 7, 8))),
         date = date(paste0(str_sub(Date, 1, 4), '-', str_sub(Date, 5, 6), '-', str_sub(Date, 7, 8))),
         tech = ifelse(Technique %in% c('Chromium', 'Smart-seq2'), Technique, 'Other')) 
data$tech <- factor(data$tech, levels = c('Chromium', 'Smart-seq2', 'Other'))

mean_df <- data %>% group_by(year) %>% summarise(year_mean = mean(`Reported cells total`))
mean_df$date <- date(paste0(mean_df$year, '-01-01'))

highlight_df <- data %>% filter(DOI %in% c('10.1126/science.aba7721', '10.1073/pnas.97.11.6144'))

ggplot(data, aes(x = date, y = `Reported cells total`)) +
  geom_point(col = '#FFB7C3', size = 1.5, alpha = 0.9) +
  geom_point(data = highlight_df, size = 5.5, col = '#C42847') +
  geom_point(data = highlight_df, size = 2.5, col = '#FFB7C3') +
  geom_path(data = mean_df, aes(y = year_mean, col = NULL), col = 'gray90') +
  geom_point(data = mean_df, aes(y = year_mean, col = NULL), col = '#96CBFE', size = 4) +
  geom_label(data = data.frame(x = as.Date("2002-12-15"), y = 1300, 
                              label = "First study - Cauli et al, 2002\nNumber of cells - 85"), mapping = aes(x = x, y = y, label = label), 
            size = 5, colour = "white", alpha = 1, inherit.aes = FALSE, family = 'Montserrat', fill = alpha(c("gray30"),0.01), label.size = NA) +
  geom_curve(aes(x = as.Date('2002-10-15'), xend = as.Date('2002-07-26'), y = 550, yend = 120), curvature = -0.25, size = 0.25, colour = "gray90", arrow = arrow(length = unit(0.01, "npc"))) +
  geom_label(data = data.frame(x = as.Date("2016-01-15"), y = 1400000, 
                               label = "Current record - Cao et al, 2020\nNumber of cells - 4,062,980"), mapping = aes(x = x, y = y, label = label), 
             size = 5, colour = "white", alpha = 1, inherit.aes = FALSE, family = 'Montserrat', fill = alpha(c("gray30"), 0.01), label.size = NA) +
  geom_curve(aes(x = as.Date('2016-10-15'), xend = as.Date('2020-07-26'), y = 3000000, yend = 4000000), curvature = -0.15, size = 0.25, colour = "gray90", arrow = arrow(length = unit(0.01, "npc"))) +
  geom_label(data = data.frame(x = as.Date("2010-10-01"), y = 4500, 
                               label = "Yearly Average"), mapping = aes(x = x, y = y, label = label), 
             size = 5, colour = "white", alpha = 1, inherit.aes = FALSE, family = 'Montserrat', fill = alpha(c("gray30"), 0.01), label.size = NA) +
  geom_curve(aes(x = as.Date('2010-10-01'), xend = as.Date('2011-09-26'), y = 2800, yend = 400), curvature = 0.25, size = 0.25, colour = "gray90", arrow = arrow(length = unit(0.01, "npc"))) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  scale_y_log10(labels = prettyNum(round(c(1,10,100,1000,10000,100000,1000000)), big.mark = ','), breaks = c(1,10,100,1000,10000,100000,1000000)) +
  annotation_logticks(color = 'gray50') +
  labs(x = '\nDate Published', y = '# of cells', color = 'Technology', title = 'Number of cells in single-cell sequencing studies\n',
       caption = '\nData: nxn.se/single-cell-studies | @sarahe145 | #30DayChartChallenge') +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_minimal(base_size = 20, base_family = 'Walter Turncoat') +
  theme(axis.text.x = element_text(color = 'white', size = 14),
        axis.text.y = element_text(color = 'white', size = 12),
        axis.ticks.x = element_line(color = 'gray50'),
        axis.title = element_text(color = 'white'),
        plot.title = element_text(color = 'white', size = 26),
        plot.margin = margin(10, 30, 10, 10),
        plot.caption = element_text(family = 'Montserrat', color = 'gray90', size = 12, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'gray35', linetype = 2, size = 0.5),
        panel.background = element_rect(color = '#333333', fill = '#333333'),
        plot.background = element_rect(color = '#333333', fill = '#333333')) +
  coord_cartesian(clip = 'off')

ggsave("06-experimental.png", dpi = 500, width = 14, height = 8)

