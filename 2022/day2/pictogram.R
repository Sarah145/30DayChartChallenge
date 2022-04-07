library(tidyverse)
library(ggtext)
library(ggimage)
library(ggfx)

sysfonts::font_add_google(name = "Montserrat", regular.wt = 300)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 400)

# data from here https://hea.ie/statistics/data-for-download-and-visualisations/access-our-data/access-our-data-students/
df <- read.csv('Programme Type Breakdown_Full Data_data.csv')

plot_df <- df %>% filter(Gender != 'Non-binary') %>% # only 2 non-binary people so filtering them out
  mutate(Gender = factor(Gender, levels = c('Female', 'Male'))) %>%
  group_by(Gender, Academic.Year, ISCED.Broad.Field.of.Study, .drop = F) %>% tally() %>% 
  ungroup() %>% group_by(Academic.Year, ISCED.Broad.Field.of.Study) %>% 
  mutate(per = n/sum(n)) # calculate percent of male/female in each field, each year

# add rows for total
plot_df <- full_join(plot_df, df %>% filter(Gender != 'Non-binary') %>% 
                       group_by(Gender, Academic.Year, .drop = F) %>% tally() %>% 
                       ungroup() %>% group_by(Academic.Year) %>%
                       mutate(per = n/sum(n),
                          ISCED.Broad.Field.of.Study = 'Total'))

# set up facet labels
labs <- str_wrap(unique(plot_df$ISCED.Broad.Field.of.Study), width = 20)
labs <- str_replace_all(labs, '\n', '<br>')
names(labs) <- unique(plot_df$ISCED.Broad.Field.of.Study)
labs['Total'] <- '**Total**'

# make df for icons
icon_df <- data.frame(y = rep(unique(df1$Academic.Year), each = 10), 
                      x = rep(seq(0.05,0.95, 0.1), 3))

icon <- "person.png"

ggplot(plot_df, aes(y = Academic.Year, fill = Gender, x = per)) +
  as_reference(geom_image(data = icon_df, aes(image = icon, x = x, y = y, col = NULL, fill = NULL), 
                          size = 0.12), id = 'icon') +
  with_blend(geom_col(), bg_layer = 'icon', blend_type = 'in') +
  scale_fill_manual(values = c('#361D9D', '#E04483')) +
  scale_x_continuous(labels = scales::percent, expand = c(0,0)) +
  scale_y_discrete(limits = c('2020/2021', '2019/2020', '2018/2019'), 
                   labels = c('2020/21', '2019/20', '2018/19'), expand = c(0,0)) +
  labs(title = 'Percentage of <span style=color:#361D9D>male</span> and <span style=color:#E04483>female</span> PhD students in Ireland<br>',
       caption = 'Data: Higher Education Authority | @sarahe145 | #30DayChartChallenge') +
  facet_wrap(~ISCED.Broad.Field.of.Study, labeller = labeller(ISCED.Broad.Field.of.Study = labs)) +
  theme_classic(base_family = 'Montserrat') +
  theme(axis.text = element_text(colour = 'black'),
        legend.position = 'none',
        plot.title = element_markdown(face = 'bold', size = 20, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(margin = margin(t = 20), hjust = 0.5, size = 14),
        strip.background = element_blank(),
        strip.text = element_markdown(size = 13, hjust = 0.5),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12, margin = margin(r = 5)),
        panel.spacing.x = unit(2, 'lines'),
        panel.spacing.y = unit(1.5, 'lines'),
        plot.margin = margin(20,20,20,20))+
  coord_cartesian(clip = 'off')

ggsave('02-pictogram.png', width = 12, height = 10, dpi = 400)
