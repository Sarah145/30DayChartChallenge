library(tidyverse)
library(csodata)
library(ggtext)
library(patchwork)
library(broom)
library(paletteer)
library(rgdal)
library(animation)

# load census data
census_df <- cso_get_data('EA039') %>% filter(Statistic == 'Irish speakers as a percentage of total')
census_df <- census_df %>% pivot_longer(3:ncol(census_df), names_to = 'Year', values_to = 'Percent')

# ggplot(d %>% filter(Province == 'State'), aes(x = Percent, y = as.numeric(Year))) + 
#   geom_col(aes(x = 100), fill = '#ffffff', orientation = 'y', col = '#333333', size = 0.2, width = 0.5) + 
#   geom_col(orientation = 'y', fill = '#44ab5c', size = 0.2, width = 3) + 
#   geom_point(col = '#44ab5c',  size = 9) + 
#   geom_text(aes(x = Percent, label = Percent), col = '#ffffff', fontface = 'bold', size = 5) +
#   scale_y_reverse(breaks = as.numeric(unique(d$Year)), expand = c(0,0)) +
#   scale_x_continuous(expand = c(0,0), labels = function(x) paste0(x, '%'), limits = c(0,105)) +
#   labs(title = '<span style=color:#44ab5c>**Irish speakers**</span> as a percentage of the Irish population (1861-2016)<br>',
#        caption = '<br>Data: data.cso.ie | @sarahe145 | #30DayChartChallenge') +
#   theme_minimal(base_size = 20) +
#   theme(axis.text = element_text(colour = '#000000'),
#         axis.title = element_blank(),
#         axis.ticks = element_line(colour = '#333333'),
#         strip.text = element_blank(),
#         strip.background = element_blank(),
#         plot.title = element_markdown(size = 20),
#         plot.title.position = 'plot',
#         plot.caption = element_markdown(size = 13, hjust = 1),
#         panel.grid = element_blank(),
#         plot.margin = margin(10,5,10,10)
#         ) +
#   coord_cartesian(clip = 'off')
# 
# ggsave('~/Documents/30DayChartChallenge/2022/day3/historical.png', width = 9.75, height = 10, bg = 'white')
# 
# ggplot(d %>% filter(Province != 'State'), aes(x = as.numeric(Year), y = Percent, fill = Province)) +
#   geom_col()

# read in shapefiles for province boundaries
# downloaded from here: https://www.cso.ie/en/census/census2011boundaryfiles/
boundaries <- readOGR( 
  dsn= "Census2011_Province_generalised20m" , 
  layer="Census2011_Province_generalised20m",
  verbose=FALSE
)

# tidy boundary data and join with census data
spdf_fortified <- tidy(boundaries, region = "PROVNAME")
spdf_census <- left_join(spdf_fortified, census_df, by = c('id' = 'Province'))

# df for annotating provinces
text_df <- census_df %>% filter(Province != 'State') %>%
  mutate(id = Province,
                        x = case_when(id == 'Connacht' ~ 150000,
                                      id == 'Leinster' ~ 280000,
                                      id == 'Munster' ~ 130000,
                                      id == 'Ulster (part of)' ~ 208743),
                        y = case_when(id == 'Connacht' ~ 260335,
                                      id == 'Leinster' ~ 216643,
                                      id == 'Munster' ~ 100000,
                                      id == 'Ulster (part of)' ~ 415144))
# make list of plots
plot_list <- list()
for(i in unique(census_df$Year)){
  #p1 <- 
    ggplot() +
    geom_polygon(data = spdf_census %>% filter(Year == i), aes( x = long, y = lat, group = group, fill = Percent), color="white") +
    geom_text(data = text_df %>% filter(Year == i), aes(x=x, y=y, label = paste0(id,'\n',paste0(Percent, '%'))), size = 8) +
    scale_fill_gradientn(colours = paletteer_c('grDevices::Greens', 10, direction = -1), breaks = seq(0,100, 20), limits = c(0,100),  name = NULL) +
    labs(title = 'Irish speakers as a percentage of the population',
         subtitle = paste0('Year: ', i, '<br>'),) +
    theme_void(base_size = 20) +
    theme(legend.position = 'top',
          legend.key.width = unit(6, 'lines'),
          plot.title = element_text(size = 22, face = 'bold', colour = '#004616FF'),
          plot.subtitle = element_markdown(size = 20),
          plot.title.position = 'plot',
          plot.margin = margin(0,0,0,0))
  
  p2 <- ggplot(census_df %>% filter(as.numeric(Year) <= as.numeric(i), Province == 'State'), aes(x = as.numeric(Year), y = Percent, fill = Percent)) +
    geom_hline(yintercept = seq(10,50,20), lty =2, size = 0.2, col = 'gray40') +
    geom_text(data = data.frame(x = 1938.5, y = 36, label = 'Irish State'), aes(x=x, y=y, label=label, fill = NULL), size = 14, col = '#004616FF') +
    geom_path(aes(fill = NULL)) +
    geom_point(pch=21, size = 6) +
    scale_y_continuous(limits = c(10,50), breaks = seq(10,50,20), labels = function(x) paste0(x, '%')) +
    scale_x_continuous(breaks = census_df %>% filter(as.numeric(Year) <= as.numeric(i)) %>% pull(Year) %>% unique() %>% as.numeric(), limits = c(1861, 2016)) + 
    scale_fill_stepsn(colours = paletteer_c('grDevices::Greens', 10, direction = -1), breaks = seq(0,100,20), limits = c(-0.1,100.1), labels = function(x) paste0(x, '%'), name = NULL) +
    labs(caption = '<br>Data: data.cso.ie | @sarahe145 | #30DayChartChallenge') +
    theme_minimal(base_size = 20) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          plot.caption = element_markdown(size = 13, hjust = 1),
          axis.title = element_blank(),
          axis.text = element_text(colour = 'black'),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.ticks.x = element_line(colour = 'gray60'),
          plot.margin = margin(20,20,20,20))
  combined_plot <- p1/p2 + plot_layout(heights = c(1.8, 0.2))
  plot_list[[i]] <- eval(substitute(combined_plot))
}

# combine plots into a gif
saveGIF({
  for (i in plot_list)plot(i)},
  movie.name = 'historical.gif',
  ani.width = 700, ani.height = 980)
