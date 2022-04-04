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
  p1 <- ggplot() +
    geom_polygon(data = spdf_census %>% filter(Year == i), aes( x = long, y = lat, group = group, fill = Percent), color="white") +
    geom_text(data = text_df %>% filter(Year == i), aes(x=x, y=y, label = paste0(id,'\n',paste0(Percent, '%'))), size = 8) +
    scale_fill_stepsn(colours = paletteer_c('grDevices::Greens', 10, direction = -1), breaks = seq(0,100, 20), limits = c(0,100), labels = function(x) paste0(x, '%'), name = NULL) +
    guides(fill = guide_bins(show.limits = T)) +
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
    scale_fill_stepsn(colours = paletteer_c('grDevices::Greens', 10, direction = -1), breaks = seq(0,100,20), limits = c(0,100), labels = function(x) paste0(x, '%'), name = NULL) +
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
  movie.name = '03-historical.gif',
  ani.width = 700, ani.height = 980)
