library(tidyverse)
library(HilbertCurve)
library(IRanges)
library(geosphere)
library(cowplot)

# I used this online tool: https://www.alltrails.com/explore/ 
# to draw a map of my walk and downloaded the coordinates and elevation of the route as a csv

dat <- read.csv('walk_04.04.21.csv')

# use distHaversine to calculate distance between longitude/latitude points
# https://stackoverflow.com/questions/31668163/geographic-geospatial-distance-between-2-lists-of-lat-lon-points-coordinates
dist <- c(0)
for(i in 1:(nrow(dat)-1)){
  dist <- c(dist, distHaversine(c(dat$Longitude[i], dat$Latitude[i]),
                                c(dat$Longitude[i+1], dat$Latitude[i+1])))
}
dat$dist_from_home <- cumsum(dist)

# create color palette
pal_func <- colorRampPalette(c("#51127C", "#B63679", "#FB8861", "#FCFDBF"))
cols <- pal_func(100)

# scale elevation between 0-100 to get corresponding color
dat$Elevation_norm <- (dat$Elevation/max(dat$Elevation))*100
col <- cols[dat$Elevation_norm]

# create iranges object for segments
ir <- IRanges(start = round(dat$dist_from_home[1:(nrow(dat)-1)]),
              end = ceiling(dat$dist_from_home[2:nrow(dat)]))

# create dummy plot to get legend
p <- ggplot(data.frame(x = dat$Elevation_norm), aes(x = x, y= 1, col = x)) +
  geom_point() +
  scale_color_gradientn(colors = pal_func(100), name = 'Elevation\n', breaks = seq(0, 100, 25), 
                        labels = paste0(quantile(dat$Elevation, seq(0,1,.25)), 'm')) +
  theme(legend.text = element_text(color = 'white', size = 8),
        legend.title = element_text(color = "white", size = 9),
        legend.margin = margin(0,10,10,-40),
        legend.background = element_rect(fill = "black"))
legd <- get_legend(p)

# draw and save Hilbert curve
png('07-physical.png', bg = 'black', width = 1400, height = 1000, res = 300)
hc <- HilbertCurve(min(dat$dist_from_home), max(dat$dist_from_home), 
                  level = 5, reference = T, arrow = F, legend = legd, 
                  start_from = 'topleft', first_seg = 'horizontal', 
                  background_col = 'black', padding = unit(5.25, 'mm'),
                  title = 'Elevation (in metres above sea level) along my walk last weekend.',
                  title_gp = gpar(fontsize = 10, col = 'white'), reference_gp = gpar(col = 'white', lty = 1.2))
hc_segments(hc, ir, gp = gpar(lwd = 3, col = col))
hc_text(hc, IRanges(start = c(0, max(dat$dist_from_home)+1), end = c(0, max(dat$dist_from_home)+1)), 
        labels = c('Home\n', 'Home\n'), gp = gpar(fontsize = 8, col = 'white'))
hc_text(hc, IRanges(start = 4000, end = 4000), 
        labels = c('\n\n\n\n\n\nEach segment of the curve represents ~15m of distance.'), 
        gp = gpar(fontsize = 6, col = 'white'))
dev.off()

