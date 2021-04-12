library(tidyverse) 
library(colorfindr)

df <- get_colors('IMG_7511.JPG')

ggplot(df, aes(x = 0, y = col_share, fill = col_hex)) +
  geom_col(position = 'fill', show.legend = F) +
  geom_text(data = data.frame(x = 0, y = 0.5, 
                              label = 'The distribution (and lack) of colour\nin my wardrobe'), 
            aes(x=x,y=y,label=label), inherit.aes = F, col = 'white', size = 10) +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_reverse(expand = c(0,0)) +
  theme_void() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(0,0,0,0)) +
  coord_flip()

ggsave('12-strips.png', dpi = 500)
