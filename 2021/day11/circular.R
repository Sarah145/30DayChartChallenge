library(tidyverse) 
library(colorfindr)

library(dplyr)
library(ggplot2)
library(tidyr)
library(colorspace)
library(purrr)

df <- get_colors('~/Downloads/IMG_7511.JPG')

convert_shades = df %>%
  mutate(rgb = map(col_hex, hex2RGB),
         hcl = map(rgb, ~as(.x, "polarLUV")),
         hcl = map(hcl, ~as_tibble(attr(.x, "coords")))) %>%
  unnest(c(hcl)) %>%
  mutate() %>%
  arrange(H) %>%
  mutate(hue_group = case_when(
    H >= 0 & H < 30 ~ 0.1,
    H >= 30 & H < 60 ~ 1.1,
    H >= 60 & H < 90 ~ 2.1,
    H >= 90 & H < 120 ~ 3.1,
    H >= 120 & H < 150 ~ 4.1,
    H >= 150 & H < 180 ~ 5.1,
    H >= 180 & H < 210 ~ 6.1,
    H >= 210 & H < 240 ~ 7.1,
    H >= 240 & H < 270 ~ 8.1,
    H >= 270 & H < 300 ~ 9.1,
    H >= 300 & H < 330 ~ 10.1,
    H >= 330 & H <= 360 ~ 11.1
  ))

convert_shades <- convert_shades %>% mutate(hex = fct_reorder(col_hex, C))

bg_raster <- as.raster(ggplot(aes(x = seq(1,10), y = 1, fill = seq(1:10))+geom_col(position = 'fill') + scale_fill_identity() + theme_void()))
shades = convert_shades %>%
  arrange(hue_group, L) %>%
  group_by(hue_group) %>%
  mutate(y = row_number())

# FIXED
outline_vert = tibble(
  x = seq(-12, 12.0, by = 1),
  xend = x,
  y = 0, 
  yend = max(shades$y) + 1
)

outline_horz = tibble(
  x = seq(-11.9, 11.9, by = 1),
  xend = x + .8,
  y = max(shades$y) + 1,
  yend = max(shades$y) + 1,
  label = c(seq(360, 120, by = -30),
            paste0("0", seq(90, 30, by = -30)),
            paste0("0", seq(30, 90, by = 30)),
            seq(120, 360, by = 30)),
  #seq(12, 1, by = -1),
  #seq(1, 12, by = 1)),
  label_x = x + .4,
  angle = seq(360, 0, length.out = 24)
)



inner_vert = tibble(
  x = c(-12, 0),
  xend = x,
  y = -10,
  yend = 0
)

systemfonts::register_variant(name = "Aleo Regular", family = "Aleo", weight = "normal")
#temp = systemfonts::font_info(family = "Aleo")

ggplot() +
  # Right shades
  geom_segment(data = shades,
               aes(x = hue_group, xend = hue_group + 0.8,
                   y = y, yend = y, color = col_hex),
               size = 2) +
  # Top Labels
  geom_segment(data = outline_horz,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               color = "#272726",
               size = 0.25) +
  geom_text(data = outline_horz,
            aes(x = label_x, y = y + 1.75, label = label, angle = angle),
            color = "#272726") +
  # Outline
  geom_segment(data = outline_vert,
               aes(x = x, y = y, 
                   xend = xend, yend = yend),
               color = "#272726",
               size = 0.25) +
  geom_segment(data = outline_horz,
               aes(x = x, xend = xend,
                   y = 0, yend = 0),
               color = "#272726",
               size = 0.25) +
  geom_segment(data = inner_vert,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               color = "#272726",
               size = 0.25) +
  annotate("text", x = -5, y = -8.25, label = "tidytuesday", 
           lineheight = 0.75,
           color = "#272726",
           angle = 270) +
  annotate("text", x = 5, y = -8.25, label = "rtistry", 
           lineheight = 0.75,
           color = "#272726",
           angle = 90) +
  # The extras
  labs(title = "Colors Featured: #rtistry vs. #TidyTuesday",
       subtitle = "The hex codes used in all of my pieces are grouped by \nsimilar hue and arranged with increasing lightness",
       caption = "Viz: @ijeamaka_a  \n") +
  scale_color_identity() +
  xlim(-12, 12) +
  ylim(-10, NA) +
  coord_polar(clip = "off") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#f2f2f2",
                                        color = "#f2f2f2"),
        plot.background = element_rect(fill = "#f2f2f2",
                                       color = "#f2f2f2"),
        plot.title = element_text(hjust = 0.5,
                                  size = 25),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 15),
        plot.caption = element_text(size = 8,
                                    hjust = 1))
