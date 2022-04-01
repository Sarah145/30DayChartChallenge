library(dplyr)
library(forcats)
library(ggplot2)
library(stringr)
library(cowplot)

sysfonts::font_add_google(name = "Roboto Mono")
showtext::showtext_auto()

# Create dataframe
data <- tribble(
  ~category, ~count, ~label_x, ~label_y,
  "Meetings", 2, 0.9, 1.3,
  "Lunch", 1, 0.99, 0.3,
  "Coding", 0.5, 0.9, -0.25,
  "Watching youtube videos while waiting for code to run", 3, 0.5, -0.5,
  "Making pretty plots I don't need while waiting for code to run", 2, 0.5, 1.45
)

data <- data %>% mutate(fraction = count/sum(count),
                ymax = cumsum(fraction),
                ymin = c(0, head(ymax, n=-1)),
                category = str_wrap(fct_reorder(category, fraction), width = 24))

cols <- c("#ffc15e","#d7f2ba","#97dffc","#a188a6","#ff6b6b")

# Plot data
p1 <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect(show.legend = F) +
  scale_fill_manual(values = cols) +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#333333", color = 'transparent'),
        panel.background = element_rect(fill = "#333333", color = 'transparent'),
        plot.margin = margin(55,55,55,55))

# Plot annotations
p2 <- ggplot(data) + 
  geom_text(aes(label_x,label_y, label = category, col = category), 
            show.legend = F,
            family = "Roboto Mono",
            size = 4.5) +
  scale_color_manual(values = cols) +
  theme_void() +
  theme(plot.margin = margin(35,35,35,35)) +
  coord_cartesian(clip = 'off') + 
  ylim(c(-0.5, 1.5)) + 
  xlim(c(0.375, 1.005))

# Draw plot
ggdraw()+
  draw_plot(p1) +
  draw_plot(p2)

ggsave('01-part-to-whole.png', bg = 'transparent', dpi = 100)
