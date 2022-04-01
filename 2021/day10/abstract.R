library(tidyverse)
library(vcfR)
library(ggforce)
library(ggfx)
library(ggtext)

# read in vcf
my_vcf <- read.vcfR('../../Sarah-Seq/29028312179.grch37.snp.vcf.gz')
my_vcf <- subset(my_vcf, my_vcf@fix[,'CHROM'] %in% paste0('chr', c(seq(1:22), 'X')))

# get variants
df <- data.frame(chr = my_vcf@fix[, 'CHROM'], 
                 ref = my_vcf@fix[, 'REF'],
                 alt = my_vcf@fix[, 'ALT'])

df <- df %>% mutate(chr = str_extract(chr, '[^a-z]+'),
                    ref = ifelse(ref %in% c('A', 'C', 'G', 'T'), ref, 'other'),
                    alt = ifelse(alt %in% c('A', 'C', 'G', 'T'), alt, 'other'),
                    var = factor(case_when(
                      ref == 'other' ~ 'other',
                      alt == 'other' ~ 'other',
                      T ~ paste0(ref, '&rarr;', alt)))) %>%
  group_by(chr, var) %>%
  tally() %>%
  mutate(percent  = n/sum(n),
         log_n = log2(n),
         scale_per = percent*360,
         var = fct_infreq(var),
         chr = factor(chr, levels = c(seq(1:22), 'X'))) %>%
  arrange(wt = -n)

# set color palette
var_cols <- c('#FFDD18', '#8CC63F', '#F05A29', '#9C161A', '#009246', '#A1499B', '#006589', '#EC008B',
              '#8C1F54', '#917600', '#ED2B35',  '#F5AA42',  '#00ADEE')
names(var_cols) <- c(sort(unique(as.character(df$var[df$var != 'other']))), 'other')

# plot with chromosome facets
ggplot(df) +
  with_blur(geom_ellipse(aes(x0 = 0, y0 = 0, a = log_n, b = log_n*pi, angle = scale_per, col = var), 
                         show.legend = T, key_glyph = draw_key_point, size = 2), sigma = 4.5) + # draw ellipses
  geom_ellipse(aes(x0 = 0, y0 = 0, a = log_n, b = log_n*pi, angle = scale_per, col = var), 
               show.legend = F) + 
  scale_color_manual(values = var_cols, breaks = c(sort(unique(as.character(df$var[df$var != 'other']))), 'other')) +
  guides(colour = guide_legend(override.aes = list(size = 3), ncol = 5)) +
  labs(caption = '\nData: my genome | Inspiration: printmydna.com | @sarahe145 | #30DayChartChallenge') +
  theme_void(base_size = 22) +
  theme(legend.text = element_markdown(color = 'gray90', size = 18),
        panel.background = element_rect(fill = 'black', color = 'black'),
        plot.background = element_rect(fill = 'black', color = 'black'),
        strip.text = element_text(colour = 'gray90'),
        legend.position = 'bottom',
        legend.box.margin = margin(t = 20),
        plot.margin = margin(20, 20, 20, 20),
        plot.caption = element_text(color = 'gray80', size = 14, hjust = 0.5)) +
  facet_wrap(~chr, strip.position = 'bottom', scales = 'free', nrow = 6) +
  coord_cartesian(clip = 'off')

ggsave('10-abstract.png', width = 10, height = 11)


# plot with all chromosomes overlaid
ggplot(df) +
  with_blur(geom_ellipse(aes(x0 = 0, y0 = 0, a = log_n, b = log_n*15, angle = scale_per*pi,  col = var), 
                         size = 1.8, show.legend = F), sigma = 2) + # draw ellipses
  geom_ellipse(aes(x0 = 0, y0 = 0, a = log_n, b = log_n*15, angle = scale_per*pi,  col = var), 
               size = 0.4, show.legend = F) +
  scale_color_manual(values = alpha(var_cols, 0.5)) + 
  labs(caption = '\nData: my genome | Inspiration: printmydna.com | @sarahe145 | #30DayChartChallenge') +
  theme_void(base_size = 22) +
  theme(panel.background = element_rect(fill = 'black', color = 'black'),
        plot.background = element_rect(fill = 'black', color = 'black'),
        strip.text = element_text(colour = 'gray90'),
        plot.margin = margin(20, 20, 20, 20),
        plot.caption = element_text(color = 'gray80', size = 14, hjust = 0.5)) +
  coord_cartesian(clip = 'off')

ggsave('10-abstract1.png', width = 10, height = 10, dpi = 400)
