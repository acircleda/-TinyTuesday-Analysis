library(tidyverse)
library(palmerpenguins)
library(colorspace)

# bad plot
penguins %>%
  ggplot()+
  geom_col(aes(y=island, x=body_mass_g, fill=sex))+
  ggtitle("Penguin Sex and Bodymass by Island")


df_penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv') %>% 
  mutate(species = if_else(species == "Adelie", "Adélie", species))





```{r data-prep-raincloudplot}
df_rect <-
  tibble(
    xmin = c(-Inf, 2.46, 3.27),
    xmax = c(Inf, Inf, Inf),
    ymin = c(3, 2, 1),
    ymax = c(Inf, Inf, Inf)
  )
df_peng_iqr <- 
  df_penguins %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  filter(!is.na(bill_ratio)) %>% 
  group_by(species) %>% 
  mutate(
    median = median(bill_ratio),
    q25 = quantile(bill_ratio, probs = .25),
    q75 = quantile(bill_ratio, probs = .75),
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(species_num = as.numeric(fct_rev(species))) 
url <- "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/man/figures/lter_penguins.png"
img <- png::readPNG(RCurl::getURLContent(url))
i2 <- grid::rasterGrob(img, interpolate = T)
```
pal <- c("#FF8C00", "#A034F0", "#159090")
```{r raincloudplot}

ggplot(df_peng_iqr, aes(bill_ratio, species_num - .2)) +
  geom_rect(
    data = df_rect,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax
    ),
    inherit.aes = F,
    fill = "white"
  ) +
  geom_linerange(
    data = df_peng_iqr %>% 
      group_by(species, species_num) %>% 
      summarize(m = unique(median)),
    aes(
      xmin = -Inf, 
      xmax = m, 
      y = species_num,
      color = species
    ),
    inherit.aes = F,
    linetype = "dotted",
    size = .7
  ) +
  geom_boxplot(
    aes(
      color = species,
      color = after_scale(darken(color, .1, space = "HLS"))
    ),
    width = 0,
    size = .9
  ) +
  geom_rect(
    aes(
      xmin = q25,
      xmax = median,
      ymin = species_num - .05,
      ymax = species_num - .35
    ),
    fill = "grey89"
  ) +
  geom_rect(
    aes(
      xmin = q75,
      xmax = median,
      ymin = species_num - .05,
      ymax = species_num - .35
    ),
    fill = "grey79"
  ) +
  geom_segment(
    aes(
      x = q25, 
      xend = q25,
      y = species_num - .05,
      yend = species_num - .35,
      color = species,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    size = .25
  ) +
  geom_segment(
    aes(
      x = q75, 
      xend = q75,
      y = species_num - .05,
      yend = species_num - .35,
      color = species,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    size = .25
  ) +
  geom_point(
    aes(
      color = species
    ), 
    shape = "|",
    size = 5,
    alpha = .33
  ) +
  ggdist::stat_halfeye(
    aes(
      y = species_num,
      color = species,
      fill = after_scale(lighten(color, .5))
    ),
    shape = 18,
    point_size = 3,
    interval_size = 1.8,
    adjust = .5,
    .width = c(0, 1)
  ) +
  geom_text(
    data = df_peng_iqr %>% 
      group_by(species, species_num) %>% 
      summarize(m = unique(median)),
    aes(
      x = m, 
      y = species_num + .12,
      label = format(round(m, 2), nsmall = 2)
    ),
    inherit.aes = F,
    color = "white",
    family = "Neutraface Slab Display TT Titl",
    size = 3.5
  ) +
  geom_text(
    data = df_peng_iqr %>% 
      group_by(species, species_num) %>% 
      summarize(n = unique(n), max = max(bill_ratio, na.rm = T)),
    aes(
      x = max + .01, 
      y = species_num + .02,
      label = glue::glue("n = {n}"),
      color = species
    ),
    inherit.aes = F,
    family = "Neutraface Slab Display TT Bold",
    size = 3.5,
    hjust = 0
  ) +
  annotation_custom(i2, ymin = 2.5, ymax = 3.6, xmin = 3, xmax = 3.7) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    limits = c(1.57, 3.7),
    breaks = seq(1.6, 3.6, by = .2),
    expand = c(.001, .001)
  ) +
  scale_y_continuous(
    limits = c(.55, NA),
    breaks = 1:3,
    labels = c("Gentoo", "Chinstrap", "Adélie"),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = pal,
    guide = F
  ) +
  scale_fill_manual(
    values = pal,
    guide = F
  ) +
  labs(
    x = "Bill ratio",
    y = NULL,
    title = "Distribution of the bill ratio of Palmer penguin species",
    subtitle = "The following graph shows how bill ratio is distributed among Palmer penguin species.\nYou can see the overall distribution above the colored line and individuals below the colored line.\nDots represent outliers.",
    caption = 'Note: Bill ratio is estimated as bill length divided by bill depth.\nVisualization: Adapted from Cédric Scherer\nData: 10.1371/journal.pone.0090081\nIllustrations: Allison Horst'
  )+
  theme_minimal()+
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(family = "Arial", 
                               color = rev(pal), size = 14, lineheight = .9),
    axis.ticks.length = unit(0, "lines"),
    plot.title = element_text(color="black", size=14, face = "bold"),
    plot.subtitle = element_text(color="grey50", size=12)
  )
```

```{r panel-save-convert, fig.width = 9, fig.height = 14}
path <- here::here("plots", "2020_31", "2020_31_PalmerPenguins")
plot_grid(scat, rain, ncol = 1, rel_heights = c(1, .75)) +
  ggsave(glue::glue("{path}.pdf"), width = 9, height = 14, device = cairo_pdf)
#ggsave(here::here("dev", glue::glue("2020_31___{format(Sys.time(), '%Y%m%d_%H%M%S')}.pdf")),
#       width = 9, height = 14, device = cairo_pdf)
pdf_convert(pdf = glue::glue("{path}.pdf"), format = "png", dpi = 300, 
            filenames = glue::glue("{path}.png"))
```
