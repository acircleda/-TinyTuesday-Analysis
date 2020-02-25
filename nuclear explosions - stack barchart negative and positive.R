##from https://github.com/gkaramanis/tidytuesday/blob/master/week-34/nuclear.R

library(tidyverse)
library(ggplot2)
library(ggtext)

#data
nuc <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

#create "n" column with positive and negative values of 1
nucloc <- nuc %>%
  mutate(
    n = case_when(
      type == "UG" | type == "TUNNEL" | type == "GALLERY" | type == "SHAFT" | type == "SHAFT/GR" | type == "SHAFT/LG" | type == "MINE" | type == "UW" ~ -1,
      TRUE ~ 1
    )
  )

#create filters data frames for ggplot
nuc_pos <- subset(nucloc, n == 1)
nuc_neg <- subset(nucloc, n == -1)

# annotations
annot <- data.frame(
  label = c(
    "**Partial Test Ban Treaty (1963)**<br>Bans nuclear weapon tests in the atmosphere,<br>in outer space and under water",
    "**Comprehensive Nuclear-Test-Ban Treaty (1996)**<br>Bans all nuclear explosions,<br>for both civilian and military purposes,<br>in all environments"
      ),
  x = c(1964, 1995),
  y = c(115, 125),
  hjust = c(0, 1)
)
annot2 <- data.frame(
  label = c(
    "**Atmospheric**",
    "**Underground**"
  ),
  x = c(1945, 1945),
  y = c(120, -57),
  hjust = c(0, 0)
)

#plot
ggplot() +
  geom_vline(xintercept = 1963, color = "grey40", size = 1) + #vertical line at 1963
  geom_vline(xintercept = 1996, color = "grey40", size = 1) + #vertical line at 1996
  geom_richtext(data = annot,
                aes(label = label, x = x, y = y,
                    hjust = hjust), vjust = 1, size = 2.5,
                family = "IBM Plex Sans", show.legend = FALSE,
                fill = NA, label.color = NA) + #add labels
  geom_richtext(data = annot2,
                aes(label = label, x = x, y = y,
                    hjust = hjust), vjust = 1, size = 5,
                family = "IBM Plex Sans", show.legend = FALSE,
                fill = NA, label.color = NA) + #add labels
  geom_curve(aes(x = 1970, y = 94, xend = 1964, yend = 80), #x, y = start of line
             arrow = arrow(length = unit(0.02, "npc")),
             curvature = -0.3) +
  geom_curve(aes(x = 1992, y = 100, xend = 1995, yend = 90),
             arrow = arrow(length = unit(0.02, "npc")),
             curvature = 0.2) +
  
# positive values = atmospheric
  geom_bar(data = nuc_pos, aes(year, n, fill = country),
           stat = "identity") +
  # negative values = underground
  geom_bar(data = nuc_neg, aes(year, n, fill = country),
           stat = "identity")


labs(
  title = "Nuclear explosions, 1945-1998",
  subtitle = "Total number of nuclear explosions by year, grouped by country and by atmospheric or underground\ntype, as defined in the sipri report (2000)",
  caption = "Source: Stockholm International Peace Research Institute | Graphic: Georgios Karamanis"
) +
  
  scale_x_continuous(limits = c(1944, 1999), expand = c(0.02, 0),
                     breaks = seq(1945, 1999, 5),
                     minor_breaks = seq(1945, 2000, 1)) +
  scale_y_continuous(breaks = seq(-80, 120, 20),
                     minor_breaks = seq(-80, 120, 10),
                     labels = abs(seq(-80, 120, 20))) +
  
  scale_fill_manual(values = wes_palette("Zissou1", 7, type = "continuous"), labels = c("China", "France", "India", "Pakistan", "United Kingdom", "USA", "Soviet Union")) +
  guides(fill = guide_legend(ncol = 2)) +
  theme_minimal(base_size = 16, base_family = "IBM Plex Sans") +
  theme(
    legend.position = c(0.7, 0.625),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 20, margin = margin(3, 0, 7, 0)),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid.major = element_line(colour = "grey95"),
    panel.grid.minor = element_line(colour = "grey95"),
    axis.title = element_blank(),
    plot.title = element_text(family = "IBM Plex Sans Bold", size = 36, margin = margin(0, 0, 10, 0)),
    plot.subtitle = element_text(size = 24, margin = margin(0, 0, 30, 0)),
    plot.caption = element_text(family = "IBM Plex Sans Light", margin = margin(30, 0, 0, 0))
  )
