library(plotly)
library(RColorBrewer)
Sys.setenv("plotly_username"="username")
Sys.setenv("plotly_api_key"="api_key")

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')

#data
nuc <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

nuc <- nuc %>% group_by(region) %>% add_tally()

plot_geo(nuc, locationmode = "country names", sizes = c(1, 300)) %>%
  add_markers(
    x= ~longitude, y = ~latitude, size = ~n, color = ~n, colors = "Reds", hoverinfo = "text",
    text = ~paste("<b>Region:</b> ", nuc$region,
                  "<br /><b>Country:</b> ", nuc$country,
                  "<br /><b>Number of tests:</b> ", nuc$n))

p <- plot_geo(locationmode = "country names", sizes = c(1, 300)) %>%
  add_markers(
    x = ~nuc$longitude, 
    y = ~nuc$latitude, 
    size = ~nuc$n, 
    color = ~nuc$n,
    colors = brewer.pal(11, "Spectral"),
    name='Number',
    text = ~paste("<b>Region:</b> ", nuc$region,
                  "<br /><b>Country:</b> ", nuc$country,
                  "<br /><b>Number of tests:</b> ", nuc$n),
    hoverinfo = "text"
) %>%
  layout(title = "<b>Nuclear Explosions: 1945-1996</b> (Zoom in for details)")

api_create(p, filename = "bubble-map")

