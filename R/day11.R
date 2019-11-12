library(tidyverse)
library(ggpmthemes)
library(sf)
library(rvest)
library(maps)

theme_set(theme_maven())

# url <- "https://en.wikipedia.org/wiki/List_of_countries_by_average_elevation"
url <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_elevation"

df <- read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  select(region = statefederal_district_or_territory, highest_point, highest_elevation) %>%
  mutate(region = str_to_lower(region))

elevation <- df %>%
  extract(highest_elevation,
    into = c("maximum_elevation", NULL),
    regex = "ft(-?[0-9]\\d*[\\.\\d+]?)",
    convert = TRUE
  )

us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  rename(region = ID)

us_states %>%
  inner_join(elevation) %>%
  ggplot() +
  geom_sf(aes(fill = maximum_elevation), color = "#333333", size = 0.25) +
  coord_sf(crs = 102008) +
  rcartocolor::scale_fill_carto_c(
    palette = "Magenta",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(20, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      label.position = "bottom",
      title = "Maximum elevation (m)"
    )
  ) +
  labs(
    title = "Maximum elevation by U.S. states",
    caption = "#30daymapchallenge (Elevation) | Data: Wikipedia | @philmassicotte"
  ) +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    legend.background = element_rect(fill = "#333333"),
    legend.key = element_blank(),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = 0.5),
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "gray75", size = 8)
  )

ggsave(
  here::here("graphs", "day11.png"),
  type = "cairo",
  device = "png",
  dpi = 600,
  width = 5.52 * 1.15,
  height = 4.68 * 1.15
)
