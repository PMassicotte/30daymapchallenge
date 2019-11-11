library(tidyverse)
library(vroom)
library(ggpmthemes)
library(sf)

theme_set(theme_maven())

sf_shp <- st_read(here::here("data/day10/", "Census 2010_ Tracts for San Francisco.geojson"))

stations <- read_csv(here::here("data/day10/", "station.csv")) %>%
  select(name:long, city) %>%
  filter(city == "San Francisco")

trips <- vroom(here::here("data/day10/", "trips.csv.gz")) %>%
  select(duration, contains("station_name")) %>%
  count(start_station_name,end_station_name, sort = TRUE) %>%
  top_n(250)

df <- trips %>%
  inner_join(stations, by = c("start_station_name" = "name")) %>%
  rename_at(vars(lat, long), ~paste0("start_", .)) %>%
  inner_join(stations, by = c("end_station_name" = "name")) %>%
  rename_at(vars(lat, long), ~paste0("end_", .))

p <- df %>%
  filter((start_long != end_long) | (start_lat != end_lat)) %>%
  ggplot(aes(
    x = start_long,
    y = start_lat,
    xend = end_long,
    yend = end_lat
  )) +
  geom_sf(
    data = sf_shp,
    inherit.aes = FALSE,
    size = 0.5,
    color = "#4F4F4F",
    fill = "black"
  ) +
  coord_sf(
    xlim = c(-122.43, -122.38),
    ylim = c(37.77, 37.805)
  ) +
  geom_curve(alpha = 0.5, aes(size = n), color = "gray75") +
  scale_size(range = c(0.1, 1)) +
  geom_point(
    data = stations,
    aes(x = long, y = lat),
    color = "white",
    inherit.aes = FALSE,
    size = 3
  ) +
  ggrepel::geom_text_repel(
    data = stations %>% filter(),
    aes(
      x = long,
      y = lat,
      label = str_wrap(name, 20)
    ),
    inherit.aes = FALSE,
    color = "white",
    size = 2,
    hjust = 1.2,
    segment.color = NA
  ) +
  labs(
    title = "San Francisco Bay Area Bike Share",
    subtitle = "The 250 most used routes around the San Francisco Bay area.",
    caption = "#30daymapchallenge (Black & White) | Data: https://www.kaggle.com/benhamner/sf-bay-area-bike-share | @philmassicotte"
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    axis.text = element_text(color = "gray75"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "gray75", size = 8)
  )

ggsave(
  here::here("graphs", "day10.png"),
  type = "cairo",
  device = "png",
  dpi = 600,
  width = 8,
  height = 8
)
