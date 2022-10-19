library(tidyverse)
library(ggpmthemes)
library(sf)
library(maps)

theme_set(theme_exo2())

# https://openflights.org/data.html#route

url_airports <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
url_routes <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"

airports <- read_csv(
  url_airports,
  col_names = c(
    "airport_id",
    "name",
    "city",
    "country",
    "iata",
    "icao",
    "latitude",
    "longitude",
    "altitude",
    "timezone",
    "dst",
    "tz_database_timezone",
    "type",
    "source"
  ),
  na = c("\\N", "")
) %>%
  filter(type == "airport") %>%
  select(iata, longitude, latitude)

routes <- read_csv(
  url_routes,
  col_names = c(
    "airline",
    "airline_id",
    "source_airport",
    "source_airport_id",
    "destination_airport",
    "destination_airport_id",
    "code_share",
    "stops",
    "equipment"
  ),
  na = c("\\N", "")
) %>%
  select(-contains("id"), -code_share, -airline, -equipment) %>%
  distinct()

df <- routes %>%
  inner_join(airports, by = c("source_airport" = "iata")) %>%
  rename_at(vars(longitude, latitude), ~paste0("source_", .)) %>%
  inner_join(airports, by = c("destination_airport" = "iata")) %>%
  rename_at(vars(longitude, latitude), ~paste0("destination_", .)) %>%
  filter((source_longitude != destination_longitude) | (source_latitude != destination_latitude))

sf_world <- st_as_sf(rworldmap::getMap(resolution = "low"))

df %>%
  count(stops)

df %>%
  count(source_airport, sort = TRUE)

df <- df %>%
  st_as_sf(coords = c("source_longitude", "source_latitude"), crs = 4326) %>%
  st_transform(crs = 54009) %>%
  cbind(., st_coordinates(.)) %>%
  as_tibble() %>%
  select(-geometry) %>%
  rename(
    source_longitude = X,
    source_latitude = Y
  ) %>%
  st_as_sf(coords = c("destination_longitude", "destination_latitude"), crs = 4326) %>%
  st_transform(crs = 54009) %>%
  cbind(., st_coordinates(.)) %>%
  as_tibble() %>%
  select(-geometry) %>%
  rename(
    destination_longitude = X,
    destination_latitude = Y
  )

p <- df %>%
  add_count(source_airport) %>%
  top_n(500) %>%
  ggplot(
    aes(
      x = source_longitude,
      y = source_latitude,
      xend = destination_longitude,
      yend = destination_latitude
    )
  ) +
  geom_sf(data = sf_world, inherit.aes = FALSE, size = 0.1, fill = "#333333", color = "gray75") +
  geom_curve(curvature = 0.15, size = 0.25, alpha = 0.25, color = "#D3BA68") +
  coord_sf(crs = 54009) +
  labs(
    title = "Flight routes around the world",
    subtitle = "Based on the top 500 airports offering the highest number of destinations",
    caption = "#30daymapchallenge (Movement) | Data: https://openflights.org/data.html | @philmassicotte"
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
    plot.subtitle = element_text(color = "white", hjust = 0.5, size = 10),
    plot.caption = element_text(color = "gray75", size = 6, hjust = 0),
    axis.title = element_blank()
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day12.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 5.52,
  height = 4.68
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day12.png")
png::writePNG(bitmap, destfile)
