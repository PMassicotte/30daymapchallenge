library(tidyverse)
library(sf)
library(ggpmthemes)

theme_set(theme_exo(base_family = "Modern Antiqua"))

url <-
  pins::pin(
    "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
  )

airports <- read_csv(
  url,
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
)

airports

airports %>%
  count(country, sort = TRUE) %>%
  top_n(32) %>%
  mutate(country = fct_reorder(country, n)) %>%
  ggplot(aes(x = country, y = n)) +
  geom_col() +
  coord_flip()

# https://slowkow.com/notes/ggplot2-color-by-density/
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

airports$density <-
  get_density(airports$longitude, airports$latitude, n = 100)

crs <- "+proj=wag5 +lon_0=0"

airports <- airports %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs) %>%
  cbind(., st_coordinates(.)) %>%
  rename(longitude = X, latitude = Y)

sf_world <- st_as_sf(rworldmap::getMap(resolution = "low"))

ggplot() +
  geom_sf(data = sf_world, size = 0.1, fill = "#e5e0b1") +
  coord_sf(crs = crs) +
  geom_point(
    data = airports,
    aes(x = longitude, y = latitude, color = density),
    size = 0.01,
    # shape = 21,
    fill = NA
  ) +
  theme(
    panel.background = element_rect(fill = "#cdb883", color = NA),
    plot.background = element_rect(fill = "#cdb883", color = NA),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_blank(),
    plot.title = element_text(color = "#3c3c3c"),
    plot.subtitle = element_text(color = "#3c3c3c"),
    plot.caption = element_text(color = "#3c3c3c", size = 6)
  ) +
  scale_color_viridis_c(option = "B", trans = "sqrt") +
  labs(
    title = "Airports around the World",
    subtitle = "Brigther areas indicate regions with higher density of airports.",
    caption = str_wrap("day 1 of the #30DayMapChallenge | Data: https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat | Dataviz: @philmassicotte", 250)
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day01.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  height = 8,
  width = 8
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day01.png")
png::writePNG(bitmap, destfile)
