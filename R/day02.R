library(tidyverse)
library(sf)
library(ggpmthemes)

theme_set(theme_poppins())

# Data
# https://mapcruzin.com/free-denmark-arcgis-maps-shapefiles.htm

dk_road <-
  read_sf("data/day02/denmark-roads-shape/roads.shp") %>%
  # sample_frac(0.1) %>%
  mutate(length = as.numeric(st_length(.)))

dk_railroad <-
  st_read("data/day02/denmark-railways-shape/railways.shp")

dk_region <-
  read_sf("data/day02/DNK_adm/DNK_adm2.shp")

dk_outline <- dk_region %>%
  st_union() %>%
  rmapshaper::ms_simplify(0.01) %>%
  st_buffer(dist = 0.05)

# dk_outline %>%
#   ggplot() +
#   geom_sf()

p <- ggplot() +
  geom_sf(
    data = dk_outline,
    color = "#4d92a1",
    fill = "#033445",
    size = 0.5
  ) +
  geom_sf(
    data = dk_region,
    fill = "#5f9aa8",
    color = "grey75",
    size = 0.1
  ) +
  geom_sf(data = dk_road, color = "#033445", aes(size = length)) +
  geom_sf(data = dk_railroad, color = "#4d92a1", size = 0.25) +
  coord_sf(xlim = c(8, 13)) +
  scale_size(range = c(0.1, 0.5)) +
  labs(
    title = "The roads of Denmark",
    caption = "Day 2 of the #30DayMapChallenge (Lines)\nData: https://mapcruzin.com/free-denmark-arcgis-maps-shapefiles.htm\n@philmassicotte"
  ) +
  theme(
    panel.background = element_rect(fill = "#033445", color = "#033445"),
    plot.background = element_rect(fill = "#033445", color = "#033445"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    plot.title = element_text(color = "#4d92a1", hjust = 0.5, size = 20),
    plot.caption = element_text(color = "#4d92a1", size = 6)
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day02.pdf")

ggsave(destfile,
  device = cairo_pdf
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day02.png")
png::writePNG(bitmap, destfile)
