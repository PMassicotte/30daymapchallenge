library(tidyverse)
library(ggpmthemes)
library(sf)
library(glue)
library(ggtext)

theme_set(theme_light_modified(base_family = "Yanone Kaffeesatz", base_size = 16))

ne <- st_read("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_time_zones.geojson")

ne %>%
  # filter(str_detect(places, "Canada")) %>%
  ggplot() +
  geom_sf()

shp <- st_read("data/day14/canada/tz_canada.shp") %>%
  janitor::clean_names() %>%
  mutate(tzid = str_remove(tzid, "America/")) %>%
  mutate(tzid = str_replace_all(tzid, "_", " ")) %>%
  st_join(ne, largest = TRUE)

# Plot --------------------------------------------------------------------

p <- shp %>%
  ggplot() +
  geom_sf(aes(fill = utc_format), show.legend = TRUE, color = NA) +
  coord_sf(crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45") +
  rcartocolor::scale_fill_carto_d(
    palette = "BluYl",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(12, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      label.position = "top",
      title = NULL,
      label.theme = element_text(size = 8, color = "white", family = "Yanone Kaffeesatz")
    )
  ) +
  labs(
    title = "Canada timezone borders",
    caption = "#30daymapchallenge (Borders) | Data: http://efele.net/maps/tz/canada/ | • @p? @philmassicotte"
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
    plot.title = element_text(color = "gray50", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5, size = 10),
    plot.caption = element_text(color = "gray75", size = 8, hjust = 0.5, family = "Yanone Kaffeesatz Light"),
    axis.title = element_blank()
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day14.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 5,
  height = 6
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day14.png")
png::writePNG(bitmap, destfile)

