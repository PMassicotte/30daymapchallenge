library(tidyverse)
library(ggpmthemes)
library(sf)

theme_set(theme_light_modified(base_family = "Exo", base_size = 16))

ws <-
  st_read(
    "data/day26/Watersheds_Shapefile/NA_Watersheds/data/NA_Watersheds/watershed_p_v2.shp"
  )

# Plot --------------------------------------------------------------------

p <- ws %>%
  filter(COUNTRY %in% c("CAN", "USA")) %>%
  ggplot() +
  geom_sf(aes(fill = NAW1_EN, color = NAW1_EN), size = 0.1) +
  coord_sf() +
  # paletteer::scale_fill_paletteer_d(rcartocolor, Temps) +
  scale_fill_manual(
    breaks = na.omit(as.character(unique(ws$NAW1_EN))),
    values = shades::saturation(paletteer::paletteer_d(rcartocolor, Temps, n = 5), 0.25)
  ) +
  scale_color_manual(
    breaks = na.omit(as.character(unique(ws$NAW1_EN))),
    values = shades::saturation(paletteer::paletteer_d(rcartocolor, Temps, n = 5), 1),
    guide = "none"
  ) +
  labs(
    title = "The watersheds of North America",
    caption = "#30daymapchallenge (Hydrology) | Data: http://www.cec.org/tools-and-resources/map-files/watersheds | @philmassicotte"
  ) +
  theme(
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      color = "gray75",
      hjust = 0.5,
      size = 20,
      face = "bold"
    ),
    plot.caption = element_text(
      color = "gray50",
      size = 6,
      hjust = 0.5
    ),
    legend.key = element_rect(size = 2, colour = NA),
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 6, color = "white"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    legend.background = element_rect(fill = "#333333")
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      label.position = "top",
      keyheight = unit(2, "mm"),
      color = NA
    )
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day26.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 5.52 * 1.15,
  height = 4.68 * 1.15
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day26.png")
png::writePNG(bitmap, destfile)
