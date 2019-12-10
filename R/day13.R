library(tidyverse)
library(ggpmthemes)
library(sf)
library(maps)
library(glue)
library(ggtext)

theme_set(theme_maven())

# http://donnees.ville.montreal.qc.ca/dataset/984f7a68-ab34-4092-9204-4bdfcca767c5/resource/9d3d60d8-4e7f-493e-8d6a-dcd040319d8d/download/geobase.json

cycle <- st_read("http://donnees.ville.montreal.qc.ca/dataset/5ea29f40-1b5b-4f34-85b3-7c67088ff536/resource/0dc6612a-be66-406b-b2d9-59c9e1c65ebf/download/reseau_cyclable_2018_c.geojson") %>%
  janitor::clean_names()

names(cycle)

cycle %>%
  mutate(longueur = as.numeric(as.character(longueur))) %>%
  ggplot() +
  geom_sf(aes(color = type_voie))


roads <- st_read("http://donnees.ville.montreal.qc.ca/dataset/984f7a68-ab34-4092-9204-4bdfcca767c5/resource/9d3d60d8-4e7f-493e-8d6a-dcd040319d8d/download/geobase.json")


# Plot --------------------------------------------------------------------

len <- cycle %>%
  st_length() %>%
  sum() / 1000 %>%
  round(digits = 0)

cycle %>%
  as_tibble() %>%
  count(nom_arr_vi, sort = TRUE)

mylabel <- glue(
  "Montreal contains **{round(len, digits = 0)} km** of cyclable bike<br>track distributed in 31 boroughs. **Saint-Laurent**,<br>**Rivière-des-Prairies** and **Hochelaga** are the<br>districs containing the highest number of<br>cyclable tracks."
) %>%
  enframe(value = "txt", name = NULL) %>%
  mutate( x = -74.1, y = 45.6)

p <- roads %>%
  ggplot() +
  geom_sf(size = 0.1, color = "gray75") +
  geom_sf(data = cycle, color = "white", size = 0.25) +
  # geom_sf(data = cycle, size = 0.25, aes(color = nom_arr_vi), show.legend = FALSE) +
  coord_sf() +
  geom_richtext(
    data = mylabel,
    aes(x = x, y = y, label = txt),
    fill = NA,
    label.color = NA,
    hjust = 0,
    color = "white",
    family = "Exo 2"
  ) +
  labs(
    title = "Montreal cyclable bike tracks",
    caption = "#30daymapchallenge (Tracks) | Data: http://donnees.ville.montreal.qc.ca | @philmassicotte"
  ) +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#596F7E"),
    panel.background = element_rect(fill = "#596F7E"),
    legend.background = element_rect(fill = "#596F7E"),
    legend.key = element_blank(),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, size = 10),
    plot.caption = element_text(color = "gray75", size = 6, hjust = 0),
    axis.title = element_blank()
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day13.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 5.52,
  height = 4.68
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day13.png")
png::writePNG(bitmap, destfile)
