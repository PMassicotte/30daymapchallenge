library(tidyverse)
library(ggpmthemes)
library(sf)
library(ggmap)
library(httr)
library(readxl)

theme_set(theme_light_modified(base_family = "Alatsi"))

GET("https://query.data.world/s/gqkvcrgerj75ptvfsm34eybjqcrd5z", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

mb <- df %>%
  janitor::clean_names() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


df2 <- st_read("data/day27/mrc_SHP/mrc_ligne.shp")


# Plot --------------------------------------------------------------------

p <- df2 %>%
  ggplot() +
  geom_sf(size = 0.2, color = "gray40") +
  geom_sf(data = mb, color = "#3c3c3c", size = 0.25) +
  # stat_sf(geom = "density2d") +
  coord_sf(crs = 32198) +
  labs(
    title = "Beer resources in Québec",
    caption = "#30daymapchallenge (Resources) | Data: https://data.world/maxclem/microbrasseriesquebec | @philmassicotte",
    subtitle = str_wrap("The microbrewery industry is growing in Québec. This essential resource is a delight for many beer lovers!", 70)
  ) +
  theme(
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      color = "#3C3C3C",
      hjust = 0.5,
      size = 20,
      face = "bold"
    ),
    plot.caption = element_text(
      color = "#3C3C3C",
      size = 5,
      hjust = 0.5
    ),
    plot.subtitle = element_text(color = "gray25", size = 8, hjust = 0.5),
    legend.key = element_rect(size = 2, colour = NA),
    legend.key.size = unit(0.25, "cm"),
    legend.text = element_text(size = 6, color = "white"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "#ABB7B7"),
    panel.background = element_rect(fill = "#ABB7B7"),
    legend.background = element_rect(fill = "#ABB7B7")
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day27.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 3.52 * 1.1,
  height = 4.68 * 1.1
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day27.png")
png::writePNG(bitmap, destfile)
