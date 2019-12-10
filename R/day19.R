library(tidyverse)
library(ggpmthemes)
library(sf)
library(rvest)
library(maps)
library(maptools)

theme_set(theme_exo2())

# st_as_sf(map("world", plot = FALSE, fill = TRUE))

data(wrld_simpl)

wrld_simpl <- wrld_simpl %>%
  st_as_sf()

urband <- read_csv(here::here("data", "day19", "urban-vs-rural-majority.csv")) %>%
  janitor::clean_names()

dat <- wrld_simpl %>%
  left_join(urband, by = c("ISO3" = "code"))

p <- dat %>%
  filter(year %in% c(1800, 1900, 2000, 2050)) %>%
  ggplot() +
  geom_sf(data = wrld_simpl, fill = "white", size = 0.1) +
  geom_sf(aes(fill = urban_percent / 100), size = 0.1) +
  coord_sf(crs = "+proj=moll") +
  facet_wrap(~year) +
  rcartocolor::scale_fill_carto_c(
    palette = "Teal",
    labels = scales::percent,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(12, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      label.position = "bottom",
      label.theme = element_text(size = 8, color = "white", family = "Yanone Kaffeesatz"),
      title.theme = element_text(size = 12, color = "white", family = "Yanone Kaffeesatz")
    )
  ) +
  labs(
    title = "The past, present and future of urban areas",
    caption = "#30daymapchallenge (Urban) | Data: https://ourworldindata.org/urbanization | @philmassicotte",
    fill = "Proportion of urban areas"
  ) +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_line(size = 0.1, color = "gray75"),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    legend.background = element_rect(fill = "#333333"),
    legend.key = element_blank(),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = "gray75", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5, size = 10),
    plot.caption = element_text(color = "gray75", size = 8, hjust = 0.5, family = "Yanone Kaffeesatz Light"),
    axis.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "gray75", size = 20, face = "bold")
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day19.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 5.52 * 1.15,
  height = 4.68 * 1.15
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day19.png")
png::writePNG(bitmap, destfile)
