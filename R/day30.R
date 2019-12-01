library(tidyverse)
library(sf)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Aldrich"))

# https://www.statcan.gc.ca/eng/lode/databases/odb

df <- st_read("data/day30/ODB_Quebec/odb_quebec.shp")

df_qc <- df %>%
  filter(Data_prov == "Quebec") %>%
  st_crop(
    xmin = 7704584.3,
    ymin = 1418922.4,
    xmax = 7793602.0,
    ymax = 1460571.6
  )

# Plot Qc -----------------------------------------------------------------

p_qc <- df_qc %>%
  # sample_frac(0.01) %>%
  mutate(size_class = ifelse(Shape_Area >= 500, 500, Shape_Area)) %>%
  # mutate(size_class = santoku::chop(Shape_Area, breaks = c(seq(0, 1e3, by = 50), 1e6), labels = c(seq(50, 1e3, by = 50), 1e6))) %>%
  ggplot() +
  geom_sf(aes(color = size_class, fill = size_class),
    size = 0.05
  ) +
  rcartocolor::scale_fill_carto_c(
    palette = "Peach",
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
    labels = c(as.character(c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450)), ">500"),
    limits = c(0, 500),
    # trans = "log",
    # breaks = scales::breaks_pretty(n = 6),
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(10, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      label.position = "bottom",
      title = bquote("Building area" ~ (m^2))
    )
  ) +
  rcartocolor::scale_color_carto_c(
    palette = "Peach",
    guide = "none"
  ) +
  coord_sf() +
  labs(
    title = "Home and building areas in Québec city",
    caption = "#30daymapchallenge (Home) | Data: https://www.statcan.gc.ca/eng/lode/databases/odb | @philmassicotte"
  ) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    axis.text = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      color = "gray85",
      size = 20,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      color = "white",
      size = 10,
      hjust = 0.5
    ),
    plot.caption = element_text(color = "gray75", size = 8, hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black")
  )

ggsave(
  here::here("graphs", "day30.png"),
  device = "png",
  type = "cairo",
  dpi = 600,
  width = 9,
  height = 9
)
