library(tidyverse)
library(ggpmthemes)
library(sf)
library(ggtext)

theme_set(theme_exo2())

# https://www150.statcan.gc.ca/n1/pub/16-201-x/2017000/sec-1/m-c/m-c-1.1.zip

df <-
  st_read("data/day17/m-c-1.1/Drainage_regions_Regions_de_drainage.shp") %>%
  janitor::clean_names() %>%
  st_simplify(dTolerance = 0.1)

df <- df %>%
  mutate(dr_name = str_replace_all(dr_name, "\\u0096", "/")) %>%
  group_by(nom_ado) %>%
  mutate(
    color = case_when(
      nom_ado == "Océan Pacifique" ~ colorRampPalette(c("#FFCDCD", "#DB0202"))(n()),
      nom_ado == "Océan Arctique" ~ colorRampPalette(c("#FFD400", "#FFEA61"))(n()),
      nom_ado == "Golfe du Mexique" ~ colorRampPalette(c("#AD466C", "#AD466C"))(n()),
      nom_ado == "Baie d'Hudson" ~ colorRampPalette(c("#9ED1EB", "#025CE7"))(n()),
      nom_ado == "Océan Atlantique" ~ colorRampPalette(c("#DFF5E9", "#236733"))(n())
    )
  )

# %>%
#   # ungroup() %>%
#   mutate(color = factor(color, levels = sort(nom_rd)))

# Plot --------------------------------------------------------------------

df %>%
  ggplot() +
  geom_sf(aes(fill = color), size = 0.1, color = "#333333") +
  coord_sf(crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45") +
  scale_fill_identity(
    guide = "legend",
    breaks = df$color,
    labels = df$dr_name
  ) +
  labs(
    title = "Drainage zones of Canada",
    subtitle = "There are five main watershed zones in Canada: <i style='color:#DB0202'>Pacific Ocean</i>, <i style='color:#FFD400'>Arctic Ocean</i>, <i style='color:#AD466C'>Gulf of Mexico</i>, <i style='color:#2879E8'>Hudson Bay</i><br>and <i style='color:#427E51'>Atlantic Ocean</i>",
    caption = "#30daymapchallenge (Zones) | Data: https://www150.statcan.gc.ca/ | @philmassicotte"
  ) +
  theme(
    legend.position = "right",
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    # legend.key.width = unit(1, "cm"),
    plot.background = element_rect(fill = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    legend.background = element_rect(fill = "#333333"),
    legend.key = element_blank(),
    legend.text = element_text(color = "white", size = 8),
    legend.title = element_blank(),
    plot.title = element_text(color = "gray75"),
    plot.subtitle = element_markdown(
      color = "gray75",
      size = 10
    ),
    plot.caption = element_text(
      color = "gray75",
      size = 8,
      hjust = 0
    ),
    axis.title = element_blank()
  ) +
  guides(
    fill = guide_legend(
      keywidth = unit(1, "cm"),
      keyheight = unit(0.2, "cm"),
      ncol = 1
    )
  )


ggsave(
  here::here("graphs", "day17.png"),
  type = "cairo",
  device = "png",
  dpi = 600,
  width = 7,
  height = 6
)
