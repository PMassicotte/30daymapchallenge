library(tidyverse)
library(ggpmthemes)
library(sf)
library(glue)
library(ggtext)
library(httr)
library(readxl)
library(ggforce)

theme_set(theme_exo(base_size = 8))


wifi <-
  st_read(
    "https://www.donneesquebec.ca/recherche/fr/dataset/d9a66edd-a803-47dd-9554-33e8c04970dc/resource/19347b72-7525-4665-961f-159e6a438126/download/zap.json"
  ) %>%
  janitor::clean_names() %>%
  drop_na(nom_lieu)

roads <-
  st_read(
    "https://www.donneesquebec.ca/recherche/fr/dataset/2804b6ac-120f-4d73-98f0-b7bf27dd3017/resource/0be1ba2e-43fe-4b54-815d-50287569313d/download/voirie.json"
  )

places <- wifi %>%
  cbind(st_coordinates(.)) %>%
  as_tibble() %>%
  filter(between(X, -68.56, -68.48) & between(Y, 48.42, 48.48)) %>%
  drop_na(nom_lieu)

# Plot --------------------------------------------------------------------

roads %>%
  ggplot() +
  geom_sf(size = 0.1, color = "gray25") +
  geom_sf(data = wifi, color = "#C91D1D", size = 2) +
  ggrepel::geom_text_repel(
    data = places,
    aes(
      x = X,
      y = Y,
      label = str_wrap(nom_lieu, 20)
    ),
    color = "white",
    size = 2,
    box.padding = unit(0.85, "lines"),
    segment.color = "#C91D1D",
    segment.size = 0.25,
    family = "Exo"
  ) +
  coord_sf(
    xlim = c(-68.56, -68.48),
    ylim = c(48.42, 48.48)
  ) +
  labs(
    title = "Places with free wifi in Rimouski",
    subtitle = str_wrap("Rimouski is a city in Quebec, Canada. Rimouski is located in the Bas-Saint-Laurent region, at the mouth of the Rimouski River. It has a population of 46,860 (as of 2011). Rimouski is the site of Université du Québec à Rimouski, the Cégep de Rimouski (which includes the Institut maritime du Québec) and the Music Conservatory. It is also the home of some ocean sciences research centres. (Source: Wikipedia)", 80),
    caption = "#30daymapchallenge (Places) | Data: https://www.donneesquebec.ca/ | @philmassicotte"
  ) +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    legend.background = element_rect(fill = "#3c3c3c"),
    legend.key = element_blank(),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = 0.5),
    plot.subtitle = element_text(
      color = "white",
      size = 6,
      face = "italic"
    ),
    plot.caption = element_text(
      color = "gray75",
      size = 5,
      hjust = 0
    ),
    axis.title = element_blank()
  )

ggsave(
  here::here("graphs", "day16.png"),
  type = "cairo",
  device = "png",
  dpi = 600,
  width = 5.52,
  height = 4.68
)

