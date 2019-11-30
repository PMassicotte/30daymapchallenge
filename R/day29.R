library(tidyverse)
library(rvest)
library(ggpmthemes)
library(sf)

theme_set(theme_light_modified(base_family = "Exo", base_size = 16))

url <- "https://www.sportsnet.ca/hockey/nhl/nhl-2019-20-season-montreal-canadiens-schedule/"

games <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  magrittr::extract2(1) %>%
  html_table(header = TRUE, fill = TRUE) %>%
  janitor::clean_names()

games <- games %>%
  add_column(game_id = 1:nrow(.), .before = 1) %>%
  as_tibble() %>%
  select(game_id, visitor, home)

arena <- jsonlite::fromJSON("https://raw.githubusercontent.com/nhlscorebot/arenas/master/teams.json")

arena <- arena %>%
  enframe() %>%
  unnest_wider(value) %>%
  mutate(location = str_match(name, "\\S+")[, 1]) %>%
  mutate(location = case_when(
    str_detect(name, "Los Angeles") ~ "Los Angeles",
    str_detect(name, "New Jersey") ~ "New Jersey",
    str_detect(name, "Islander") ~ "N.Y. Islanders",
    str_detect(name, "Rangers") ~ "N.Y. Rangers",
    str_detect(name, "San Jose") ~ "San Jose",
    str_detect(name, "St. Louis") ~ "St. Louis",
    str_detect(name, "Tampa Bay") ~ "Tampa Bay",
    TRUE ~ location
  ))


# Add missing arenas
arena <- arena %>%
  add_row(
    name = "Arizona Coyotes",
    arena = "Gila River Arena",
    lat = 33.531944,
    long = -112.261111,
    location = "Arizona"
  ) %>%
  add_row(
    name = "Vegas Golden Knights",
    arena = "T-Mobile Arena",
    lat = 36.102778,
    long = -115.178333,
    location = "Vegas"
  ) %>%
  select(lat, long, location)

df <- games %>%
  left_join(arena, by = c("visitor" = "location")) %>%
  rename_at(vars(lat, long), ~ paste0("visitor_", .)) %>%
  left_join(arena, by = c("home" = "location")) %>%
  rename_at(vars(lat, long), ~ paste0("home_", .)) %>%
  mutate(direction = ifelse(home == "Montreal", "local", "away"))

ws <- st_read(
  "data/day26/Watersheds_Shapefile/NA_Watersheds/data/NA_Watersheds/watershed_p_v2.shp"
) %>%
  filter(COUNTRY %in% c("CAN", "USA"))

proj <- st_crs(ws)

ws <- ws %>%
  st_transform(crs = 4326)

# %>%
#   st_crop(
#     xmin = -130,
#     xmax = 0,
#     ymin = 25,
#     ymax = 60
#   )

# Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_sf(data = ws, size = 0.1, fill = "#3c3c3c") +
  geom_point(
    data = df,
    aes(
      x = visitor_long,
      y = visitor_lat
    ),
    color = "white"
  ) +
  ggrepel::geom_text_repel(
    data = arena,
    aes(x = long, y = lat, label = location),
    color = "white",
    size = 2
  ) +
  geom_curve(
    data = df,
    curvature = 0.25,
    size = 0.25,
    alpha = 0.5,
    aes(
      x = visitor_long,
      y = visitor_lat,
      xend = home_long,
      yend = home_lat,
      color = direction
    ),
    arrow = arrow(length = unit(0.01, "npc"))
  ) +
  coord_sf(xlim = c(-130, -50), ylim = c(25, 60)) +
  labs(
    title = "The routes of Montreal Canadiens",
    subtitle = str_wrap("The NHL Montreal Canadiens will play a total of 82 games in 32 differents arenas", 100),
    caption = "#30daymapchallenge (Experimental) | dataviz @philmassicotte"
  ) +
  paletteer::scale_color_paletteer_d(ggthemes, wsj_rgby) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    axis.text = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "gray75", size = 20, hjust = 0.5),
    plot.subtitle = element_text(color = "white", size = 10, hjust = 0.5),
    plot.caption = element_text(color = "gray75", size = 8),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black")
  )

ggsave(
  here::here("graphs", "day29.png"),
  device = "png",
  type = "cairo",
  dpi = 600,
  width = 5.52 * 1.15,
  height = 4.68 * 1.15
)
