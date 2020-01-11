library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggpmthemes)
library(paletteer)
library(patchwork)
library(ggtext)

theme_set(theme_light_modified(base_family = "Abel"))

files <- fs::dir_ls("data/day04/namesbystate/", recurse = TRUE, glob = "*.TXT")

births <- map_df(
  files,
  data.table::fread,
  col.names = c("state", "sex", "year", "name", "n")
) %>%
  as_tibble()

births <- births %>%
  group_by(state, year) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(state = state.name[match(state, state.abb)])

births %>%
  filter(year %in% seq(1910, 2018, length.out = 4)) %>%
  ggplot(aes(x = year, y = n, color = state)) +
  geom_line(show.legend = F)

us_county <- st_as_sf(maps::map("county",
  plot = FALSE,
  fill = TRUE
)) %>%
  st_transform(crs = 3395)

us_state <-
  st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = 3395)

us_outline <- us_state %>%
  st_simplify() %>%
  st_union()

hex <-
  sf::st_make_grid(us_outline, square = FALSE, cellsize = 150000)

hex %>%
  ggplot() +
  geom_sf() +
  coord_sf(crs = 3395)

df <- hex %>%
  st_as_sf() %>%
  st_join(us_state) %>%
  mutate(ID = str_to_title(ID)) %>%
  left_join(births, by = c("ID" = "state"))

# Plot --------------------------------------------------------------------

make_plot <- function(year, color, fill, df) {
  df %>%
    filter(year == !!year) %>%
    ggplot() +
    geom_sf(size = 0.1, aes(fill = n), color = color) +
    labs(title = year) +
    scale_fill_paletteer_c(
      glue::glue("ggthemes::{fill}"),
      trans = "log10",
      labels = scales::label_number_auto()
      # ,
      # limits = c(1, 600000),
      # oob = scales::squish,
      # direction = -1
    ) +
    theme(
      plot.title = element_text(
        family = "Changa One",
        color = "white",
        hjust = 0.5,
        size = 28
      ),
      plot.subtitle = element_text(family = "Abel", color = "white"),
      plot.background = element_rect(fill = "#2B2C30", color = "#2B2C30"),
      panel.background = element_rect(fill = "#2B2C30", color = "#2B2C30"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(color = "white"),
      legend.key = element_rect(colour = NA),
      legend.box.background = element_blank()
    ) +
    guides(
      fill = guide_colorbar(
        barheight = unit(2.5, units = "mm"),
        barwidth = unit(120, units = "mm"),
        direction = "horizontal",
        ticks.colour = "#3c3c3c",
        title.position = "top",
        title.hjust = 0.5,
        label.theme = element_text(color = "white"),
        title.theme = element_text(color = "white"),
        title = "Number of births"
      )
    )
}


p <- pmap(list(
  seq(1910, 2018, length.out = 4),
  c(
    "blue",
    "green",
    "red",
    "orange"
  ),
  c(
    "Blue",
    "Green",
    "Red",
    "Orange"
  )
), make_plot, df = df)

p2 <- patchwork::wrap_plots(p, ncol = 2) +
  patchwork::plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "#2B2C30", color = "#2B2C30"),
      panel.border = element_rect(fill = "#2B2C30", color = "#2B2C30"),
      panel.background = element_rect(fill = "#2B2C30", color = "#2B2C30"),
      panel.grid = element_blank(),
      plot.title = element_markdown(
        family = "Changa One",
        color = "white",
        hjust = 0.5,
        size = 32,
        margin = margin(20, 2, 50, 2)
      ),
      plot.caption = element_text(color = "grey75", family = "Abel", size = 14)
    ),
    title = "US population in <i style='color:#8ebdddff'>1910</i>, <i style='color:#68b05dff'>1946</i>, <i style='color:#ae123aff'>1982</i> and <i style='color:#e86a20ff'>2018</i>",
    caption = "#30daymapchallenge (#4, #6, #7, #8, #9) | Data: https://www.ssa.gov/OACT/babynames/limits.html | @philmassicotte"
  )

pdf_file <- here::here("graphs/", "day04_06_07_08_09.pdf")
pdf_png <- here::here("graphs/", "day04_06_07_08_09.png")

ggsave(
  pdf_file,
  device = cairo_pdf,
  plot = p2,
  width = 10,
  height = 10
)

knitr::plot_crop(pdf_file)

bitmap <- pdftools::pdf_render_page(pdf_file, dpi = 600)
png::writePNG(bitmap, pdf_png)
