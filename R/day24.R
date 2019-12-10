library(tidyverse)
library(ggpmthemes)
library(sf)
library(glue)
library(rvest)
library(patchwork)

theme_set(theme_light_modified(base_family = "Michroma", base_size = 16))

# The geography of maple syrup

html <-
  read_html(
    "https://www.worldatlas.com/articles/the-world-s-top-producers-of-maple-syrup.html"
  )

maple_harvested <- html %>%
  html_table() %>%
  do.call(as_tibble, .) %>%
  janitor::clean_names() %>%
  mutate(average_annual_harvest_gallons = parse_number(average_annual_harvest_gallons)) %>%
  rename(region = x_u_feff_region) %>%
  separate(region, into = c("region", "country"), sep = ",") %>%
  mutate_if(is.character, .funs = list(str_trim)) %>%
  arrange(desc(average_annual_harvest_gallons)) %>%
  mutate(color = colorRampPalette(c("#663000", "#F2DACE"))(12))

canada <- st_read("data/day24/Canada/Canada.shp") %>%
  janitor::clean_names() %>%
  rename(region = name) %>%
  left_join(maple_harvested, by = "region") %>%
  st_transform(crs = 4326) %>%
  select(region, average_annual_harvest_gallons, color, geometry) %>%
  mutate(country = "Canada")

canada %>%
  ggplot(aes(fill = factor(average_annual_harvest_gallons))) +
  geom_sf()

usa <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  rename(region = ID) %>%
  mutate(region = str_to_title(region)) %>%
  left_join(maple_harvested, by = "region") %>%
  st_transform(crs = 4326) %>%
  select(region, average_annual_harvest_gallons, color, geometry) %>%
  mutate(country = "USA")

usa %>%
  ggplot(aes(fill = factor(average_annual_harvest_gallons))) +
  geom_sf()

df <- rbind(canada, usa)

p1 <- canada %>%
  # mutate(color = fct_reorder(color, average_annual_harvest_gallons)) %>%
  ggplot(aes(fill = color)) +
  geom_sf(size = 0.1, color = "#996136")  +
  coord_sf(crs = "+init=epsg:2163") +
  labs(
    fill = "Average annual\nharvest gallons",
    title = "Harvested gallons in Canada"
  ) +
  scale_fill_identity(
    guide = "legend",
    na.translate = FALSE,
    breaks = maple_harvested$color,
    labels =
      glue("{maple_harvested$region} ({scales::comma(maple_harvested$average_annual_harvest_gallons)})")
  ) +
  theme(
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      color = "#996136",
      hjust = 0.5,
      size = 20,
      face = "bold",
      family = "Maven Pro"
    ),
    plot.caption = element_text(
      color = "gray50",
      size = 6,
      hjust = 0.5
    ),
    legend.key = element_rect(size = 6, colour = NA),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10)
  ) +
  guides(fill = guide_legend(
    byrow = TRUE,
    keyheight = unit(2, "mm"),
    color = NA,
    override.aes = list(linetype = "blank", shape = NA)
  ))

p2 <- usa %>%
  # mutate(color = fct_reorder(color, average_annual_harvest_gallons)) %>%
  ggplot(aes(fill = color)) +
  geom_sf(size = 0.1, color = "#996136")  +
  coord_sf(crs = "+init=epsg:2163") +
  labs(
    fill = "Average annual\nharvest gallons",
    title = "Harversted gallons in USA"
  ) +
  scale_fill_identity(
    guide = "legend",
    na.translate = FALSE,
    breaks = maple_harvested$color,
    labels =
      glue("{maple_harvested$region} ({scales::comma(maple_harvested$average_annual_harvest_gallons)})")
  ) +
  theme(
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      color = "#996136",
      hjust = 0.5,
      size = 20,
      face = "bold",
      family = "Maven Pro"
    ),
    plot.caption = element_text(
      color = "gray50",
      size = 6,
      hjust = 0.5
    ),
    legend.key = element_rect(size = 6, colour = NA),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10)
  ) +
  guides(fill = guide_legend(
    byrow = TRUE,
    keyheight = unit(2, "mm"),
    color = NA,
    override.aes = list(linetype = "blank", shape = NA)
  ))


p1 / p2 +
  plot_annotation(
    caption = "#30daymapchallenge (Statistics) | Data: https://www.worldatlas.com/ | @philmassicotte",
    theme = theme(plot.caption = element_text(
      color = "gray50",
      size = 6,
      hjust = 0.5
    ))

)

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day24.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 5.52 * 1.3,
  height = 4.68 * 1.15
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day24.png")
png::writePNG(bitmap, destfile)
