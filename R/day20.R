library(tidyverse)
library(ggpmthemes)
library(sf)
library(ggtext)

theme_set(theme_light_modified(base_family = "Michroma"))

# Proportion urban/rural area USA
# https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural/2010-urban-rural.html
#

f <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = f)
f <- unzip(f, exdir = tempdir())
f <- f[grepl(".shp$", f)]

usa <- st_read(f) %>%
  janitor::clean_names() %>%
  mutate_if(is.factor, as.character) %>%
  filter(!state %in% c("02","15","72"))

f <- tempfile()
curl::curl_download("https://www2.census.gov/geo/docs/reference/ua/PctUrbanRural_County.xls", destfile = f)
rural <- readxl::read_excel(f) %>%
  janitor::clean_names()

# Merge -------------------------------------------------------------------

usa <- usa %>%
  left_join(rural, by = c("state", "county"))

usa_outline_state <- usa %>%
  group_by(state) %>%
  summarise(average = mean(poppct_rural))

usa_outline <- usa %>%
  st_union()

# Plot --------------------------------------------------------------------

cols <- c("TRUE" = "#F1C78A", "FALSE" = "#E6E7D5")

p <- usa %>%
  mutate(is_mostly_rural = ifelse(poppct_rural > 50, TRUE, FALSE)) %>%
  ggplot() +
  geom_sf(aes(fill = is_mostly_rural), size = 0.1, color = "gray75") +
  geom_sf(data = usa_outline_state, fill = NA, color = "#3c3c3c", size = 0.2, show.legend = FALSE) +
  coord_sf(crs = "+init=epsg:2163") +
  scale_fill_manual(
    values = cols,
    breaks = c("TRUE", "FALSE"),
    labels = c("Mostly rural", "Mostly urban")
  ) +
  labs(
    title = "Rural counties in USA",
    subtitle = "US counties have been classified as **rural** if the percentage of the<br>population living in rural cities was greater than 50 percent.",
    caption = "#30daymapchallenge (Rural) | Data: https://www2.census.gov/ | @philmassicotte"
  ) +
  theme(
    legend.justification = c(0, 0),
    legend.position = c(0.05, 0.05),
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(color = "gray75", hjust = 0.5, size = 16),
    plot.subtitle = element_markdown(color = "gray75", size = 8, hjust = 0.5),
    plot.caption = element_text(color = "gray75", size = 6, hjust = 0.5, family = "Open Sans"),
    legend.key = element_rect(size = 6, colour = NA),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 6)
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      label.position = "top",
      keyheight = unit(2, "mm"),
      color = NA
    )
  )
# +
#   geom_sf(data = usa_outline, fill = NA, color = "gray75", size = 0.5)

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "day20.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 5.52 * 1.15,
  height = 4.68 * 1.15
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "day20.png")
png::writePNG(bitmap, destfile)
