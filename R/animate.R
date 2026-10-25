# Annimated graph
# 2026/10/25

# library
library(tidyverse)
library(gganimate)
library(geomtextpath)
library(ggtext)
library(here)

# HDI data preparation for figure ----
df_hdi_raw <- read_csv(here("data", "raw", "undp_hdi_20261026.csv")) #dataset from https://hdr.undp.org/data-center/documentation-and-downloads

df_hdi <- df_hdi_raw |> 
  select(
    iso3, country,
    matches("^hdi_\\d{4}$")
  ) |> 
  filter(
    iso3 %in% c("THA", "LAO", "MMR", "KHM")
  ) |> 
  pivot_longer(
    cols = starts_with("hdi_"),
    names_to = "year",
    names_transform = list(year = ~ str_remove_all(., "hdi_")),
    values_to = "hdi"
  ) |> 
  mutate(
    year = as.numeric(year)
  )

# Draw line graph ----
bz = 8

line <- df_hdi |> 
  mutate(
    country = if_else(
      iso3 == "LAO", "Laos", country
    )
  ) |> 
  ggplot(aes(year, hdi, color = country)) +
  geom_textline(aes(label = country), vjust = -0.2, hjust = 1, show.legend = FALSE, size = 0.8 * bz / .pt) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  theme_bw(base_size = 8) +
  scale_x_continuous(
    breaks = seq(1990, 2023, by = 2),
    expand = expansion(mult = c(0.08, 0.05))
  ) +
  scale_color_brewer(
    palette = "Dark2"
  ) +
  labs(
    title = "Human Development Index, 1990-2023",
    subtitle = "The Human Development Index (HDI) is a composite score (0–1) measuring national progress based on the geometric mean of three dimensions: life expectancy, educational attainment, and GNI per capita (PPP).",
    x = NULL,
    y = "Human Development Index",
    caption = "Data source: UNDP, 2026"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(color = "darkblue"),
    plot.subtitle = element_textbox_simple(
      color = "grey30",
      margin = margin(0, 0, 2, 0, unit = "mm")
    ),
    plot.caption = element_text(color = "grey30"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

animated_line2 <- line + 
  transition_reveal(year)

rendered_line <- animate(
  animated_line2,
  nframes = 100, fps = 10, width = 1600, height = 1200,
  res = 300,
  renderer = gifski_renderer(loop = FALSE)
)

# Save Gif ----
anim_save("export/hdi.gif", rendered_line)
