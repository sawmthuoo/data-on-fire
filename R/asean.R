# Title: GDP per capita ASEAN
# Date: 2026/10/21

# library
library(tidyverse)
library(janitor)
library(geomtextpath)
library(ggtext)

# ---- ----
df <- read_csv("data/raw/wb_pgdp_20261022.csv",
               skip = 4) |> 
  clean_names()

asean <- c("KHM", "IDN", "LAO", "MYS", "MMR", "THA", "VNM")

df_asean <- df |> 
  select(
    country_name,
    iso3 = country_code,
    x2015:x2024
  ) |> 
  filter(
    iso3 %in% asean
  ) |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "pgdp"
  ) |> 
  mutate(
    year = parse_number(year)
  )

write_csv(df_asean, "data/processed/df_asean.csv")

# read raw data ----

p_basic <- df_asean |> 
  ggplot(aes(year, pgdp, group = iso3, color = country_name)) +
  geom_line()

df_asean <- df_asean |> 
  mutate(
    year = parse_number(year),
    color = if_else(
      iso3 == "MMR", "red", "black"
    )
  )

df_mm <- df_asean |> 
  filter(iso3 == "MMR")

p <- df_asean |> 
  ggplot(aes(year, pgdp, group = iso3)) +
  geom_textline(aes(label = country_name, color = color),
                vjust = -0.2,
                linewidth = 0.6,
                show.legend = FALSE,
                size = 2.5) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  geom_point(
    shape = 21,
    color = "white",
    fill = "black",
    stroke = 0.7,
    size = 1
  ) +
  scale_x_continuous(
    breaks = seq(2015, 2024, by = 1)
  ) +
  scale_color_identity() +
  labs(
    title = "GDP per capita in ASEAN countries (Constant 2025 USD), 2015-2024",
    subtitle = "Singapore and Brunei are excluded. Myanmar’s GDP per capita has consistently been the lowest among ASEAN countries.",
    x = NULL,
    y = "Real GDP per capita (USD)",
    caption = "Data source: World Bank, 2026"
  ) +
  theme_classic(base_size = 9) +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      margin = margin(0, 0, 10, 0, unit = "pt"),
      color = "grey40"
      ),
    plot.caption.position = "plot",
    plot.caption = element_textbox(color = "grey40")
  )

ggsave("export/asean_pgdp.png", p, height = 4.19, width = 6, dpi = 300)
ggsave("export/asean_pgdp_base.png", p_basic, height = 4.19, width = 6, dpi = 300)

  
