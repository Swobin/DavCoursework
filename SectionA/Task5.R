#Needed libraries
library(tidyverse)
library(readxl)
#Loading data and Drop reminder rows with slices
issues <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_Issues") |>
  slice(-1) |>
  mutate(Pub_Year = as.integer(Pub_Year))
titles <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_TitlesOverview")
#Artist IDs
artist_ids <- c(
  "SA_1269" = "George Perez",
  "SA_1212" = "Tony (TS) Daniel",
  "SA_1305" = "Lee Bermejo"
)
#Lookup titles
title_lookup <- titles |>
  select(U_SeriesID, Title_Full, Volume) |>
  mutate(
    Title_Label = if_else(Volume > 1,
                          paste0(Title_Full, " (Vol.", Volume, ")"),
                          Title_Full)
  )
#A Covers
a_covers <- issues |>
  filter(CvrA_Artist %in% names(artist_ids)) |>
  select(Issue_UID, Pub_Year, artist_id = CvrA_Artist) |>
  mutate(cover_type = "A Cover")
#Variant Covers
var_covers <- issues |>
  filter(!is.na(CvrVarArtists)) |>
  select(Issue_UID, Pub_Year, CvrVarArtists) |>
  separate_rows(CvrVarArtists, sep = ";\\s*") |>
  mutate(
    # Extract artist ID
    artist_id  = str_extract(CvrVarArtists, "SA_\\d+"),
    code_part  = str_split_fixed(CvrVarArtists, "_SA_", 2)[, 1],
    print_run  = as.integer(str_extract(code_part, "\\d+$")),
    cover_type = if_else(is.na(print_run) | print_run == 1,
                         "1st Print",
                         "Reprint")
  ) |>
  filter(artist_id %in% names(artist_ids)) |>
  select(Issue_UID, Pub_Year, artist_id, cover_type)
#Combining and adding titles
all_covers <- bind_rows(a_covers, var_covers) |>
  mutate(U_SeriesID = str_extract(Issue_UID, "^[A-Z*]+_\\d+")) |>
  left_join(title_lookup, by = "U_SeriesID") |>
  mutate(
    Artist      = artist_ids[artist_id],
    Title_Label = if_else(is.na(Title_Label), U_SeriesID, Title_Label)
  ) |>
  filter(!is.na(Pub_Year))
#Summarising
heat_data <- all_covers |>
  count(Artist, Title_Label, Pub_Year, cover_type)
#Set a consistent cover type order
heat_data <- heat_data |>
  mutate(cover_type = factor(cover_type,
                             levels = c("A Cover",
                                        "1st Print",
                                        "Reprint")))
#Heatmap
p5 <- ggplot(heat_data, aes(x = Pub_Year, y = Title_Label, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  facet_grid(cover_type ~ Artist, scales = "free", space = "free") +
  scale_fill_gradient(low      = "lightblue",
                      high     = "darkblue",
                      name     = "Covers",
                      na.value = "lightblue") +
  scale_x_continuous(breaks = seq(1975, 2030, by = 5)) +
  labs(
    title    = "Cover Artist Career Paths: Perez, Daniel & Bermejo",
    subtitle = "Number of covers per title per year – split by cover type",
    x        = "Publication Year",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    strip.text       = element_text(face = "bold"),
    strip.text.y     = element_text(face = "bold", angle = 0),
    panel.grid       = element_blank(),
    legend.position  = "bottom",
    plot.margin      = margin(t = 5, r = 40, b = 5, l = 120)
  )