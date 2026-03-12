# Needed libraries
library(tidyverse)
library(readxl)
library(plotly)

# Loading the data
issues <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_Issues", skip = 1)
artists <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "Shared_CoverArtist")
titles <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_TitlesOverview")

# Building clean artist names
artist_names <- artists %>%
  mutate(Artist = paste(Name_1st, Name_Last)) %>%
  select(UID, Artist)

# Build title lookup with full name and volume
title_lookup <- titles %>%
  mutate(Title_Label = paste(Title_Full, "Vol.", Volume)) %>%
  select(U_SeriesID, Title_Label)

# Rename by column position
issues_clean <- issues %>%
  rename(
    Issue_UID     = 1,
    Cover_A       = 14,
    Cover_Variant = 18
  ) %>%
  select(Issue_UID, Cover_A, Cover_Variant)

# Pivoting both cover columns into one
covers_long <- issues_clean %>%
  pivot_longer(
    cols      = c(Cover_A, Cover_Variant),
    names_to  = "Cover_Type",
    values_to = "Artist_UID"
  ) %>%
  filter(!is.na(Artist_UID)) %>%
  filter(str_starts(Artist_UID, "SA_"))

# Finding the top 10 most prolific cover artists
top10_artists <- covers_long %>%
  group_by(Artist_UID) %>%
  summarise(Total = n(), .groups = "drop") %>%
  slice_max(Total, n = 10) %>%
  pull(Artist_UID)

# Summarising by artist and series joining full title names
cover_data <- covers_long %>%
  filter(Artist_UID %in% top10_artists) %>%
  group_by(Artist_UID, Issue_UID) %>%
  summarise(Issues = n(), .groups = "drop") %>%
  left_join(artist_names, by = c("Artist_UID" = "UID")) %>%
  left_join(title_lookup, by = c("Issue_UID" = "U_SeriesID")) %>%
  filter(!is.na(Title_Label)) %>%
  rename(Title = Title_Label) %>%
  mutate(Artist = fct_reorder(Artist, Issues, sum))

# Plotting the chart
p4 <- ggplot(cover_data, aes(x = Title, y = Artist, fill = Issues)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "10 Most Prolific Cover Artists by Title",
    x     = "Title",
    y     = NULL,
    fill  = "Issues"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 6),
    panel.grid.minor = element_blank()
  )

ggplotly(p4)