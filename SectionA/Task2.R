#Load the necessary libraries.
library(tidyverse)
library(readxl)
library(ggplot2)

#Reading the excel sheets
issues  <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_Issues")
writers <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "Shared_LeadWriter")
titles  <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_TitlesOverview")

#Removing the placeholder rows where the field is "b"
issues_clean <- issues %>%
  filter(Write_Lead != "b", !is.na(Write_Lead), !is.na(Issue_UID))

#Build a direct lookup from U_SeriesID to Title_Full with no stripping
title_lookup <- titles %>%
  filter(!is.na(U_SeriesID)) %>%
  distinct(U_SeriesID, .keep_all = TRUE) %>%
  select(U_SeriesID, Title_Full)

#Added manual entries for codes that are missing from the DC_TitlesOverview
manual_lookup <- tibble(
  U_SeriesID = c("JKR",   "JLD",                 "SSS",        "BLT_BKG",
                 "DCM_BWS","ELS_BVP",             "KO_RND",     "XO_YV_SP",
                 "SP_RB"),
  Title_Full = c("Joker", "Justice League Dark", "Secret Six", "Batman: Blackgate",
                 "DC/Marvel: Batman/Wolverine", "Elseworlds: Batman vs Poison Ivy",
                 "KO: Round", "Year of the Villain: Special", "Superman/Robin")
)

title_lookup <- bind_rows(title_lookup, manual_lookup)

#For each Issue_UID, try matching at progressively shorter codes until a match is found
get_title <- function(uid, lookup_df) {
  lookup <- setNames(lookup_df$Title_Full, lookup_df$U_SeriesID)
  parts <- strsplit(uid, "_")[[1]]
  for (i in length(parts):1) {
    candidate <- paste(parts[1:i], collapse = "_")
    if (candidate %in% names(lookup)) return(lookup[[candidate]])
    v1 <- paste0(candidate, "_1")
    if (v1 %in% names(lookup)) return(lookup[[v1]])
  }
  return(uid)
}

#Applies the title lookup to every issue
issues_clean <- issues_clean %>%
  mutate(Title_Full = map_chr(Issue_UID, get_title, lookup_df = title_lookup))

#Joining the writers names
writers_clean <- writers %>%
  mutate(Writer_Name = paste(Name_1st, Name_Last)) %>%
  select(UID, Writer_Name)

issues_named <- issues_clean %>%
  left_join(writers_clean, by = c("Write_Lead" = "UID"))

#Finding out the top 10 writers by the total issues.
top10_ids <- issues_named %>%
  group_by(Write_Lead, Writer_Name) %>%
  summarise(Total_Issues = n(), .groups = "drop") %>%
  arrange(desc(Total_Issues)) %>%
  slice_head(n = 10) %>%
  pull(Write_Lead)

#Counting the issues per writer per title, filtering to top 10.
writer_title_counts <- issues_named %>%
  filter(Write_Lead %in% top10_ids) %>%
  group_by(Write_Lead, Writer_Name, Title_Full) %>%
  summarise(Issues = n(), .groups = "drop")

#Orders the writers by total issues on Y.
writer_order <- writer_title_counts %>%
  group_by(Writer_Name) %>%
  summarise(Total = sum(Issues)) %>%
  arrange(desc(Total)) %>%
  pull(Writer_Name)

#Orders the titles by total issues on X.
title_order <- writer_title_counts %>%
  group_by(Title_Full) %>%
  summarise(Total = sum(Issues)) %>%
  arrange(desc(Total)) %>%
  pull(Title_Full)

writer_title_counts <- writer_title_counts %>%
  mutate(
    Writer_Name = factor(Writer_Name, levels = rev(writer_order)),
    Title_Full  = factor(Title_Full,  levels = title_order)
  )

#heatmap plotting
ggplot(writer_title_counts,
       aes(x = Title_Full, y = Writer_Name, fill = Issues)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(
    aes(label = ifelse(Issues >= 5, as.character(Issues), "")),
    size = 2.8, colour = "white", fontface = "bold"
  ) +
  scale_fill_gradient(low = "lightblue", high = "blue", name = "Issues") +
  labs(
    title    = "DC Comics: 10 Most Prolific Writers",
    subtitle = "Number of issues credited per title — darker = more issues",
    x        = NULL,
    y        = NULL,
    caption  = "Source: Crackers Do Matter dataset"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey", size = 10),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y      = element_text(size = 10),
    panel.grid       = element_blank(),
    legend.position  = "right"
  )

#Save
ggsave("task2_prolific_writers.png", width = 16, height = 7, dpi = 150)