#Necessary Libraries
library(tidyverse)
library(readxl)
library(plotly)

#Loading the data we need
issues <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_Issues", skip = 1)
writers <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "Shared_LeadWriter")

# Get the two target writers we are looking for and building a clean name column
target_ids <- c("SW_2000", "SW_2004")

writer_names <- writers %>%
  filter(UID %in% target_ids) %>%
  mutate(Writer = paste(Name_1st, Name_Last)) %>%
  select(UID, Writer)

#Extracting the title prefix from the Reminder column by removing _number and cleaning up the year column
issues_clean <- issues %>%
  rename(
    Issue_UID = Reminder,
    Pub_Year  = `# x 4`,
    Write_Lead = FK_WriterTbl
  ) %>%
  mutate(
    Title    = sub("_.*", "", Issue_UID),
    Pub_Year = as.integer(Pub_Year)
  )

#Filter to just the two writers that we want and summarise it by writer and year
career_data <- issues_clean %>%
  filter(Write_Lead %in% target_ids) %>%
  filter(!is.na(Pub_Year)) %>%
  group_by(Write_Lead, Pub_Year) %>%
  summarise(
    Issues = n(),
    Titles = n_distinct(Title),
    .groups = "drop"
  ) %>%
  left_join(writer_names, by = c("Write_Lead" = "UID"))

#Building a full year range so neither of the writers has gaps
min_year <- career_data %>% pull(Pub_Year) %>% min()
max_year <- career_data %>% pull(Pub_Year) %>% max()

all_combinations <- expand_grid(
  UID      = target_ids,
  Pub_Year = min_year:max_year
) %>%
  left_join(writer_names, by = "UID")

#Join the career data onto the full grid, filling the missing years with a zero
career_full <- all_combinations %>%
  left_join(
    career_data %>% select(Write_Lead, Pub_Year, Issues, Titles),
    by = c("UID" = "Write_Lead", "Pub_Year" = "Pub_Year")
  ) %>%
  mutate(
    Issues = replace_na(Issues, 0),
    Titles = replace_na(Titles, 0)
  )

#changed it to long format for faceting
career_long <- career_full %>%
  pivot_longer(
    cols      = c(Issues, Titles),
    names_to  = "Metric",
    values_to = "Count"
  )

#Plotting the chart
p <- ggplot(career_long, aes(x = Pub_Year, y = Count, fill = Writer)) +
  geom_area(alpha = 0.6, position = "identity") +
  facet_wrap(
    ~Metric,
    ncol     = 1,
    scales   = "free_y",
    labeller = labeller(Metric = c(Issues = "Issues Published",
                                   Titles = "Distinct Titles"))
  ) +
  scale_fill_manual(
    values = c("Chuck Dixon" = "red", "Marv Wolfman" = "blue")
  ) +
  labs(
    title  = "Career Comparison: Chuck Dixon vs Marv Wolfman",
    x      = "Year",
    y      = NULL,
    fill   = "Writer"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

#Making the plot interactive
ggplotly(p)