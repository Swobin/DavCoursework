library(tidyverse)
library(readxl)


issues <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_Issues") %>%
  filter(Issue_UID != "Reminder")
titles <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_TitlesOverview")

dg_ids <- c("DC_0556", "DC_0557", "DC_0558", "DC_0559")
dg_pattern <- paste(dg_ids, collapse = "|")

#Filtering to Dick Grayson only Issues
dg_issues <- issues %>%
  filter(
    str_detect(replace_na(Title_Roles,   ""), dg_pattern) |
    str_detect(replace_na(Support_Roles, ""), dg_pattern)
  )

#adding the codename and the role columns
dg_issues <- dg_issues %>%
  mutate(
    Codename = case_when(
      str_detect(replace_na(Title_Roles,   ""), "DC_0556") ~ "Robin",
      str_detect(replace_na(Title_Roles,   ""), "DC_0557") ~ "Nightwing",
      str_detect(replace_na(Title_Roles,   ""), "DC_0558") ~ "Agent 37",
      str_detect(replace_na(Title_Roles,   ""), "DC_0559") ~ "Batman",
      str_detect(replace_na(Support_Roles, ""), "DC_0556") ~ "Robin",
      str_detect(replace_na(Support_Roles, ""), "DC_0557") ~ "Nightwing",
      str_detect(replace_na(Support_Roles, ""), "DC_0558") ~ "Agent 37",
      str_detect(replace_na(Support_Roles, ""), "DC_0559") ~ "Batman"
    ),
    Role = if_else(
      str_detect(replace_na(Title_Roles, ""), dg_pattern),
      "Lead", "Guest"
    )
  )

#Counting Dick Graysons appearance by series
dg_count <- dg_issues %>%
  count(Issue_UID, Codename, Role, name = "dg_issues")

#Counting the total issues per series
total_count <- issues %>%
  count(Issue_UID, name = "Total_Issues")

#Joining the counts and title names together
dg_summary <- dg_count %>%
  left_join(total_count, by = "Issue_UID") %>%
  left_join(titles, by = c("Issue_UID" = "U_SeriesID")) %>%
  mutate(
    proportion  = dg_issues / Total_Issues,
    Title_Label = paste(Title_Full, "Vol", Volume)
  ) %>%
  filter(!is.na(Title_Full)) %>%
  distinct(Issue_UID, Codename, Role, .keep_all = TRUE)


#Plotting
p <- ggplot(dg_summary, aes(x = reorder(Title_Label, proportion),
                       y = proportion,
                       fill = Codename)) +
  geom_col(position = "stack") +
  facet_wrap(~Role, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(
    title = "Dick Grayson appearances by title",
    x = NULL,
    y = "Proportion of issues",
    fill = "Codename"
  ) +
  theme_minimal()

