library(tidyverse)
library(readxl)

#Loading the sheets
issues <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_Issues") %>%
  slice(-1) %>%
  filter(Issue_UID != "b") %>%
  mutate(Pub_Year = as.numeric(Pub_Year))

arcs <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_StoryArc")


#Counting the issues per arc. Doing join to get the arc names.
arc_counts <- issues %>%
  filter(!Arc_Links %in% c("DCA_0000", "b"), !is.na(Arc_Links)) %>%
  group_by(Arc_Links) %>%
  summarise(issue_count = n(), .groups = "drop") %>%
  left_join(arcs %>% select(UID, Arc_Title), by = c("Arc_Links" = "UID")) %>%
  filter(!is.na(Arc_Title), issue_count >= 8) %>%
  arrange(desc(issue_count))


#Plotting
ggplot(arc_counts, aes(x = reorder(Arc_Title, issue_count), y = issue_count)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Biggest DC Story Arcs by Issues Published",
    x = NULL,
    y = "Number of Issues"
  ) +
  theme_minimal()