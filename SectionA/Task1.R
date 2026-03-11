#Section A - Task 1: Most Popular Titles
#Gantt chart so you can see both active years and total issues clearly.

#Load the libraries needed
library(tidyverse)
library(readxl)


#Read the excel sheet
titles <- read_excel("CrackersDoMatter_DS.xlsx", sheet = "DC_TitlesOverview")

top_titles <- titles %>%
  mutate(
    Last_PubYr = ifelse(Last_PubYr == "Ongoing", 2026, as.numeric(Last_PubYr)),
    Max_Issues = as.numeric(Max_Issues),
    `#1_pubYr` = as.numeric(`#1_PubYr`)
  ) %>%
  group_by(Title_Full) %>%
  summarise(
    Total_Issues = sum(Max_Issues, na.rm = TRUE),
    Start_Year = min(`#1_PubYr`, na.rm = TRUE),
    End_Year = max(Last_PubYr, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(Total_Issues)) %>%
  slice_head(n = 20) %>%
  #order so highest is at top of the chart
  mutate(Title_Full = fct_reorder(Title_Full, Total_Issues))

# Plotting
ggplot(top_titles) +
  
  geom_segment(
    aes(x = Start_Year, xend = End_Year,
        y = Title_Full, yend = Title_Full),
    colour = "blue", linewidth = 3, alpha = 0.4
  ) + 
  
  geom_text(
    aes(x = End_Year + 1,
        y = Title_Full, 
        label = paste0(Total_Issues, " issues")),
    hjust = 0, size = 3, colour = "black"
  ) +
  
  scale_size_continuous(range = c(2, 10), guide = "none") + 
  scale_x_continuous(
    limits = c(1930, 2040),
    breaks = seq(1930, 2040, by = 10)
  ) + 
  
  labs(
    title = "DC Comics: 20 Most Popular Continuing Titles",
    subtitle = "Ranked by total issues published. Bar shows the active publication years",
    x = "Year",
    y = NULL,
    caption = "Source: Crackers Do Matter DS"
  ) + 
  
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10)
  )

#Save plot
ggsave("task1_top20_titles.png", width = 12, height = 7, dpi = 150)
  