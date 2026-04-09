library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(scales)

DATA_PATH <- "CrackersDoMatter_DS.xlsx"

# Load all data sheets
titles   <- read_excel(DATA_PATH, sheet = "DC_TitlesOverview")
issues   <- read_excel(DATA_PATH, sheet = "DC_Issues") %>%
  filter(Issue_UID != "Reminder", !is.na(Issue_UID))
issues_skip <- read_excel(DATA_PATH, sheet = "DC_Issues", skip = 1)
writers  <- read_excel(DATA_PATH, sheet = "Shared_LeadWriter")
artists  <- read_excel(DATA_PATH, sheet = "Shared_CoverArtist")
arcs     <- read_excel(DATA_PATH, sheet = "DC_StoryArc")

# T1_A
a1_data <- titles %>%
  mutate(
    Last_PubYr = ifelse(Last_PubYr == "Ongoing", 2026, as.numeric(Last_PubYr)),
    Max_Issues = as.numeric(Max_Issues),
    `#1_PubYr` = as.numeric(`#1_PubYr`)
  ) %>%
  group_by(Title_Full) %>%
  summarise(
    Total_Issues = sum(Max_Issues, na.rm = TRUE),
    Start_Year   = min(`#1_PubYr`, na.rm = TRUE),
    End_Year     = max(Last_PubYr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Issues)) %>%
  slice_head(n = 20) %>%
  mutate(Title_Full = fct_reorder(Title_Full, Total_Issues))

# T2_A
issues_clean_a2 <- issues %>%
  filter(Write_Lead != "b", !is.na(Write_Lead))

title_lookup_a2 <- titles %>%
  filter(!is.na(U_SeriesID)) %>%
  distinct(U_SeriesID, .keep_all = TRUE) %>%
  select(U_SeriesID, Title_Full)

manual_lookup <- tibble(
  U_SeriesID = c("JKR","JLD","SSS","BLT_BKG","DCM_BWS","ELS_BVP","KO_RND","XO_YV_SP","SP_RB"),
  Title_Full = c(
    "Joker","Justice League Dark","Secret Six","Batman: Blackgate",
    "DC/Marvel: Batman/Wolverine","Elseworlds: Batman vs Poison Ivy",
    "KO: Round","Year of the Villain: Special","Superman/Robin"
  )
)

title_lookup_a2 <- bind_rows(title_lookup_a2, manual_lookup)

get_title <- function(uid, lookup_df) {
  lookup <- setNames(lookup_df$Title_Full, lookup_df$U_SeriesID)
  parts  <- strsplit(uid, "_")[[1]]
  
  for (i in length(parts):1) {
    candidate <- paste(parts[1:i], collapse = "_")
    if (candidate %in% names(lookup)) return(lookup[[candidate]])
    v1 <- paste0(candidate, "_1")
    if (v1 %in% names(lookup)) return(lookup[[v1]])
  }
  
  return(uid)
}

issues_clean_a2 <- issues_clean_a2 %>%
  mutate(Title_Full = map_chr(Issue_UID, get_title, lookup_df = title_lookup_a2))

writers_clean <- writers %>%
  mutate(Writer_Name = paste(Name_1st, Name_Last)) %>%
  select(UID, Writer_Name)

issues_named_a2 <- issues_clean_a2 %>%
  left_join(writers_clean, by = c("Write_Lead" = "UID"))

top10_writer_ids <- issues_named_a2 %>%
  group_by(Write_Lead, Writer_Name) %>%
  summarise(Total_Issues = n(), .groups = "drop") %>%
  arrange(desc(Total_Issues)) %>%
  slice_head(n = 10) %>%
  pull(Write_Lead)

a2_data <- issues_named_a2 %>%
  filter(Write_Lead %in% top10_writer_ids) %>%
  group_by(Write_Lead, Writer_Name, Title_Full) %>%
  summarise(Issues = n(), .groups = "drop")

writer_order <- a2_data %>%
  group_by(Writer_Name) %>%
  summarise(Total = sum(Issues), .groups = "drop") %>%
  arrange(desc(Total)) %>%
  pull(Writer_Name)

title_order_a2 <- a2_data %>%
  group_by(Title_Full) %>%
  summarise(Total = sum(Issues), .groups = "drop") %>%
  arrange(desc(Total)) %>%
  pull(Title_Full)

a2_data <- a2_data %>%
  mutate(
    Writer_Name = factor(Writer_Name, levels = rev(writer_order)),
    Title_Full  = factor(Title_Full, levels = title_order_a2)
  )

# T3_A
target_ids <- c("SW_2000", "SW_2004")

writer_names <- writers %>%
  filter(UID %in% target_ids) %>%
  mutate(Writer = paste(Name_1st, Name_Last)) %>%
  select(UID, Writer)

issues_a3 <- issues_skip %>%
  rename(Issue_UID = 1, Pub_Year = `# x 4`, Write_Lead = FK_WriterTbl) %>%
  mutate(
    Title = sub("_.*", "", Issue_UID),
    Pub_Year = as.integer(Pub_Year)
  )

career_data <- issues_a3 %>%
  filter(Write_Lead %in% target_ids, !is.na(Pub_Year)) %>%
  group_by(Write_Lead, Pub_Year) %>%
  summarise(
    Issues = n(),
    Titles = n_distinct(Title),
    .groups = "drop"
  ) %>%
  left_join(writer_names, by = c("Write_Lead" = "UID"))

yr_range <- range(career_data$Pub_Year, na.rm = TRUE)

a3_data <- expand_grid(UID = target_ids, Pub_Year = yr_range[1]:yr_range[2]) %>%
  left_join(writer_names, by = "UID") %>%
  left_join(
    career_data %>% select(Write_Lead, Pub_Year, Issues, Titles),
    by = c("UID" = "Write_Lead", "Pub_Year")
  ) %>%
  mutate(
    Issues = replace_na(Issues, 0),
    Titles = replace_na(Titles, 0)
  ) %>%
  pivot_longer(c(Issues, Titles), names_to = "Metric", values_to = "Count")

# T4_A
artist_names <- artists %>%
  mutate(Artist = paste(Name_1st, Name_Last)) %>%
  select(UID, Artist)

title_lookup_a4 <- titles %>%
  mutate(Title_Label = paste(Title_Full, "Vol.", Volume)) %>%
  select(U_SeriesID, Title_Label)

issues_a4 <- issues_skip %>%
  rename(Issue_UID = 1, Cover_A = 14, Cover_Variant = 18) %>%
  select(Issue_UID, Cover_A, Cover_Variant)

covers_long <- issues_a4 %>%
  pivot_longer(
    c(Cover_A, Cover_Variant),
    names_to = "Cover_Type",
    values_to = "Artist_UID"
  ) %>%
  filter(!is.na(Artist_UID), str_starts(Artist_UID, "SA_"))

top10_artist_ids <- covers_long %>%
  group_by(Artist_UID) %>%
  summarise(Total = n(), .groups = "drop") %>%
  slice_max(Total, n = 10) %>%
  pull(Artist_UID)

a4_data <- covers_long %>%
  filter(Artist_UID %in% top10_artist_ids) %>%
  group_by(Artist_UID, Issue_UID) %>%
  summarise(Issues = n(), .groups = "drop") %>%
  left_join(artist_names, by = c("Artist_UID" = "UID")) %>%
  left_join(title_lookup_a4, by = c("Issue_UID" = "U_SeriesID")) %>%
  filter(!is.na(Title_Label)) %>%
  rename(Title = Title_Label) %>%
  mutate(Artist = fct_reorder(Artist, Issues, sum))

# T5_A
artist_ids_a5 <- c(
  "SA_1269" = "George Perez",
  "SA_1212" = "Tony (TS) Daniel",
  "SA_1305" = "Lee Bermejo"
)

issues_a5 <- issues %>%
  mutate(Pub_Year = as.integer(Pub_Year))

title_lookup_a5 <- titles %>%
  select(U_SeriesID, Title_Full, Volume) %>%
  mutate(
    Title_Label = if_else(
      Volume > 1,
      paste0(Title_Full, " (Vol.", Volume, ")"),
      Title_Full
    )
  )

a_covers_a5 <- issues_a5 %>%
  filter(CvrA_Artist %in% names(artist_ids_a5)) %>%
  select(Issue_UID, Pub_Year, artist_id = CvrA_Artist) %>%
  mutate(cover_type = "A Cover")

var_covers_a5 <- issues_a5 %>%
  filter(!is.na(CvrVarArtists)) %>%
  select(Issue_UID, Pub_Year, CvrVarArtists) %>%
  separate_rows(CvrVarArtists, sep = ";\\s*") %>%
  mutate(
    artist_id  = str_extract(CvrVarArtists, "SA_\\d+"),
    code_part  = str_split_fixed(CvrVarArtists, "_SA_", 2)[, 1],
    print_run  = as.integer(str_extract(code_part, "\\d+$")),
    cover_type = if_else(is.na(print_run) | print_run == 1, "1st Print", "Reprint")
  ) %>%
  filter(artist_id %in% names(artist_ids_a5)) %>%
  select(Issue_UID, Pub_Year, artist_id, cover_type)

a5_data <- bind_rows(a_covers_a5, var_covers_a5) %>%
  mutate(U_SeriesID = str_extract(Issue_UID, "^[A-Z*]+_\\d+")) %>%
  left_join(title_lookup_a5, by = "U_SeriesID") %>%
  mutate(
    Artist      = artist_ids_a5[artist_id],
    Title_Label = if_else(is.na(Title_Label), U_SeriesID, Title_Label),
    cover_type  = factor(cover_type, levels = c("A Cover", "1st Print", "Reprint"))
  ) %>%
  filter(!is.na(Pub_Year)) %>%
  count(Artist, Title_Label, Pub_Year, cover_type)

# T1_B
dg_ids     <- c("DC_0556", "DC_0557", "DC_0558", "DC_0559")
dg_pattern <- paste(dg_ids, collapse = "|")

dg_issues <- issues %>%
  filter(
    str_detect(replace_na(Title_Roles, ""), dg_pattern) |
      str_detect(replace_na(Support_Roles, ""), dg_pattern)
  ) %>%
  mutate(
    Codename = case_when(
      str_detect(replace_na(Title_Roles, ""), "DC_0556") ~ "Robin",
      str_detect(replace_na(Title_Roles, ""), "DC_0557") ~ "Nightwing",
      str_detect(replace_na(Title_Roles, ""), "DC_0558") ~ "Agent 37",
      str_detect(replace_na(Title_Roles, ""), "DC_0559") ~ "Batman",
      str_detect(replace_na(Support_Roles, ""), "DC_0556") ~ "Robin",
      str_detect(replace_na(Support_Roles, ""), "DC_0557") ~ "Nightwing",
      str_detect(replace_na(Support_Roles, ""), "DC_0558") ~ "Agent 37",
      str_detect(replace_na(Support_Roles, ""), "DC_0559") ~ "Batman",
      TRUE ~ NA_character_
    ),
    Role = if_else(str_detect(replace_na(Title_Roles, ""), dg_pattern), "Lead", "Guest")
  )

dg_count <- dg_issues %>%
  count(Issue_UID, Codename, Role, name = "dg_issues")

total_count <- issues %>%
  count(Issue_UID, name = "Total_Issues")

b1_data <- dg_count %>%
  left_join(total_count, by = "Issue_UID") %>%
  left_join(titles, by = c("Issue_UID" = "U_SeriesID")) %>%
  mutate(
    proportion  = dg_issues / Total_Issues,
    Title_Label = paste(Title_Full, "Vol", Volume)
  ) %>%
  filter(!is.na(Title_Full)) %>%
  distinct(Issue_UID, Codename, Role, .keep_all = TRUE)

# T2_B
issues_b2 <- issues %>%
  filter(!is.na(Pub_Year)) %>%
  mutate(Pub_Year = as.numeric(Pub_Year))

b2_data <- issues_b2 %>%
  filter(!Arc_Links %in% c("DCA_0000", "b"), !is.na(Arc_Links)) %>%
  group_by(Arc_Links) %>%
  summarise(issue_count = n(), .groups = "drop") %>%
  left_join(arcs %>% select(UID, Arc_Title), by = c("Arc_Links" = "UID")) %>%
  filter(!is.na(Arc_Title), issue_count >= 8) %>%
  arrange(desc(issue_count))

ui <- navbarPage(
  title = div(
    span("🦇", style = "margin-right:6px;"),
    strong("DC Comics Dashboard"),
    style = "color:#FFD700;"
  ),
  theme = NULL,
  header = tags$head(
    tags$style(HTML("
      body { background-color: #0d0d1a; color: #e0e0e0; font-family: 'Segoe UI', sans-serif; }
      .navbar { background-color: #1a1a2e !important; border-bottom: 2px solid #FFD700; }
      .navbar-brand, .navbar-nav > li > a {
        color: #FFD700 !important; font-weight: 600;
      }
      .navbar-nav > li > a:hover { color: #fff !important; background-color: #FFD700 !important; }
      .nav-tabs > li > a { color: #aaa; background-color: #1a1a2e; border-color: #333; }
      .nav-tabs > li.active > a { color: #0d0d1a; background-color: #FFD700; font-weight:bold; }
      .well, .panel { background-color: #1a1a2e; border: 1px solid #333; }
      label { color: #FFD700; }
      h4 { color: #FFD700; }
      .shiny-plot-output { background: #12122a; border-radius: 8px; padding: 10px; }
      .section-header {
        background: linear-gradient(90deg, #1a1a2e, #0d0d1a);
        border-left: 4px solid #FFD700;
        padding: 8px 14px; margin-bottom: 12px; border-radius: 4px;
        color: #FFD700; font-weight: bold; font-size: 15px;
      }
      .essay-box {
        background-color: #1a1a2e;
        border: 1px solid #333;
        border-left: 4px solid #FFD700;
        border-radius: 6px;
        padding: 18px;
        color: #e0e0e0;
        line-height: 1.6;
        margin-top: 20px;
      }
      .essay-box h3, .essay-box h4 {
        color: #FFD700;
      }
    "))
  ),
  
  # Section A
  navbarMenu("Section A",
             
             tabPanel("A1 – Popular Titles",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          div(class = "section-header", "Section A · Task 1"),
                          p("Gantt chart of the 20 most popular DC titles by total issues published, showing their active publication years.", style = "color:#ccc; font-size:13px;"),
                          sliderInput("a1_n", "Number of titles:", min = 5, max = 30, value = 20, step = 1)
                        ),
                        mainPanel(
                          width = 9,
                          plotOutput("plot_a1", height = "620px")
                        )
                      )
             ),
             
             tabPanel("A2 – Prolific Writers",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          div(class = "section-header", "Section A · Task 2"),
                          p("Heatmap of the top 10 most prolific writers and the DC titles they wrote for.", style = "color:#ccc; font-size:13px;"),
                          sliderInput("a2_min_issues", "Min issues to label:", min = 1, max = 20, value = 5)
                        ),
                        mainPanel(
                          width = 9,
                          plotOutput("plot_a2", height = "520px")
                        )
                      )
             ),
             
             tabPanel("A3 – Dixon vs Wolfman",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          div(class = "section-header", "Section A · Task 3"),
                          p("Career comparison of Chuck Dixon and Marv Wolfman — issues and distinct titles per year.", style = "color:#ccc; font-size:13px;"),
                          checkboxGroupInput(
                            "a3_metric", "Show metric:",
                            choices = c("Issues", "Titles"),
                            selected = c("Issues", "Titles")
                          )
                        ),
                        mainPanel(
                          width = 9,
                          plotlyOutput("plot_a3", height = "560px")
                        )
                      )
             ),
             
             tabPanel("A4 – Prolific Cover Artists",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          div(class = "section-header", "Section A · Task 4"),
                          p("Heatmap of the top 10 most prolific cover artists across DC titles, combining A covers and variants.", style = "color:#ccc; font-size:13px;")
                        ),
                        mainPanel(
                          width = 9,
                          plotlyOutput("plot_a4", height = "520px")
                        )
                      )
             ),
             
             tabPanel("A5 – Artist Careers",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          div(class = "section-header", "Section A · Task 5"),
                          p("Career paths of George Perez, Tony (TS) Daniel and Lee Bermejo — covers per title per year, split by cover type.", style = "color:#ccc; font-size:13px;"),
                          checkboxGroupInput(
                            "a5_artists", "Show artists:",
                            choices = c("George Perez", "Tony (TS) Daniel", "Lee Bermejo"),
                            selected = c("George Perez", "Tony (TS) Daniel", "Lee Bermejo")
                          ),
                          checkboxGroupInput(
                            "a5_types", "Cover types:",
                            choices = c("A Cover", "1st Print", "Reprint"),
                            selected = c("A Cover", "1st Print", "Reprint")
                          )
                        ),
                        mainPanel(
                          width = 9,
                          plotOutput("plot_a5", height = "700px")
                        )
                      )
             )
  ),
  
  # Section B
  navbarMenu("Section B",
             
             tabPanel("B1 – Dick Grayson",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          div(class = "section-header", "Section B · Task 1"),
                          p("Proportion of appearances by Dick Grayson in each title volume, split by codename (Robin, Nightwing, Batman, Agent 37) and role (Lead / Guest).", style = "color:#ccc; font-size:13px;"),
                          checkboxGroupInput(
                            "b1_codename", "Codenames:",
                            choices = c("Robin", "Nightwing", "Batman", "Agent 37"),
                            selected = c("Robin", "Nightwing", "Batman", "Agent 37")
                          ),
                          checkboxGroupInput(
                            "b1_role", "Role:",
                            choices = c("Lead", "Guest"),
                            selected = c("Lead", "Guest")
                          )
                        ),
                        mainPanel(
                          width = 9,
                          plotOutput("plot_b1", height = "680px")
                        )
                      )
             ),
             
             tabPanel("B2 – Story Arcs",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          div(class = "section-header", "Section B · Task 2"),
                          p("Largest DC story arcs by number of issues. Filtered to arcs with at least the selected minimum number of issues.", style = "color:#ccc; font-size:13px;"),
                          sliderInput("b2_min", "Min issues to include:", min = 2, max = 20, value = 8, step = 1)
                        ),
                        mainPanel(
                          width = 9,
                          plotOutput("plot_b2", height = "600px")
                        )
                      )
             )
  ),
  
  # Section C
  navbarMenu("Section C",
             
             tabPanel("C1 – Explain and Explore",
                      fluidPage(
                        div(
                          class = "essay-box",
                          h3("Section C1 – Explain B1"),
                          p("For the Dick Grayson task, I figured that this chart would best describe how to compare his presence across many different titles. So I chose the proportional stacked bar chart."),
                          p("It also separates the lead and guest appearances to show what kind of role he had in each of the titles so you can see which comics he is the lead in and which ones he takes a more guest role."),
                          p("I used colour for the codenames so you can see when Dick Grayson was Robin, Nightwing, Batman or Agent 37"),
                          p("Overall this lets me show the proportion of appearances and when he is which codename and the lead/guest appearances of Dick Grayson by facet"),
                          p("A line chart would not have worked for this since that would suggest a continous time trend so I looked up different types of charts and looked at what they all looked like before choosing which one to use."),
                          p("The only real problem I can see with this chart is that if there are too many different title volumes the chart can get very crowded making it harder to read. Which is why my use of faceting and colour is there to help keep the display readable still even if there are a lot of different titles")
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  
  # T1_A
  output$plot_a1 <- renderPlot({
    dat <- a1_data %>%
      arrange(desc(Total_Issues)) %>%
      slice_head(n = input$a1_n) %>%
      mutate(Title_Full = fct_reorder(Title_Full, Total_Issues))
    
    ggplot(dat) +
      geom_segment(
        aes(x = Start_Year, xend = End_Year, y = Title_Full, yend = Title_Full),
        colour = "#1565C0", linewidth = 3.5, alpha = 0.55
      ) +
      geom_text(
        aes(x = End_Year + 1, y = Title_Full, label = paste0(Total_Issues, " issues")),
        hjust = 0, size = 3.2, colour = "#ccc"
      ) +
      scale_x_continuous(limits = c(1930, 2042), breaks = seq(1930, 2040, 10)) +
      labs(
        title = "DC Comics: Most Popular Continuing Titles",
        subtitle = "Ranked by total issues — bar spans active publication years",
        x = "Year", y = NULL,
        caption = "Source: Crackers Do Matter DS"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background    = element_rect(fill = "#12122a", colour = NA),
        panel.background   = element_rect(fill = "#12122a", colour = NA),
        text               = element_text(colour = "#e0e0e0"),
        axis.text          = element_text(colour = "#bbb"),
        plot.title         = element_text(face = "bold", size = 14, colour = "#FFD700"),
        plot.subtitle      = element_text(colour = "#aaa", size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#2a2a4a")
      )
  }, bg = "#12122a")
  
  # T2_A
  output$plot_a2 <- renderPlot({
    ggplot(a2_data, aes(x = Title_Full, y = Writer_Name, fill = Issues)) +
      geom_tile(colour = "#12122a", linewidth = 0.4) +
      geom_text(
        aes(label = ifelse(Issues >= input$a2_min_issues, as.character(Issues), "")),
        size = 2.8, colour = "white", fontface = "bold"
      ) +
      scale_fill_gradient(low = "#1a3a6b", high = "#FFD700", name = "Issues") +
      labs(
        title = "DC Comics: 10 Most Prolific Writers",
        subtitle = "Issues credited per title — darker = more issues",
        x = NULL, y = NULL,
        caption = "Source: Crackers Do Matter DS"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#12122a", colour = NA),
        panel.background = element_rect(fill = "#12122a", colour = NA),
        text             = element_text(colour = "#e0e0e0"),
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 7, colour = "#bbb"),
        axis.text.y      = element_text(size = 10, colour = "#e0e0e0"),
        plot.title       = element_text(face = "bold", size = 14, colour = "#FFD700"),
        plot.subtitle    = element_text(colour = "#aaa", size = 10),
        panel.grid       = element_blank(),
        legend.text      = element_text(colour = "#e0e0e0"),
        legend.title     = element_text(colour = "#FFD700")
      )
  }, bg = "#12122a")
  
  # T3_A
  output$plot_a3 <- renderPlotly({
    req(length(input$a3_metric) > 0)
    dat <- a3_data %>% filter(Metric %in% input$a3_metric)
    
    p <- ggplot(dat, aes(x = Pub_Year, y = Count, fill = Writer)) +
      geom_area(alpha = 0.65, position = "identity") +
      facet_wrap(
        ~Metric,
        ncol = 1,
        scales = "free_y",
        labeller = labeller(Metric = c(Issues = "Issues Published", Titles = "Distinct Titles"))
      ) +
      scale_fill_manual(values = c("Chuck Dixon" = "#e63946", "Marv Wolfman" = "#457b9d")) +
      labs(
        title = "Career Comparison: Chuck Dixon vs Marv Wolfman",
        x = "Year", y = NULL, fill = "Writer"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "#12122a", colour = NA),
        panel.background = element_rect(fill = "#12122a", colour = NA),
        text             = element_text(colour = "#e0e0e0"),
        strip.text       = element_text(face = "bold", colour = "#FFD700"),
        legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#2a2a4a")
      )
    
    ggplotly(p) %>%
      layout(
        paper_bgcolor = "#12122a",
        plot_bgcolor = "#12122a",
        font = list(color = "#e0e0e0")
      )
  })
  
  # T4_A
  output$plot_a4 <- renderPlotly({
    p <- ggplot(
      a4_data,
      aes(x = Title, y = Artist, fill = Issues,
          text = paste0(Artist, "\n", Title, "\n", Issues, " issues"))
    ) +
      geom_tile(colour = "#12122a", linewidth = 0.5) +
      scale_fill_gradient(low = "#1a3a6b", high = "#FFD700", name = "Issues") +
      labs(title = "10 Most Prolific Cover Artists by Title", x = NULL, y = NULL, fill = "Issues") +
      theme_minimal(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "#12122a", colour = NA),
        panel.background = element_rect(fill = "#12122a", colour = NA),
        text             = element_text(colour = "#e0e0e0"),
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 6, colour = "#bbb"),
        panel.grid       = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "#12122a",
        plot_bgcolor = "#12122a",
        font = list(color = "#e0e0e0")
      )
  })
  
  # T5_A
  output$plot_a5 <- renderPlot({
    req(length(input$a5_artists) > 0, length(input$a5_types) > 0)
    
    dat <- a5_data %>%
      filter(Artist %in% input$a5_artists, cover_type %in% input$a5_types)
    
    ggplot(dat, aes(x = Pub_Year, y = Title_Label, fill = n)) +
      geom_tile(colour = "#12122a", linewidth = 0.35) +
      facet_grid(cover_type ~ Artist, scales = "free", space = "free") +
      scale_fill_gradient(low = "#1a3a6b", high = "#FFD700", name = "Covers", na.value = "#1a3a6b") +
      scale_x_continuous(breaks = seq(1975, 2030, by = 5)) +
      labs(
        title = "Cover Artist Career Paths: Perez, Daniel & Bermejo",
        subtitle = "Covers per title per year — split by cover type",
        x = "Publication Year", y = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background  = element_rect(fill = "#12122a", colour = NA),
        panel.background = element_rect(fill = "#12122a", colour = NA),
        text             = element_text(colour = "#e0e0e0"),
        axis.text.x      = element_text(angle = 45, hjust = 1, colour = "#bbb"),
        axis.text.y      = element_text(size = 7, colour = "#ccc"),
        strip.text       = element_text(face = "bold", colour = "#FFD700"),
        strip.text.y     = element_text(angle = 0),
        panel.grid       = element_blank(),
        legend.text      = element_text(colour = "#e0e0e0"),
        legend.title     = element_text(colour = "#FFD700"),
        plot.title       = element_text(face = "bold", colour = "#FFD700"),
        plot.subtitle    = element_text(colour = "#aaa")
      )
  }, bg = "#12122a")
  
  # T1_B
  output$plot_b1 <- renderPlot({
    req(length(input$b1_codename) > 0, length(input$b1_role) > 0)
    
    dat <- b1_data %>%
      filter(Codename %in% input$b1_codename, Role %in% input$b1_role)
    
    ggplot(dat, aes(x = reorder(Title_Label, proportion), y = proportion, fill = Codename)) +
      geom_col(position = "stack") +
      facet_wrap(~Role, scales = "free_y") +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(values = c(
        "Robin" = "#e63946",
        "Nightwing" = "#457b9d",
        "Batman" = "#FFD700",
        "Agent 37" = "#2a9d8f"
      )) +
      coord_flip() +
      labs(
        title = "Dick Grayson: Appearances Across DC Titles",
        subtitle = "Proportion of issues per title volume — by codename and role",
        x = NULL, y = "Proportion of issues", fill = "Codename",
        caption = "Source: Crackers Do Matter DS"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background    = element_rect(fill = "#12122a", colour = NA),
        panel.background   = element_rect(fill = "#12122a", colour = NA),
        text               = element_text(colour = "#e0e0e0"),
        axis.text          = element_text(colour = "#bbb", size = 8),
        strip.text         = element_text(face = "bold", colour = "#FFD700", size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#2a2a4a"),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold", colour = "#FFD700", size = 14),
        plot.subtitle      = element_text(colour = "#aaa"),
        legend.text        = element_text(colour = "#e0e0e0"),
        legend.title       = element_text(colour = "#FFD700")
      )
  }, bg = "#12122a")
  
  # T2_B
  output$plot_b2 <- renderPlot({
    dat <- issues_b2 %>%
      filter(!Arc_Links %in% c("DCA_0000", "b"), !is.na(Arc_Links)) %>%
      group_by(Arc_Links) %>%
      summarise(issue_count = n(), .groups = "drop") %>%
      left_join(arcs %>% select(UID, Arc_Title), by = c("Arc_Links" = "UID")) %>%
      filter(!is.na(Arc_Title), issue_count >= input$b2_min) %>%
      arrange(desc(issue_count))
    
    ggplot(dat, aes(x = reorder(Arc_Title, issue_count), y = issue_count)) +
      geom_col(fill = "#1565C0", alpha = 0.85) +
      geom_text(aes(label = issue_count), hjust = -0.2, colour = "#FFD700", size = 3.2) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(
        title = "Biggest DC Story Arcs by Issues",
        subtitle = paste0("Arcs with ≥ ", input$b2_min, " issues"),
        x = NULL, y = "Number of Issues",
        caption = "Source: Crackers Do Matter DS"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background    = element_rect(fill = "#12122a", colour = NA),
        panel.background   = element_rect(fill = "#12122a", colour = NA),
        text               = element_text(colour = "#e0e0e0"),
        axis.text          = element_text(colour = "#bbb"),
        plot.title         = element_text(face = "bold", colour = "#FFD700", size = 14),
        plot.subtitle      = element_text(colour = "#aaa"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#2a2a4a")
      )
  }, bg = "#12122a")
}

shinyApp(ui, server)
