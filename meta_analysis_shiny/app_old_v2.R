# app.R

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(metafor)
library(patchwork)
library(psych)
library(DT)          # <-- for interactive tables (new)

# Load and preprocess data
# Load already-processed data directly
library(readxl)
data_aw <- read_xlsx("data/processed_data_aw.xlsx")

# ---- Quality Coding data (cleaned Excel) ----
qc_path <- "data/quality_coding_clean.xlsx"   # adjust if saved elsewhere (e.g., "data/cleaned_data.xlsx")
dat_qc  <- read_xlsx(qc_path)

# Recode numeric 0/1 back into Yes/No
dat_qc <- dat_qc %>%
  mutate(across(
    -c(`0_IDF`, `1_ID`, `2_Author`, `3_Year`, `4_Title`),   # keep identifiers as is
    ~ case_when(
      . == 0 ~ "Yes",
      . == 1 ~ "No",
      TRUE   ~ NA_character_
    )
  ))

# Derived columns ----
data_aw <- data_aw %>%
  mutate(
    avg_rel = rowMeans(select(., `33_Relt1`, `34_Relt`), na.rm = TRUE),
    timelagc = WBtitj - mean(WBtitj, na.rm = TRUE),
    corrdis = WBcorrij / avg_rel,
    v_corr = 1 / (`10_Size` - 3),
    v_corrdis = v_corr / (avg_rel^2)
  )

# UI ----
ui <- fluidPage(
  titlePanel("Meta-Analysis of Well-being Studies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("affect_filter", "Affect Type:",
                  choices = c("Both", "Positive", "Negative"),
                  selected = "Both"),
      # sliderInput("female_filter", "% Female Range:", min = 0, max = 100, value = c(0, 100)),
      selectInput("region_filter", "Region:", choices = c("All", unique(data_aw$region))),
      selectInput("income_filter", "Income Level:", choices = c("All", unique(data_aw$income_level))),
      checkboxInput("short_lag", "Short Time Lag Only (0.5 yrs)", FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary",
                 verbatimTextOutput("summary_output"),
                 tableOutput("data_table")
        ),
        tabPanel("Meta-Analysis",
                 verbatimTextOutput("meta_output")
        ),
        tabPanel("Funnel Plots",
                 plotOutput("funnel_plot")
        ),
        tabPanel("Lollipop Charts",
                 plotOutput("lollipop_plot")
        ),
        tabPanel("Rank-Order Stability",
                 verbatimTextOutput("rank_output")
        ),
        
        # ---- Quality Coding (reworked with overview + dropdowns) ----
        tabPanel("Quality Coding",
                 h3("Quality Coding"),
                 h4("Overall"),
                 DTOutput("qc_overall"),    # overall interactive view (new)
                 
                 # Dropdowns (collapsible)
                 tags$details(
                   tags$summary(strong("Basic Information of Publication (click for details)")),
                   br(), tableOutput("qc_basic_info")
                 ),
                 tags$details(
                   tags$summary(strong("General Reporting Expectation (click for details)")),
                   br(), tableOutput("qc_gre1")
                 ),
                 tags$details(
                   tags$summary(strong("Sample recruitment (click for details)")),
                   br(), tableOutput("qc_sample_recruit")
                 ),
                 tags$details(
                   tags$summary(strong("Data collection (click for details)")),
                   br(), tableOutput("qc_data_collection")
                 ),
                 tags$details(
                   tags$summary(strong("Measurement (click for details)")),
                   br(), tableOutput("qc_measurement")
                 ),
                 tags$details(
                   tags$summary(strong("Statistical and data analysis (click for details)")),
                   br(), tableOutput("qc_stats1")
                 ),
                 tags$details(
                   tags$summary(strong("Dropout (click for details)")),
                   br(), tableOutput("qc_dropout")
                 ),
                 tags$details(
                   tags$summary(strong("Missing data (click for details)")),
                   br(), tableOutput("qc_missing")
                 ),
                 tags$details(
                   tags$summary(strong("Statistical and data analysis (click for details)")),
                   br(), tableOutput("qc_stats2")
                 ),
                 tags$details(
                   tags$summary(strong("General Reporting Expectation (click for details)")),
                   br(), tableOutput("qc_gre2")
                 )
        )
        # ---- END Quality Coding ----
      )
    )
  )
)

# Server ----
# --------------for meta analysis--------------
server <- function(input, output) {
  
  filtered_data <- reactive({
    df <- data_aw
    if (input$affect_filter != "Both") {
      affect_val <- ifelse(input$affect_filter == "Positive", 0, 1)
      df <- df %>% filter(`Negative/Positive+LS` == affect_val)
    }
 #   df <- df %>% filter(`18_Female` >= input$female_filter[1], `18_Female` <= input$female_filter[2])
    if (input$region_filter != "All") df <- df %>% filter(region == input$region_filter) # will show data overall only for region 
    if (input$income_filter != "All") df <- df %>% filter(income_level == input$income_level)
    if (input$short_lag) df <- df %>% filter(WBtitj <= 0.5)
    df # if no any filter is selected, or condition fail, then show original data
  })
  
  output$summary_output <- renderPrint({
    summary(filtered_data())
  })
  
  output$data_table <- renderTable({
    head(filtered_data(), 10)
  })
  
  output$meta_output <- renderPrint({
    dat <- filtered_data()
    dat <- dat %>% mutate(
      es_d = (WBmj - WBmi) / WBsdi,
      es_dyear = es_d / WBtitj,
      v_d = ((2 * (1 - WBcorrij) / `10_Size`) + (es_d^2 / (2 * `10_Size`))),
      v_dyear = v_d / (WBtitj^2)
    )
    tryCatch({
      res <- rma.mv(yi = es_dyear, V = v_dyear, random = ~1 | `1.1_CaseID`/RowID, data = dat)
      summary(res)
    }, error = function(e) paste("Model error:", e$message))
  })
  
  output$funnel_plot <- renderPlot({
    dat <- filtered_data()
    dat <- dat %>% mutate(
      es_d = (WBmj - WBmi) / WBsdi,
      es_dyear = es_d / WBtitj,
      v_d = ((2 * (1 - WBcorrij) / `10_Size`) + (es_d^2 / (2 * `10_Size`))),
      v_dyear = v_d / (WBtitj^2)
    )
    res <- tryCatch({
      rma(yi = es_dyear, vi = v_dyear, data = dat)
    }, error = function(e) NULL)
    if (!is.null(res)) funnel(res)
  })
  
  output$lollipop_plot <- renderPlot({
    dat <- filtered_data()
    dat <- dat %>% mutate(decade = floor(as.numeric(`3_Year`) / 5) * 5)
    agg <- dat %>% group_by(region, decade) %>%
      summarise(mean_es = mean(es_dyear, na.rm = TRUE))
    
    ggplot(agg, aes(x = as.factor(decade), y = mean_es)) +
      geom_segment(aes(xend = as.factor(decade), y = 0, yend = mean_es)) +
      geom_point(size = 3) +
      facet_wrap(~region) +
      theme_minimal() +
      labs(title = "Mean Effect per Year by Region and Decade",
           x = "Decade", y = "Mean Effect")
  })
  
  output$rank_output <- renderPrint({
    dat <- filtered_data()
    tryCatch({
      res <- rma.mv(yi = corrdis, V = v_corrdis, random = ~1 | `1.1_CaseID`/RowID, data = dat)
      summary(res)
    }, error = function(e) paste("Model error:", e$message))
  })
  
  # ---- Quality Coding outputs ----
  
  # Overall: show all relevant columns together as an interactive table
  output$qc_overall <- renderDT({
    all_cols <- c("0_IDF","1_ID","2_Author","3_Year","4_Title",
                  "1_Qaim","2_Qpar","3_Qrep",
                  "4_Agre","5_AgrRep",
                  "6_Qdat",
                  "7_Qmeasure",
                  "8_Qconf","9_Qsat",
                  "10_Qarr",
                  "11_Qmiss","12_Qmisshan",
                  "13_QconS",
                  "14_Qfind")
    keep <- intersect(names(dat_qc), all_cols)
    datatable(dat_qc[keep], options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$qc_basic_info <- renderTable({
    keep <- intersect(names(dat_qc), c("0_IDF","1_ID","2_Author","3_Year","4_Title"))
    head(dat_qc[keep], 10)
  })
  
  output$qc_gre1 <- renderTable({
    keep <- intersect(names(dat_qc), c("1_Qaim","2_Qpar","3_Qrep"))
    head(dat_qc[keep], 10)
  })
  
  output$qc_sample_recruit <- renderTable({
    keep <- intersect(names(dat_qc), c("4_Agre","5_AgrRep"))
    head(dat_qc[keep], 10)
  })
  
  output$qc_data_collection <- renderTable({
    keep <- intersect(names(dat_qc), c("6_Qdat"))
    head(dat_qc[keep], 10)
  })
  
  output$qc_measurement <- renderTable({
    keep <- intersect(names(dat_qc), c("7_Qmeasure"))
    head(dat_qc[keep], 10)
  })
  
  output$qc_stats1 <- renderTable({
    keep <- intersect(names(dat_qc), c("8_Qconf","9_Qsat"))
    head(dat_qc[keep], 10)
  })
  
  output$qc_dropout <- renderTable({
    keep <- intersect(names(dat_qc), c("10_Qarr"))
    head(dat_qc[keep], 10)
  })
  
  output$qc_missing <- renderTable({
    keep <- intersect(names(dat_qc), c("11_Qmiss","12_Qmisshan"))
    head(dat_qc[keep], 10)
  })
  
  output$qc_stats2 <- renderTable({
    keep <- intersect(names(dat_qc), c("13_QconS"))
    head(dat_qc[keep], 10)
  })
  
  output$qc_gre2 <- renderTable({
    keep <- intersect(names(dat_qc), c("14_Qfind"))
    head(dat_qc[keep], 10)
  })
  # ---- END Quality Coding ----
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
