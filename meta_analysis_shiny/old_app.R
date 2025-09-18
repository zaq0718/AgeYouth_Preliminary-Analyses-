# app.R

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(metafor)
library(patchwork)
library(psych)

# Load and preprocess data
# Load already-processed data directly
library(readxl)
data_aw <- read_xlsx("data/processed_data_aw.xlsx")


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
      sliderInput("female_filter", "% Female Range:", min = 0, max = 100, value = c(0, 100)),
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
        )
      )
    )
  )
)

# Server ----
server <- function(input, output) {
  
  filtered_data <- reactive({
    df <- data_aw
    if (input$affect_filter != "Both") {
      affect_val <- ifelse(input$affect_filter == "Positive", 0, 1)
      df <- df %>% filter(`Negative/Positive+LS` == affect_val)
    }
    df <- df %>% filter(`18_Female` >= input$female_filter[1], `18_Female` <= input$female_filter[2])
    if (input$region_filter != "All") df <- df %>% filter(region == input$region_filter)
    if (input$income_filter != "All") df <- df %>% filter(income_level == input$income_filter)
    if (input$short_lag) df <- df %>% filter(WBtitj <= 0.5)
    df
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
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
