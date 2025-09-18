<<<<<<< HEAD
# app.R

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(metafor)
library(patchwork)
library(psych)
library(DT)
library(stringr)
library(purrr)
library(tidyr)

# ---------------- Data Load ----------------

# Well-being data
data_aw <- read_xlsx("data/processed_data_aw.xlsx")

# Quality Coding data (already cleaned to 0/1/NA earlier and recoded here to Yes/No/NA)
qc_path <- "data/quality_coding_clean.xlsx"
dat_qc  <- read_xlsx(qc_path)

# Recode numeric 0/1 back into Yes/No, but KEEP Rep_Rep as 0/1
protect_cols <- c("0_IDF","1_ID","2_Author","3_Year","4_Title","Rep_Rep")

dat_qc <- dat_qc %>%
  mutate(across(
    -all_of(protect_cols),
    ~ case_when(
      . == 0 ~ "Yes",
      . == 1 ~ "No",
      TRUE   ~ NA_character_
    )
  ))

# ---------------- Derived Columns ----------------
data_aw <- data_aw %>%
  mutate(
    avg_rel   = rowMeans(select(., `33_Relt1`, `34_Relt`), na.rm = TRUE),
    timelagc  = WBtitj - mean(WBtitj, na.rm = TRUE),
    corrdis   = WBcorrij / avg_rel,
    v_corr    = 1 / (`10_Size` - 3),
    v_corrdis = v_corr / (avg_rel^2)
  )

# ---------------- Helpers ----------------

is_numeric_like <- function(x) is.numeric(x) || inherits(x, c("integer", "double"))
is_categorical_like <- function(x) is.factor(x) || is.character(x)

# Build a tidy "README" table of variable types for data_aw (no class, no example)
build_readme <- function(df) {
  tibble(
    variable   = names(df),
    type       = map_chr(df, ~ case_when(
      is.numeric(.x)    ~ "numeric",
      is.integer(.x)    ~ "integer",
      is.factor(.x)     ~ "categorical",
      is.character(.x)  ~ "character",
      TRUE              ~ "other"
    )),
    n_missing  = map_int(df, ~ sum(is.na(.x))),
    n_unique   = map_int(df, ~ length(unique(.x)))
  )
}

readme_aw <- build_readme(data_aw)

# Logical constraint checker:
constraint_message <- function(region, income) {
  if (!is.null(region) && !is.null(income) &&
      region != "All" && income != "All") {
    if (region == "Europe" && str_detect(tolower(income), "middle")) {
      return("Selection conflict: Europe is typically not categorized as 'Middle' income in this dataset. Showing unfiltered data and explaining the constraint.")
    }
  }
  return(NULL)
}

# ---------------- UI ----------------
ui <- fluidPage(
  titlePanel("Meta-analysis of longitudinal studies on subjective well-being during adolescence"),
  sidebarLayout(
    sidebarPanel(
      selectInput("affect_filter", "Affect Type:",
                  choices = c("Both", "Positive", "Negative"),
                  selected = "Both"),
      selectInput("region_filter", "Region:", choices = c("All", unique(data_aw$region))),
      selectInput("income_filter", "Income Level:", choices = c("All", unique(data_aw$income_level))),
      checkboxInput("short_lag", "Short Time Lag Only (0.5 yrs)", FALSE),
      hr(),
      
      # Show categorical selector ONLY when README tab is active
      conditionalPanel(
        condition = "input.main_tabs == 'README (Variables & Types)'",
        uiOutput("cat_var_selector")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        # README tab
        tabPanel("README (Variables & Types)",
                 h4("Variable Dictionary"),
                 DTOutput("readme_dt"),
                 hr(),
                 h4("Categorical Preview (selected on left)"),
                 plotOutput("readme_categorical_preview", height = "300px")
        ),
        
        # Overall searchable table
        tabPanel("Overall Data (Searchable)",
                 DTOutput("overall_dt")
        ),
        
        # Data Summary
        tabPanel("Data Summary",
                 textOutput("logic_msg"),
                 h4("Numeric Variables - Summary"),
                 DTOutput("numeric_summary_dt"),
                 hr()
        ),
        
        # Meta and plots
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
        
        # --- Overall Quality Assessment tab (searchable + discrete stats) ---
        tabPanel("Quality Assessment",
                 h3("Quality Assessment : Overall"),
                 h4("Overall (Searchable)"),
                 DTOutput("qc_overall"),
                 tags$br(),
                 h4("Discrete Statistics (Yes/No/NA counts)"),
                 DTOutput("qc_discrete_summary")
        ),
        
        # --- Section-wise-only tab (no overall tables here) ---
        tabPanel("Quality Assessment : Section-wise Output",
                 h3("Quality Assessment : Section-wise Overview"),
                 
                 # Quick in-page TOC
                 tags$ul(
                   tags$li(tags$a(href = "#qc_basic_anchor", "Basic Information of Publication")),
                   tags$li(tags$a(href = "#qc_gre1_anchor", "General Reporting Expectation")),
                   tags$li(tags$a(href = "#qc_sample_anchor", "Sample Recruitment")),
                   tags$li(tags$a(href = "#qc_data_anchor", "Data Collection")),
                   tags$li(tags$a(href = "#qc_meas_anchor", "Measurement")),
                   tags$li(tags$a(href = "#qc_stats1_anchor", "Statistical & Data Analysis (1)")),
                   tags$li(tags$a(href = "#qc_drop_anchor", "Dropout")),
                   tags$li(tags$a(href = "#qc_miss_anchor", "Missing Data")),
                   tags$li(tags$a(href = "#qc_stats2_anchor", "Statistical & Data Analysis (2)")),
                   tags$li(tags$a(href = "#qc_gre2_anchor", "General Reporting Expectation (2)"))
                 ),
                 tags$hr(),
                 
                 # Section: Basic Information of Publication
                 tags$div(id = "qc_basic_anchor"),
                 h4("Basic Information of Publication"),
                 tableOutput("qc_basic_info"),
                 tags$hr(),
                 
                 # Section: General Reporting Expectation (primary)
                 tags$div(id = "qc_gre1_anchor"),
                 h4("General Reporting Expectation"),
                 tableOutput("qc_gre1"),
                 tags$hr(),
                 
                 # Section: Sample Recruitment
                 tags$div(id = "qc_sample_anchor"),
                 h4("Sample Recruitment"),
                 tableOutput("qc_sample_recruit"),
                 tags$hr(),
                 
                 # Section: Data Collection
                 tags$div(id = "qc_data_anchor"),
                 h4("Data Collection"),
                 tableOutput("qc_data_collection"),
                 tags$hr(),
                 
                 # Section: Measurement
                 tags$div(id = "qc_meas_anchor"),
                 h4("Measurement"),
                 tableOutput("qc_measurement"),
                 tags$hr(),
                 
                 # Section: Statistical & Data Analysis (1)
                 tags$div(id = "qc_stats1_anchor"),
                 h4("Statistical & Data Analysis (1)"),
                 tableOutput("qc_stats1"),
                 tags$hr(),
                 
                 # Section: Dropout
                 tags$div(id = "qc_drop_anchor"),
                 h4("Dropout"),
                 tableOutput("qc_dropout"),
                 tags$hr(),
                 
                 # Section: Missing Data
                 tags$div(id = "qc_miss_anchor"),
                 h4("Missing Data"),
                 tableOutput("qc_missing"),
                 tags$hr(),
                 
                 # Section: Statistical & Data Analysis (2)
                 tags$div(id = "qc_stats2_anchor"),
                 h4("Statistical & Data Analysis (2)"),
                 tableOutput("qc_stats2"),
                 tags$hr(),
                 
                 # Section: General Reporting Expectation (2)
                 tags$div(id = "qc_gre2_anchor"),
                 h4("General Reporting Expectation (2)"),
                 tableOutput("qc_gre2")
        ),
        )
      )
    )
  )

# ---------------- Server ----------------
server <- function(input, output, session) {
  
  # Categorical selector (only rendered when README tab is active)
  output$cat_var_selector <- renderUI({
    cat_vars <- names(data_aw)[map_lgl(data_aw, is_categorical_like)]
    if (length(cat_vars) == 0) {
      return(helpText("No categorical variables detected."))
    }
    selectInput("categorical_var", "Choose Categorical Variable:",
                choices = cat_vars, selected = cat_vars[1])
  })
  
  # Apply filters, with constraint handling
  filtered_data <- reactive({
    region_sel <- input$region_filter
    income_sel <- input$income_filter
    msg <- constraint_message(region_sel, income_sel)
    
    df <- data_aw
    
    if (input$affect_filter != "Both") {
      affect_val <- ifelse(input$affect_filter == "Positive", 0, 1)
      df <- df %>% filter(`Negative/Positive+LS` == affect_val)
    }
    
    if (!is.null(msg)) {
      attr(df, "logic_msg") <- msg
      return(df)
    }
    
    if (region_sel != "All") df <- df %>% filter(region == region_sel)
    if (income_sel != "All") df <- df %>% filter(income_level == income_sel)
    if (input$short_lag)     df <- df %>% filter(WBtitj <= 0.5)
    
    attr(df, "logic_msg") <- NULL
    df
  })
  
  # Logic message
  output$logic_msg <- renderText({
    df <- filtered_data()
    msg <- attr(df, "logic_msg")
    if (is.null(msg)) "" else msg
  })
  
  # Overall searchable DT
  output$overall_dt <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # README table (without class/example)
  output$readme_dt <- renderDT({
    datatable(readme_aw, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  # README categorical preview (driven by selector in sidebar; shown only on README tab)
  output$readme_categorical_preview <- renderPlot({
    req(input$categorical_var)
    var <- input$categorical_var
    ggplot(data_aw, aes(x = .data[[var]])) +
      geom_bar() +
      theme_minimal() +
      labs(x = var, y = "Count",
           title = paste("Distribution of", var, "(README Preview)")) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  })
  
  
  # Data Summary - numeric stats
  output$numeric_summary_dt <- renderDT({
    df <- filtered_data()
    num_vars <- names(df)[map_lgl(df, is_numeric_like)]
    if (length(num_vars) == 0) {
      return(datatable(data.frame(Note = "No numeric variables available after filtering.")))
    }
    stats <- df %>%
      select(all_of(num_vars)) %>%
      summarise(across(everything(),
                       list(
                         n = ~sum(!is.na(.)),
                         mean = ~mean(., na.rm = TRUE),
                         sd = ~sd(., na.rm = TRUE),
                         min = ~min(., na.rm = TRUE),
                         max = ~max(., na.rm = TRUE)
                       ),
                       .names = "{.col}__{.fn}")) %>%
      pivot_longer(everything(),
                   names_to = c("variable","stat"),
                   names_sep = "__",
                   values_to = "value") %>%
      pivot_wider(names_from = stat, values_from = value)
    
    datatable(stats, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  # Meta-analysis
  output$meta_output <- renderPrint({
    dat <- filtered_data()
    dat <- dat %>% mutate(
      WBmj     = as.numeric(WBmj),
      WBmi     = as.numeric(WBmi),
      WBsdi    = as.numeric(WBsdi),
      WBtitj   = as.numeric(WBtitj),
      WBcorrij = as.numeric(WBcorrij),
      `10_Size`= as.numeric(`10_Size`),
      es_d     = (WBmj - WBmi) / WBsdi,
      es_dyear = es_d / WBtitj,
      v_d      = ((2 * (1 - WBcorrij) / `10_Size`) + (es_d^2 / (2 * `10_Size`))),
      v_dyear  = v_d / (WBtitj^2),
      `1.1_CaseID` = as.factor(`1.1_CaseID`),
      RowID        = if (!"RowID" %in% names(.)) row_number() else RowID,
      RowID        = as.factor(RowID)
    ) %>%
      filter(is.finite(es_dyear), is.finite(v_dyear), v_dyear > 0)
    
    tryCatch({
      res <- rma.mv(yi = es_dyear, V = v_dyear,
                    random = ~ 1 | `1.1_CaseID`/RowID, data = dat)
      summary(res)
    }, error = function(e) paste("Model error:", e$message))
  })
  
  output$funnel_plot <- renderPlot({
    dat <- filtered_data()
    dat <- dat %>% mutate(
      WBmj     = as.numeric(WBmj),
      WBmi     = as.numeric(WBmi),
      WBsdi    = as.numeric(WBsdi),
      WBtitj   = as.numeric(WBtitj),
      WBcorrij = as.numeric(WBcorrij),
      `10_Size`= as.numeric(`10_Size`),
      es_d     = (WBmj - WBmi) / WBsdi,
      es_dyear = es_d / WBtitj,
      v_d      = ((2 * (1 - WBcorrij) / `10_Size`) + (es_d^2 / (2 * `10_Size`))),
      v_dyear  = v_d / (WBtitj^2)
    )
    res <- tryCatch({
      rma(yi = es_dyear, vi = v_dyear, data = dat)
    }, error = function(e) NULL)
    if (!is.null(res)) funnel(res)
  })
  
  output$lollipop_plot <- renderPlot({
    dat <- filtered_data()
    dat <- dat %>%
      mutate(es_d = (as.numeric(WBmj) - as.numeric(WBmi)) / as.numeric(WBsdi),
             es_dyear = es_d / as.numeric(WBtitj),
             decade = floor(as.numeric(`3_Year`) / 5) * 5)
    agg <- dat %>% group_by(region, decade) %>%
      summarise(mean_es = mean(es_dyear, na.rm = TRUE), .groups = "drop")
    
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
      res <- rma.mv(yi = corrdis, V = v_corrdis,
                    random = ~ 1 | `1.1_CaseID`/RowID, data = dat)
      summary(res)
    }, error = function(e) paste("Model error:", e$message))
  })
  
  # ---------------- Quality Coding ----------------
  
  output$qc_overall <- renderDT({
    all_cols <- c("0_IDF","1_ID","2_Author","3_Year","4_Title",
                  "1_Qaim","2_Qpar","3_Qrep",
                  "4_Agre","5_AgrRep", "Rep_Rep",
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
  
  output$qc_discrete_summary <- renderDT({
    id_cols <- c("0_IDF","1_ID","2_Author","3_Year","4_Title")
    qc_cols <- setdiff(names(dat_qc), id_cols)
    if (length(qc_cols) == 0) {
      return(datatable(data.frame(Note = "No QC columns available.")))
    }
    tmp <- dat_qc %>% mutate(across(all_of(qc_cols), ~ as.character(.)))
    long <- tmp %>%
      select(all_of(qc_cols)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
      mutate(value = replace_na(value, "NA"))
    summary_tbl <- long %>%
      group_by(variable, value) %>%
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = value, values_from = n, values_fill = 0) %>%
      arrange(variable)
    datatable(summary_tbl, options = list(pageLength = 20, scrollX = TRUE))
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
    keep <- intersect(names(dat_qc), c("4_Agre","5_AgrRep", "Rep_Rep"))
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
}

# ---------------- Run ----------------
shinyApp(ui = ui, server = server)
=======
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

#data_aw <- read_xlsx("data/long_format_with_newcol.xlsx") %>%
#  mutate(
   # `1.1_CaseID` = as.numeric(`1.1_CaseID`),
   # `2_Author` = as.factor(`2_Author`),
   # `11_Mage` = as.numeric(`11_Mage`),
   # `16_CNT` = as.factor(`16_CNT`),
   # `17_Eth` = as.factor(`17_Eth`),
  #  `18_Female` = as.numeric(`18_Female`),
  #  `Negative/Positive+LS` = as.numeric(`Negative/Positive+LS`),
  #  `33_Relt1` = as.numeric(`33_Relt1`),
  #  `34_Relt` = as.numeric(`34_Relt`)
  #)

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
>>>>>>> fb0254f3d5c6721e966e7c02d1c642553523eed8
