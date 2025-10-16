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

# Quality Coding data
qc_path <- "data/quality_coding_clean.xlsx"
dat_qc  <- read_xlsx(qc_path)

# Remove reviewer/agreement columns if present (user requested earlier)
drop_cols <- c("5_Reviewer", "4_Agre")
dat_qc <- dat_qc %>% select(-any_of(drop_cols))

# Recode 15_Sam (only if present)
if ("15_Sam" %in% names(dat_qc)) {
  dat_qc <- dat_qc %>%
    mutate(`15_Sam` = case_when(
      `15_Sam` %in% c(1, "1") ~ "Random Probability sample",
      `15_Sam` %in% c(0, "0") ~ "not random probability Sample",
      `15_Sam` %in% c("9999", "unable to determine") ~ NA_character_,
      TRUE ~ as.character(`15_Sam`)
    ))
}

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

is_numeric_like_basic <- function(v) {
  # treat numeric vectors or vectors that are fully numeric when coerced as numeric-like
  if (is.numeric(v)) return(TRUE)
  vch <- as.character(v)
  vch <- vch[!is.na(vch) & vch != ""]
  if (length(vch) == 0) return(TRUE)
  all(grepl("^\\s*-?\\d+(?:\\.\\d+)?\\s*$", vch))
}

is_numeric_like <- function(x) is.numeric(x) || inherits(x, c("integer", "double"))
is_categorical_like <- function(x) is.factor(x) || is.character(x)

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
      conditionalPanel(
        condition = "input.main_tabs == 'README (Variables & Types)'",
        uiOutput("cat_var_selector")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("README (Variables & Types)",
                 h4("Variable Dictionary"),
                 DTOutput("readme_dt"),
                 hr(),
                 h4("Categorical Preview (selected on left)"),
                 plotOutput("readme_categorical_preview", height = "300px")
        ),
        tabPanel("Overall Data (Searchable)",
                 DTOutput("overall_dt")
        ),
        tabPanel("Data Summary",
                 textOutput("logic_msg"),
                 h4("Numeric Variables - Summary"),
                 DTOutput("numeric_summary_dt"),
                 hr()
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
        tabPanel("Quality Assessment",
                 h3("Quality Assessment : Overall"),
                 h4("Overall (Searchable)"),
                 DTOutput("qc_overall"),
                 tags$br(),
                 h4("Discrete Statistics (Yes/No/Unable/NA counts)"),
                 DTOutput("qc_discrete_summary")
        ),
        tabPanel("Quality Assessment : Section-wise Output",
                 h3("Quality Assessment : Section-wise Overview"),
                 tags$ul(
                   tags$li(tags$a(href = "#qc_basic_anchor", "Basic Information of Publication")),
                   tags$li(tags$a(href = "#qc_gre1_anchor", "General Reporting Expectation")),
                   tags$li(tags$a(href = "#qc_sample_anchor", "Sample Recruitment")),
                   tags$li(tags$a(href = "#qc_data_anchor", "Data Collection")),
                   tags$li(tags$a(href = "#qc_meas_anchor", "Measurement")),
                   tags$li(tags$a(href = "#qc_stats1_anchor", "Statistical & Data Analysis")),
                   tags$li(tags$a(href = "#qc_drop_anchor", "Dropout")),
                   tags$li(tags$a(href = "#qc_miss_anchor", "Missing Data"))
                 ),
                 tags$hr(),
                 tags$div(id = "qc_basic_anchor"),
                 h4("Basic Information of Publication"),
                 tableOutput("qc_basic_info"),
                 tags$hr(),
                 tags$div(id = "qc_gre1_anchor"),
                 h4("General Reporting Expectation"),
                 tableOutput("qc_gre1"),
                 tags$hr(),
                 tags$div(id = "qc_sample_anchor"),
                 h4("Sample Recruitment"),
                 tableOutput("qc_sample_recruit"),
                 tags$hr(),
                 tags$div(id = "qc_data_anchor"),
                 h4("Data Collection"),
                 tableOutput("qc_data_collection"),
                 tags$hr(),
                 tags$div(id = "qc_meas_anchor"),
                 h4("Measurement"),
                 tableOutput("qc_measurement"),
                 tags$hr(),
                 tags$div(id = "qc_stats1_anchor"),
                 h4("Statistical & Data Analysis"),
                 tableOutput("qc_stats1"),
                 tags$hr(),
                 tags$div(id = "qc_drop_anchor"),
                 h4("Dropout"),
                 tableOutput("qc_dropout"),
                 tags$hr(),
                 tags$div(id = "qc_miss_anchor"),
                 h4("Missing Data"),
                 tableOutput("qc_missing"),
                 tags$hr()
        )
      )
    )
  )
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  
  output$cat_var_selector <- renderUI({
    cat_vars <- names(data_aw)[map_lgl(data_aw, is_categorical_like)]
    if (length(cat_vars) == 0) {
      return(helpText("No categorical variables detected."))
    }
    selectInput("categorical_var", "Choose Categorical Variable:",
                choices = cat_vars, selected = cat_vars[1])
  })
  
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
  
  output$logic_msg <- renderText({
    df <- filtered_data()
    msg <- attr(df, "logic_msg")
    if (is.null(msg)) "" else msg
  })
  
  output$overall_dt <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$readme_dt <- renderDT({
    datatable(readme_aw, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$readme_categorical_preview <- renderPlot({
    req(input$categorical_var)
    var <- input$categorical_var
    ggplot(data_aw, aes(x = .data[[var]])) +
      geom_bar() +
      theme_minimal() +
      labs(x = var, y = "Count",
           title = paste("Distribution of", var, "(README Preview)")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
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
      RowID = if (!"RowID" %in% names(.)) row_number() else RowID,
      RowID = as.factor(RowID)
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
                  "1_Qaim","2_Qpar",
                  "4_Agre","15_Sam",
                  "6_Qdat","7_Qmeasure",
                  "8_Qconf","9_Qsat",
                  "10_Qarr","11_Qmiss","12_Qmisshan",
                  "13_QconS","14_Qfind")
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
    
    # ---- canonicalize/collapse value columns (case-insensitive) ----
    # Keep 'variable' column untouched and collapse the other columns which are value labels
    orig_names <- colnames(summary_tbl)
    if ("variable" %in% orig_names) {
      var_idx <- which(orig_names == "variable")
      value_idx <- setdiff(seq_along(orig_names), var_idx)
      value_names <- orig_names[value_idx]
      
      canonicalize_name <- function(n) {
        n2 <- str_trim(tolower(as.character(n)))
        if (n2 %in% c("0", "yes", "y", "true")) return("Yes")
        if (n2 %in% c("1", "no", "n", "false")) return("No")
        if (n2 %in% c("9999", "unable to determine", "unable to determine")) return("Unable to determine")
        if (n2 %in% c("na", "not provided", "n/a", "", "missing")) return("Not Provided")
        # fallback: Title Case of the cleaned name
        return(str_to_title(n2))
      }
      
      canon_names <- vapply(value_names, canonicalize_name, character(1))
      unique_names <- unique(canon_names)
      
      collapsed_values <- lapply(unique_names, function(cn) {
        idx_local <- which(canon_names == cn)
        if (length(idx_local) == 1) {
          # return the original numeric column (ensure numeric)
          col <- summary_tbl[[ value_idx[idx_local] ]]
          if (is_numeric_like_basic(col)) return(as.numeric(col))
          return(as.character(col))
        } else {
          cols <- summary_tbl[ , value_idx[idx_local], drop = FALSE]
          # if all numeric-like -> sum them
          if (all(sapply(cols, is_numeric_like_basic))) {
            mat <- sapply(cols, function(x) as.numeric(as.character(x)))
            return(rowSums(mat, na.rm = TRUE))
          } else {
            # otherwise coalesce to first non-NA / non-empty per row (as character)
            cols_char <- lapply(cols, function(x) {
              x_char <- as.character(x)
              x_char[x_char == ""] <- NA_character_
              x_char
            })
            return(Reduce(function(a, b) dplyr::coalesce(a, b), cols_char))
          }
        }
      })
      
      # assemble final cleaned table: variable + collapsed columns
      clean_tbl <- data.frame(variable = summary_tbl$variable, stringsAsFactors = FALSE)
      for (i in seq_along(unique_names)) {
        clean_tbl[[ unique_names[i] ]] <- collapsed_values[[i]]
      }
      
    } else {
      # no 'variable' column (unlikely) - just process all columns
      # fallback: apply simpler canonicalization to colnames then collapse
      orig_names2 <- colnames(summary_tbl)
      canon_names2 <- vapply(orig_names2, function(n) {
        n2 <- str_trim(tolower(as.character(n)))
        if (n2 %in% c("0", "yes", "y", "true")) return("Yes")
        if (n2 %in% c("1", "no", "n", "false")) return("No")
        if (n2 %in% c("9999", "unable to determine", "unable to determine")) return("Unable to determine")
        if (n2 %in% c("na", "not provided", "n/a", "", "missing")) return("Not Provided")
        str_to_title(n2)
      }, character(1))
      unique_names2 <- unique(canon_names2)
      collapsed_values2 <- lapply(unique_names2, function(cn) {
        idx_local <- which(canon_names2 == cn)
        cols <- summary_tbl[ , idx_local, drop = FALSE]
        if (all(sapply(cols, is_numeric_like_basic))) {
          mat <- sapply(cols, function(x) as.numeric(as.character(x)))
          return(rowSums(mat, na.rm = TRUE))
        } else {
          cols_char <- lapply(cols, function(x) {
            x_char <- as.character(x)
            x_char[x_char == ""] <- NA_character_
            x_char
          })
          return(Reduce(function(a, b) dplyr::coalesce(a, b), cols_char))
        }
      })
      clean_tbl <- as.data.frame(collapsed_values2, stringsAsFactors = FALSE)
      colnames(clean_tbl) <- unique_names2
    }
    
    datatable(clean_tbl, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  ####################
  output$qc_basic_info <- renderTable({
    keep <- intersect(names(dat_qc), c("0_IDF","1_ID","2_Author","3_Year","4_Title"))
    head(dat_qc[keep], 10)
  })
  output$qc_gre1 <- renderTable({
    keep <- intersect(names(dat_qc), c("1_Qaim","2_Qpar","14_Qfind"))
    head(dat_qc[keep], 10)
  })
  output$qc_sample_recruit <- renderTable({
    keep <- intersect(names(dat_qc), c("4_Agre","15_Sam"))
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
    keep <- intersect(names(dat_qc), c("8_Qconf","9_Qsat","13_QconS"))
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
}

# ---------------- Run ----------------
shinyApp(ui = ui, server = server)
