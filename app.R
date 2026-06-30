library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(jsonlite)

# ---- Data loading ----------------------------------------------------------
ames    <- read.csv("EDA_AmesHousing.csv",       stringsAsFactors = FALSE)
corolla <- read.csv("Nonlinear_ToyotaCorolla.csv", stringsAsFactors = FALSE)

ames    <- ames[complete.cases(ames[c("SalePrice", "Gr.Liv.Area", "Overall.Qual")]), ]
corolla <- corolla[complete.cases(corolla[c("Price", "Age_08_04")]) &
                   corolla$Price > 0 & corolla$Age_08_04 > 0, ]
corolla$Age_yr <- corolla$Age_08_04 / 12   # months → years (range 0.08–6.67)

# Embedded monthly average high temperatures for West Point, NY (°F)
monthly_temp <- data.frame(
  Month = 1:12,
  Label = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
  TempF = c(36, 40, 50, 62, 72, 80, 85, 83, 75, 64, 52, 40)
)

# Pre-sampled subsets for Tab 4 plotting performance
set.seed(7)
t4_ames    <- ames[sample(nrow(ames), min(400, nrow(ames))), ]
t4_corolla <- corolla[sample(nrow(corolla), min(400, nrow(corolla))), ]

# ---- Named variable list for Tab 1 -----------------------------------------
num_vars <- c(
  "Above Grade Living Area (sq ft)" = "Gr.Liv.Area",
  "Total Basement Area (sq ft)"     = "Total.Bsmt.SF",
  "Garage Area (sq ft)"             = "Garage.Area",
  "Year Built"                      = "Year.Built",
  "Overall Quality (1-10)"          = "Overall.Qual"
)

# ---- Sample answers (PDF only — never rendered in UI) ----------------------
sample_answers <- list(
  tab1 = list(
    "Descriptive. This question summarizes what already occurred in the existing dataset — it describes what we have rather than predicting anything about a new, unseen house.",
    "Predictive. The scatter sets up a relationship that could be used to predict sale price for a new house. The correlation coefficient r measures the strength: |r| near 1 means a strong linear relationship; r near 0 means little linear association.",
    "The selected variable (e.g., living area) is the independent variable — it is used to explain or predict sale price. Sale price is the dependent variable because its value depends on the independent variable.",
    "Example descriptive: 'What is the median sale price in each Ames neighborhood?' Example predictive: 'What will a 2,000 sq ft house sell for?' Descriptive questions summarize existing data; predictive questions forecast outcomes for new, unseen observations."
  ),
  tab2 = list(
    "At 70%, approximately 2,051 houses are in the training set and 879 are in the test set. Yes, 2,051 + 879 = 2,930 total.",
    "If we train and evaluate on the same data, the model appears more accurate than it really is — it has already 'memorized' those observations. The test set simulates new, unseen data so we get an honest measure of real-world performance.",
    "The colors (which specific houses are in each set) change when the seed changes, but the counts and percentages stay the same. The seed ensures reproducibility — two people using the same seed get the identical partition, critical for verifying each other's work.",
    "They should have similar shapes and centers. If very different, one partition is unrepresentative: the model trains on a skewed picture of prices and will perform poorly on the test set — giving a misleading view of accuracy."
  ),
  tab3 = list(
    "The model predicts the training set mean sale price for every house — the same value regardless of the house's features. That number minimizes total squared prediction error, making it the mathematically best constant guess.",
    "The training and test MAEs are typically around $57,000–$60,000 and are close to each other. The residual distribution (below) shows that most errors are concentrated near zero but some houses have errors well above $100,000 — the mean model systematically fails for very cheap or very expensive houses.",
    "As the training percentage increases, both MAEs stabilize slightly and converge. Because this model is extremely simple, the split percentage has little effect — training and test MAEs remain roughly equal across all splits.",
    "The mean model ignores every feature of a specific house: its size, age, location, condition, number of rooms, etc. Knowing square footage would let us predict that larger houses sell for more and smaller ones for less, reducing prediction error for houses far from the mean."
  ),
  tab4 = list(
    "The best linear fit is approximately m ≈ 110–120 (dollars per additional square foot) with b ≈ 10,000–20,000. R² ≈ 0.50–0.55 — the line captures the general upward trend but individual houses at the same size vary widely in price.",
    "At b = 1.0, the function becomes y = a + d, a constant — the car never depreciates, which clearly fits the data poorly. Reducing b below 1 restores the decay curve. The base b represents the fraction of depreciable value retained per year (e.g., b = 0.85 means the car keeps 85% of its value each year).",
    "With a = 0, the fit is a straight line that misses the steep rise at quality 9–10. Adding positive a (upward curvature) improves R² significantly — each additional quality point adds proportionally more value at the high end than at the low end, so a curved model is more appropriate than a straight line.",
    "The amplitude (a) controls the full seasonal swing: max temp ≈ a + d (summer peak) and min temp ≈ d - a (winter trough). The vertical shift (d) is the year-round mean temperature. Together they determine the full range of temperatures across all 12 months."
  )
)

# ---- RQ text strings -------------------------------------------------------
rq_text <- list(
  tab1 = c(
    "RQ 1. The histogram shows the distribution of the variable you selected. Is asking 'What is the average value of this variable across all Ames houses?' a descriptive or a predictive question? Why?",
    "RQ 2. Look at the scatter plot and the correlation coefficient r displayed on it. What does a value of r close to 1 or -1 tell you about the relationship? What does r near 0 mean?",
    "RQ 3. In the scatter plot, which variable is the independent variable and which is the dependent variable? How did you decide which is which?",
    "RQ 4. Using the Ames Housing data, write one descriptive question and one predictive question. How are these questions fundamentally different in what they are asking?"
  ),
  tab2 = c(
    "RQ 1. Set the training set to 70%. How many houses are in the training set? How many are in the test set? Do they add up to the total?",
    "RQ 2. Why do we need to hold back a test set? What problem would occur if we evaluated our model using the same data we used to build it?",
    "RQ 3. Change the random seed a few times. What changes on the plot? What stays the same? Why does specifying the seed matter when working with a partner?",
    "RQ 4. Look at the distribution comparison below the scatter. Do the training and test sets have similar shapes and centers? What could cause them to be very different, and why would that matter for your model?"
  ),
  tab3 = c(
    "RQ 1. Our baseline model predicts the same sale price for every house. What does it predict, and how was that number chosen?",
    "RQ 2. Look at the residual distribution below. Residuals are the differences between actual sale prices and the mean prediction. Where are most errors concentrated? What does the width of this distribution tell you about the model's accuracy?",
    "RQ 3. Adjust the training set percentage from 50% to 90%. How do the training MAE and test MAE change? At which split do the two errors seem most similar?",
    "RQ 4. What information about a specific house does the mean model completely ignore? Why might knowing a house's square footage allow us to make a better prediction?"
  ),
  tab4 = c(
    "RQ 1. Select 'Linear — House Price vs. Living Area.' Adjust the slope (m) and intercept (b) until the orange line fits the data as well as possible. What slope and intercept give you the highest R²? What does R² = 1.0 mean?",
    "RQ 2. Switch to 'Exponential — Car Price vs. Age.' Set the decay rate (b) to 1.00. What happens to the curve and to R²? Now reduce b below 1. What does the base b represent in terms of how a car loses value over time?",
    "RQ 3. Switch to 'Polynomial — House Price vs. Quality.' First set the curvature (a) to 0 — you now have a straight line. Does it fit the data well? Increase a above zero. How does adding curvature change R², and what does it reveal about how quality affects price?",
    "RQ 4. Switch to 'Trigonometric — Monthly Temperatures.' Which parameter controls the peak summer temperature? Which controls the coldest winter temperature? Adjust them to match the 12 data points as closely as possible."
  )
)

# ---- Helper: RQ box + textarea ---------------------------------------------
rq_block <- function(rq_id, rq_text_str) {
  tagList(
    div(class = "rq-box", rq_text_str),
    div(
      style = "margin: 0 0 16px 0;",
      textAreaInput(rq_id, label = NULL, value = "", rows = 3,
                    placeholder = "Type your response here...", width = "100%")
    )
  )
}

# ============================================================================
ui <- fluidPage(

  tags$head(
    # JS: receive base64 PDF bytes and trigger browser download
    tags$script(HTML("
      Shiny.addCustomMessageHandler('trigger_download', function(msg) {
        var binaryString = atob(msg.b64);
        var bytes = new Uint8Array(binaryString.length);
        for (var i = 0; i < binaryString.length; i++) {
          bytes[i] = binaryString.charCodeAt(i);
        }
        var blob = new Blob([bytes], { type: msg.mime });
        var url  = URL.createObjectURL(blob);
        var a    = document.createElement('a');
        a.href = url; a.download = msg.filename;
        document.body.appendChild(a); a.click();
        document.body.removeChild(a);
        setTimeout(function() { URL.revokeObjectURL(url); }, 2000);
      });
    ")),
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 14px; }
      h2 { font-size: 20px; font-weight: 600; margin-top: 0; }
      h4 { font-size: 15px; font-weight: 600; color: #333; margin-top: 18px; margin-bottom: 4px; }
      .well { background: #f7f7f7; border: 1px solid #e0e0e0; border-radius: 6px; padding: 14px; }
      .rq-box { background: #eef4fb; border-left: 4px solid #2c6fad; padding: 10px 14px;
                 border-radius: 0 6px 6px 0; margin-bottom: 6px; font-size: 13px; }
      .stat-box { background: #fff; border: 1px solid #ddd; border-radius: 6px;
                  padding: 10px 14px; margin-bottom: 8px; }
      .stat-label { font-size: 12px; color: #777; margin-bottom: 2px; }
      .stat-value { font-size: 22px; font-weight: 600; color: #2c6fad; }
      .nav-tabs > li > a { font-size: 13px; }
      textarea.form-control { border-radius: 4px; border: 1px solid #c8d8e8;
                               background: #f9fbfd; font-size: 13px; }
      .context-box { background: #f0f7ee; border-left: 4px solid #2ca05a; padding: 8px 14px;
                     border-radius: 0 6px 6px 0; margin-bottom: 12px; font-size: 13px; color: #333; }
    "))
  ),

  titlePanel("Intro to Predictive Modeling — Ames Housing & Toyota Corolla"),

  tabsetPanel(

    # ---- TAB 1: Types of Questions -----------------------------------------
    tabPanel("1. Types of Questions",
      sidebarLayout(
        sidebarPanel(
          h4("Choose a Variable"),
          selectInput("tab1_var", "Independent Variable:",
                      choices = num_vars, selected = "Gr.Liv.Area"),
          hr(),
          div(class = "stat-box",
            div(class = "stat-label", "Total Houses"),
            div(class = "stat-value", comma(nrow(ames)))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Median Sale Price"),
            div(class = "stat-value", dollar(median(ames$SalePrice)))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Correlation with Sale Price (r)"),
            div(class = "stat-value", textOutput("tab1_cor", inline = TRUE))
          )
        ),
        mainPanel(
          h2("Descriptive vs. Predictive Questions"),
          fluidRow(
            column(6, strong("Descriptive — What happened?"),
                      plotOutput("tab1_hist", height = "260px")),
            column(6, strong("Predictive — What will happen?"),
                      plotOutput("tab1_scatter", height = "260px"))
          ),
          br(),
          rq_block("rq_1_1", rq_text$tab1[1]),
          rq_block("rq_1_2", rq_text$tab1[2]),
          rq_block("rq_1_3", rq_text$tab1[3]),
          rq_block("rq_1_4", rq_text$tab1[4])
        )
      )
    ),

    # ---- TAB 2: Training & Test Sets ---------------------------------------
    tabPanel("2. Training & Test Sets",
      sidebarLayout(
        sidebarPanel(
          h4("Partition Controls"),
          sliderInput("split_pct",  "Training Set (% of data):",
                      min = 50, max = 90, value = 70, step = 5),
          sliderInput("split_seed", "Random Seed:",
                      min = 1, max = 200, value = 42, step = 1),
          hr(),
          div(class = "stat-box",
            div(class = "stat-label", "Training Set — # of Houses"),
            div(class = "stat-value", textOutput("n_train", inline = TRUE))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Test Set — # of Houses"),
            div(class = "stat-value", textOutput("n_test", inline = TRUE))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Training Mean Sale Price"),
            div(class = "stat-value", textOutput("train_mean_sp", inline = TRUE))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Test Mean Sale Price"),
            div(class = "stat-value", textOutput("test_mean_sp", inline = TRUE))
          )
        ),
        mainPanel(
          h2("Partitioning Data into Training and Test Sets"),
          plotOutput("tab2_plot", height = "300px"),
          plotOutput("tab2_dist",  height = "180px"),
          br(),
          rq_block("rq_2_1", rq_text$tab2[1]),
          rq_block("rq_2_2", rq_text$tab2[2]),
          rq_block("rq_2_3", rq_text$tab2[3]),
          rq_block("rq_2_4", rq_text$tab2[4])
        )
      )
    ),

    # ---- TAB 3: Mean Baseline Model ----------------------------------------
    tabPanel("3. Baseline Model (Mean)",
      sidebarLayout(
        sidebarPanel(
          h4("Partition Controls"),
          sliderInput("m_split_pct", "Training Set (% of data):",
                      min = 50, max = 90, value = 70, step = 5),
          sliderInput("m_seed",      "Random Seed:",
                      min = 1, max = 200, value = 42, step = 1),
          hr(),
          div(class = "stat-box",
            div(class = "stat-label", "Model Prediction (every house)"),
            div(class = "stat-value", textOutput("model_pred", inline = TRUE))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Training MAE"),
            div(class = "stat-value", textOutput("train_mae", inline = TRUE))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Test MAE"),
            div(class = "stat-value", textOutput("test_mae", inline = TRUE))
          )
        ),
        mainPanel(
          h2("A Simple Predictive Model: Predict Using the Mean"),
          plotOutput("tab3_plot",  height = "290px"),
          plotOutput("tab3_resid", height = "180px"),
          br(),
          rq_block("rq_3_1", rq_text$tab3[1]),
          rq_block("rq_3_2", rq_text$tab3[2]),
          rq_block("rq_3_3", rq_text$tab3[3]),
          rq_block("rq_3_4", rq_text$tab3[4])
        )
      )
    ),

    # ---- TAB 4: Families of Functions (data-fitting) -----------------------
    tabPanel("4. Families of Functions",
      sidebarLayout(
        sidebarPanel(
          h4("Choose a Scenario"),
          selectInput("fn_scenario", "Data & Function Family:",
            choices = c(
              "Linear — House Price vs. Living Area"  = "linear",
              "Exponential — Car Price vs. Age"        = "exponential",
              "Polynomial — House Price vs. Quality"   = "polynomial",
              "Trigonometric — Monthly Temperatures"   = "trig"
            )
          ),
          hr(),
          h4("Adjust Parameters"),

          # Linear sliders
          conditionalPanel("input.fn_scenario == 'linear'",
            sliderInput("lin_m", "Slope (m, $ per sq ft):",
                        min = -50, max = 250, value = 110, step = 5),
            sliderInput("lin_b", "Intercept (b, $):",
                        min = -100000, max = 150000, value = 15000, step = 5000)
          ),
          # Exponential sliders
          conditionalPanel("input.fn_scenario == 'exponential'",
            sliderInput("exp_a", "Scale (a, starting price €):",
                        min = 5000, max = 30000, value = 18000, step = 500),
            sliderInput("exp_b", "Decay Rate (b, base):",
                        min = 0.50, max = 1.00, value = 0.82, step = 0.01),
            sliderInput("exp_d", "Floor Value (d, €):",
                        min = 0, max = 5000, value = 2000, step = 250)
          ),
          # Polynomial sliders
          conditionalPanel("input.fn_scenario == 'polynomial'",
            sliderInput("poly_a", "Curvature (a, $ per quality²):",
                        min = -3000, max = 10000, value = 4000, step = 200),
            sliderInput("poly_b", "Slope (b, $ per quality):",
                        min = -40000, max = 20000, value = -5000, step = 1000),
            sliderInput("poly_c", "Intercept (c, $):",
                        min = 0, max = 150000, value = 63000, step = 3000)
          ),
          # Trigonometric sliders
          conditionalPanel("input.fn_scenario == 'trig'",
            sliderInput("trig_a", "Amplitude (a, °F):",
                        min = 0, max = 40, value = 24, step = 1),
            sliderInput("trig_b", "Frequency (b):",
                        min = 0.10, max = 1.50, value = 0.52, step = 0.02),
            sliderInput("trig_c", "Horizontal Shift (c, months):",
                        min = -5, max = 10, value = 4.0, step = 0.5),
            sliderInput("trig_d", "Vertical Shift (d, °F):",
                        min = 30, max = 80, value = 54, step = 1)
          ),

          hr(),
          div(class = "stat-box",
            div(class = "stat-label", "Equation"),
            div(style = "font-size:13px; font-weight:600; color:#2c6fad; word-break:break-word;",
                textOutput("fn_equation"))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "R² (Goodness of Fit)"),
            div(class = "stat-value", textOutput("tab4_rsq", inline = TRUE))
          ),
          div(style = "font-size:11px; color:#777; margin-top:-4px; margin-bottom:8px;",
              "0 = no fit  |  1 = perfect fit")
        ),
        mainPanel(
          h2("Fitting a Function to Real Data"),
          uiOutput("tab4_context"),
          plotOutput("tab4_plot", height = "380px"),
          br(),
          rq_block("rq_4_1", rq_text$tab4[1]),
          rq_block("rq_4_2", rq_text$tab4[2]),
          rq_block("rq_4_3", rq_text$tab4[3]),
          rq_block("rq_4_4", rq_text$tab4[4])
        )
      )
    ),

    # ---- TAB 5: Export -----------------------------------------------------
    tabPanel("5. Export Responses",
      fluidPage(
        br(),
        h2("Export Your Responses to PDF"),
        p("Complete the research questions in Tabs 1–4, then click the button below."),
        wellPanel(
          fluidRow(
            column(5,
              h4("Your Information (optional)"),
              textInput("student_name", "Name:",    placeholder = "Last, First"),
              textInput("section",      "Section:", placeholder = "e.g., A1")
            ),
            column(4,
              h4("Progress"),
              uiOutput("completion_summary")
            ),
            column(3,
              h4("Download"),
              br(),
              actionButton("export_btn", "Generate & Download PDF",
                           class = "btn-primary",
                           style = "width:100%; font-size:14px; white-space:normal;"),
              br(), br(),
              uiOutput("export_status")
            )
          )
        ),
        div(style = "color:#555; font-size:13px; background:#fff8e1;
                     border-left:4px solid #f0ad4e; padding:10px 14px;
                     border-radius:0 6px 6px 0; margin-top:8px;",
          strong("Note:"), " Sample answers are not visible anywhere in this app.",
          " They appear only in the downloaded PDF, printed after your typed responses.")
      )
    )
  )
)

# ============================================================================
server <- function(input, output, session) {

  # ---- Partitions (Tabs 2 & 3) --------------------------------------------
  part2 <- reactive({
    set.seed(input$split_seed)
    n   <- nrow(ames)
    idx <- sample(n, size = floor(input$split_pct / 100 * n))
    list(train = ames[idx, ], test = ames[-idx, ])
  })
  part3 <- reactive({
    set.seed(input$m_seed)
    n   <- nrow(ames)
    idx <- sample(n, size = floor(input$m_split_pct / 100 * n))
    list(train = ames[idx, ], test = ames[-idx, ])
  })
  train_mean <- reactive(mean(part3()$train$SalePrice))

  # ================================================================
  # TAB 1
  # ================================================================
  tab1_df <- reactive({
    var <- input$tab1_var
    ames[!is.na(ames[[var]]), ]
  })

  output$tab1_cor <- renderText({
    var <- input$tab1_var
    df  <- tab1_df()
    round(cor(df[[var]], df$SalePrice, use = "complete.obs"), 2)
  })

  plt_tab1_hist <- reactive({
    var   <- input$tab1_var
    label <- names(num_vars)[num_vars == var]
    ggplot(tab1_df(), aes(x = .data[[var]])) +
      geom_histogram(fill = "#2c6fad", color = "white", bins = 30) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      labs(x = label, y = "Count", title = paste("Distribution of", label)) +
      theme_minimal(base_size = 13)
  })

  plt_tab1_scatter <- reactive({
    var   <- input$tab1_var
    label <- names(num_vars)[num_vars == var]
    df    <- tab1_df()
    r_val <- round(cor(df[[var]], df$SalePrice, use = "complete.obs"), 2)
    ggplot(df, aes(x = .data[[var]], y = SalePrice)) +
      geom_point(color = "#2c6fad", alpha = 0.25, size = 1.3) +
      geom_hline(yintercept = mean(df$SalePrice), color = "#2ca05a",
                 linetype = "dashed", linewidth = 0.8) +
      annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.8,
               label = paste0("r = ", r_val),
               color = "#e05c2a", fontface = "bold", size = 4.5) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = dollar) +
      labs(x = label, y = "Sale Price ($)",
           title = paste("Sale Price vs.", label),
           caption = "Green dashed line = mean sale price") +
      theme_minimal(base_size = 13)
  })

  output$tab1_hist    <- renderPlot({ plt_tab1_hist() })
  output$tab1_scatter <- renderPlot({ plt_tab1_scatter() })

  # ================================================================
  # TAB 2
  # ================================================================
  output$n_train       <- renderText(comma(nrow(part2()$train)))
  output$n_test        <- renderText(comma(nrow(part2()$test)))
  output$train_mean_sp <- renderText(dollar(mean(part2()$train$SalePrice)))
  output$test_mean_sp  <- renderText(dollar(mean(part2()$test$SalePrice)))

  plt_tab2 <- reactive({
    p  <- part2()
    tr <- p$train; tr$Set <- "Training"
    te <- p$test;  te$Set <- "Test"
    combined <- rbind(tr, te)
    ggplot(combined, aes(x = Gr.Liv.Area, y = SalePrice, color = Set)) +
      geom_point(alpha = 0.35, size = 1.4) +
      scale_color_manual(values = c("Training" = "#2c6fad", "Test" = "#e05c2a")) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = dollar) +
      labs(x = "Above Grade Living Area (sq ft)", y = "Sale Price ($)",
           title = "Sale Price vs. Living Area — Training vs. Test Split",
           color = "Set") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  plt_tab2_dist <- reactive({
    p  <- part2()
    tr <- p$train; tr$Set <- "Training"
    te <- p$test;  te$Set <- "Test"
    combined <- rbind(tr, te)
    ggplot(combined, aes(x = SalePrice, color = Set, fill = Set)) +
      geom_density(alpha = 0.25, linewidth = 0.9) +
      scale_color_manual(values = c("Training" = "#2c6fad", "Test" = "#e05c2a")) +
      scale_fill_manual(values  = c("Training" = "#2c6fad", "Test" = "#e05c2a")) +
      scale_x_continuous(labels = dollar) +
      labs(x = "Sale Price ($)", y = "Density",
           title = "Sale Price Distribution: Training vs. Test",
           color = "Set", fill = "Set") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", plot.title = element_text(size = 13))
  })

  output$tab2_plot <- renderPlot({ plt_tab2() })
  output$tab2_dist <- renderPlot({ plt_tab2_dist() })

  # ================================================================
  # TAB 3
  # ================================================================
  output$model_pred <- renderText(dollar(train_mean()))
  output$train_mae  <- renderText({
    tr <- part3()$train
    dollar(mean(abs(tr$SalePrice - train_mean())))
  })
  output$test_mae   <- renderText({
    p <- part3()
    dollar(mean(abs(p$test$SalePrice - train_mean())))
  })

  plt_tab3 <- reactive({
    p    <- part3()
    pred <- train_mean()
    tr   <- p$train; tr$Set <- "Training"
    te   <- p$test;  te$Set <- "Test"
    combined <- rbind(tr, te)
    ggplot(combined, aes(x = SalePrice, fill = Set)) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 40, color = "white") +
      geom_vline(xintercept = pred, color = "#e05c2a", linewidth = 1.5, linetype = "dashed") +
      annotate("text", x = pred * 1.04, y = Inf, vjust = 1.8,
               label = paste0("Prediction:\n", dollar(round(pred))),
               color = "#e05c2a", hjust = 0, size = 3.8) +
      scale_fill_manual(values = c("Training" = "#2c6fad", "Test" = "#2ca05a")) +
      scale_x_continuous(labels = dollar) +
      scale_y_continuous(labels = comma) +
      labs(x = "Sale Price ($)", y = "Count",
           title = "Sale Price Distribution with Mean Baseline Prediction",
           fill = "Set") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  plt_tab3_resid <- reactive({
    p    <- part3()
    pred <- train_mean()
    tr   <- p$train; tr$Set <- "Training"
    te   <- p$test;  te$Set <- "Test"
    combined <- rbind(tr, te)
    combined$Residual <- combined$SalePrice - pred
    ggplot(combined, aes(x = Residual, color = Set, fill = Set)) +
      geom_density(alpha = 0.25, linewidth = 0.9) +
      geom_vline(xintercept = 0, color = "#333", linetype = "dashed", linewidth = 0.8) +
      scale_color_manual(values = c("Training" = "#2c6fad", "Test" = "#2ca05a")) +
      scale_fill_manual(values  = c("Training" = "#2c6fad", "Test" = "#2ca05a")) +
      scale_x_continuous(labels = dollar) +
      labs(x = "Residual: Actual − Predicted ($)", y = "Density",
           title = "Residual Distribution (how far off is each prediction?)",
           color = "Set", fill = "Set") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", plot.title = element_text(size = 13))
  })

  output$tab3_plot  <- renderPlot({ plt_tab3() })
  output$tab3_resid <- renderPlot({ plt_tab3_resid() })

  # ================================================================
  # TAB 4 — Function fitting
  # ================================================================

  # Scenario context blurb
  output$tab4_context <- renderUI({
    txt <- switch(input$fn_scenario,
      "linear"      = "Ames Housing (400 houses sampled). Adjust the orange line's slope and intercept to follow the data trend. Watch R² — higher is a better fit.",
      "exponential" = "Toyota Corolla listings (400 sampled). Adjust the curve to match how car prices drop with age. Exponential decay means the steepest price drop happens in the first few years.",
      "polynomial"  = paste0("All ", comma(nrow(ames)), " Ames houses by quality rating (1–10). A straight line (a = 0) fits the low end but misses the steep rise at high quality. Try adding curvature."),
      "trig"        = "Average monthly high temperatures at West Point, NY (12 data points). The sine function naturally models seasonal patterns — one full cycle per year."
    )
    div(class = "context-box", txt)
  })

  # Equation label
  output$fn_equation <- renderText({
    switch(input$fn_scenario,
      "linear"      = paste0("y = ", input$lin_m, "x + ", input$lin_b),
      "exponential" = paste0("y = ", input$exp_a, " x ", input$exp_b, "^x + ", input$exp_d),
      "polynomial"  = paste0("y = ", input$poly_a, "x² + ", input$poly_b, "x + ", input$poly_c),
      "trig"        = paste0("y = ", input$trig_a, " x sin(", input$trig_b,
                             "(x − ", input$trig_c, ")) + ", input$trig_d)
    )
  })

  # R² reactive
  tab4_rsq <- reactive({
    s <- input$fn_scenario
    if (s == "linear") {
      y_act  <- t4_ames$SalePrice
      y_hat  <- input$lin_m * t4_ames$Gr.Liv.Area + input$lin_b
    } else if (s == "exponential") {
      y_act  <- t4_corolla$Price
      y_hat  <- input$exp_a * input$exp_b^t4_corolla$Age_yr + input$exp_d
    } else if (s == "polynomial") {
      y_act  <- ames$SalePrice
      y_hat  <- input$poly_a * ames$Overall.Qual^2 +
                input$poly_b * ames$Overall.Qual +
                input$poly_c
    } else {
      y_act  <- monthly_temp$TempF
      y_hat  <- input$trig_a * sin(input$trig_b * (monthly_temp$Month - input$trig_c)) +
                input$trig_d
    }
    ss_res <- sum((y_act - y_hat)^2)
    ss_tot <- sum((y_act - mean(y_act))^2)
    round(max(0, 1 - ss_res / ss_tot), 3)
  })

  output$tab4_rsq <- renderText({ tab4_rsq() })

  # Main plot
  plt_tab4 <- reactive({
    s <- input$fn_scenario

    if (s == "linear") {
      x_seq <- seq(min(t4_ames$Gr.Liv.Area), max(t4_ames$Gr.Liv.Area), length.out = 300)
      df_f  <- data.frame(x = x_seq, y = input$lin_m * x_seq + input$lin_b)
      ggplot(t4_ames, aes(x = Gr.Liv.Area, y = SalePrice)) +
        geom_point(color = "#2c6fad", alpha = 0.3, size = 1.5) +
        geom_line(data = df_f, aes(x = x, y = y), color = "#e05c2a", linewidth = 1.5) +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = dollar) +
        labs(x = "Above Grade Living Area (sq ft)", y = "Sale Price ($)",
             title = paste0("Linear Fit   |   R² = ", tab4_rsq())) +
        theme_minimal(base_size = 13)

    } else if (s == "exponential") {
      x_seq <- seq(0, max(t4_corolla$Age_yr) + 0.3, length.out = 300)
      y_seq <- input$exp_a * input$exp_b^x_seq + input$exp_d
      df_f  <- data.frame(x = x_seq, y = y_seq)
      df_f  <- df_f[df_f$y >= 0, ]
      ggplot(t4_corolla, aes(x = Age_yr, y = Price)) +
        geom_point(color = "#2c6fad", alpha = 0.35, size = 1.5) +
        geom_line(data = df_f, aes(x = x, y = y), color = "#e05c2a", linewidth = 1.5) +
        scale_y_continuous(labels = comma) +
        labs(x = "Car Age (years)", y = "Price (€)",
             title = paste0("Exponential Fit   |   R² = ", tab4_rsq())) +
        theme_minimal(base_size = 13)

    } else if (s == "polynomial") {
      x_seq <- seq(1, 10, by = 0.05)
      y_seq <- input$poly_a * x_seq^2 + input$poly_b * x_seq + input$poly_c
      df_f  <- data.frame(x = x_seq, y = y_seq)
      ggplot(ames, aes(x = Overall.Qual, y = SalePrice)) +
        geom_jitter(color = "#2c6fad", alpha = 0.18, size = 1.2, width = 0.2, height = 0) +
        geom_line(data = df_f, aes(x = x, y = y), color = "#e05c2a", linewidth = 1.8) +
        scale_x_continuous(breaks = 1:10) +
        scale_y_continuous(labels = dollar) +
        labs(x = "Overall Quality Rating (1 = poor, 10 = excellent)", y = "Sale Price ($)",
             title = paste0("Polynomial Fit   |   R² = ", tab4_rsq())) +
        theme_minimal(base_size = 13)

    } else {  # trig
      x_seq <- seq(1, 12, by = 0.05)
      y_seq <- input$trig_a * sin(input$trig_b * (x_seq - input$trig_c)) + input$trig_d
      df_f  <- data.frame(x = x_seq, y = y_seq)
      ggplot(monthly_temp, aes(x = Month, y = TempF)) +
        geom_point(color = "#2c6fad", size = 5) +
        geom_line(data = df_f, aes(x = x, y = y), color = "#e05c2a", linewidth = 1.5) +
        scale_x_continuous(breaks = 1:12, labels = monthly_temp$Label) +
        labs(x = "Month", y = "Avg High Temperature (°F)",
             title = paste0("Trigonometric Fit   |   R² = ", tab4_rsq())) +
        theme_minimal(base_size = 13)
    }
  })

  output$tab4_plot <- renderPlot({ plt_tab4() })

  # ================================================================
  # TAB 5 — Completion counter & PDF export
  # ================================================================
  output$completion_summary <- renderUI({
    all_ids <- paste0("rq_", rep(1:4, each = 4), "_", rep(1:4, times = 4))
    filled  <- sum(sapply(all_ids, function(id) {
      v <- input[[id]]; !is.null(v) && nchar(trimws(v)) > 0
    }))
    div(class = "stat-box",
      div(class = "stat-label", "Questions Answered"),
      div(class = "stat-value", paste0(filled, " / 16"))
    )
  })

  export_msg <- reactiveVal("")
  output$export_status <- renderUI({
    msg <- export_msg()
    if (nchar(msg) == 0) return(NULL)
    div(style = "color:#2c6fad; font-size:13px; margin-top:4px;", msg)
  })

  observeEvent(input$export_btn, {
    export_msg("Building PDF...")

    write_block <- function(txt, x, y, width = 84, cex = 0.82,
                             col = "black", font = 1, lh = 0.043) {
      for (ln in strwrap(txt, width = width)) {
        text(x, y, ln, adj = 0, cex = cex, col = col, font = font); y <- y - lh
      }
      y - 0.008
    }
    new_page <- function() {
      plot.new()
      par(mar = c(0.3, 0.3, 0.3, 0.3))
      plot.window(xlim = c(0, 1), ylim = c(0, 1))
    }
    section_header <- function(title) {
      new_page()
      text(0.5, 0.975, "MA153X — Lesson 7: Intro to Predictive Modeling",
           adj = 0.5, cex = 0.72, col = "gray50")
      text(0.5, 0.940, title, adj = 0.5, cex = 1.22, font = 2)
      0.895
    }
    qa_pages <- function(tab_title, qs, rs, ans) {
      y <- section_header(tab_title)
      for (i in seq_along(qs)) {
        if (y < 0.20) { y <- section_header(paste0(tab_title, " (cont.)")) }
        y <- write_block(qs[i], 0.02, y, cex = 0.84, col = "#1a3a6e", font = 2, lh = 0.043)
        resp <- if (nchar(trimws(rs[i])) == 0) "(No response entered.)" else rs[i]
        y <- write_block(paste0("Your response: ", resp),     0.04, y, cex = 0.79, col = "#222222", lh = 0.039)
        y <- write_block(paste0("Sample answer: ", ans[[i]]), 0.04, y, cex = 0.79, col = "#1a5c2a", lh = 0.039)
        y <- y - 0.022
      }
    }

    rs1 <- c(input$rq_1_1, input$rq_1_2, input$rq_1_3, input$rq_1_4)
    rs2 <- c(input$rq_2_1, input$rq_2_2, input$rq_2_3, input$rq_2_4)
    rs3 <- c(input$rq_3_1, input$rq_3_2, input$rq_3_3, input$rq_3_4)
    rs4 <- c(input$rq_4_1, input$rq_4_2, input$rq_4_3, input$rq_4_4)

    tmp <- tempfile(fileext = ".pdf")
    tryCatch({
      pdf(tmp, width = 8.5, height = 11, title = "Lesson 7 Responses")

      # Cover
      new_page()
      text(0.5, 0.72, "MA153X Data-Driven Modeling",            adj = 0.5, cex = 1.9, font = 2)
      text(0.5, 0.63, "Lesson 7: Intro to Predictive Modeling", adj = 0.5, cex = 1.4)
      text(0.5, 0.55, "Research Questions & Responses",          adj = 0.5, cex = 1.15)
      nm <- trimws(input$student_name); sc <- trimws(input$section)
      if (nchar(nm) > 0) text(0.5, 0.44, paste("Student:", nm), adj = 0.5, cex = 1.0)
      if (nchar(sc) > 0) text(0.5, 0.37, paste("Section:", sc), adj = 0.5, cex = 1.0)
      text(0.5, 0.28, paste("Date:", format(Sys.Date(), "%d %B %Y")), adj = 0.5, cex = 1.0)

      qa_pages("Tab 1: Types of Questions",    rq_text$tab1, rs1, sample_answers$tab1)
      print(plt_tab1_hist()); print(plt_tab1_scatter())

      qa_pages("Tab 2: Training & Test Sets",  rq_text$tab2, rs2, sample_answers$tab2)
      print(plt_tab2()); print(plt_tab2_dist())

      qa_pages("Tab 3: Baseline Model (Mean)", rq_text$tab3, rs3, sample_answers$tab3)
      print(plt_tab3()); print(plt_tab3_resid())

      qa_pages("Tab 4: Families of Functions", rq_text$tab4, rs4, sample_answers$tab4)
      print(plt_tab4())

      dev.off()

      pdf_bytes <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
      b64       <- gsub("\n", "", jsonlite::base64_enc(pdf_bytes))
      nm_clean  <- trimws(input$student_name)
      filename  <- if (nchar(nm_clean) > 0)
        paste0("LSN7_", gsub("[^A-Za-z0-9]", "_", nm_clean), ".pdf")
      else "LSN7_Responses.pdf"

      session$sendCustomMessage("trigger_download",
        list(b64 = b64, filename = filename, mime = "application/pdf"))
      export_msg("Done! Check your downloads folder.")

    }, error = function(e) {
      try(dev.off(), silent = TRUE)
      export_msg(paste("Error:", conditionMessage(e)))
    })
  })
}

shinyApp(ui, server)
