library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(jsonlite)

# Load and clean data
ames <- read.csv("EDA_AmesHousing.csv", stringsAsFactors = FALSE)
ames <- ames[complete.cases(ames[c("SalePrice", "Gr.Liv.Area")]), ]

num_vars <- c(
  "Above Grade Living Area (sq ft)" = "Gr.Liv.Area",
  "Total Basement Area (sq ft)"     = "Total.Bsmt.SF",
  "Garage Area (sq ft)"             = "Garage.Area",
  "Year Built"                      = "Year.Built",
  "Overall Quality (1-10)"          = "Overall.Qual"
)

# Sample answers — not displayed in the app; included only in the PDF export
sample_answers <- list(
  tab1 = list(
    "Descriptive. This question summarizes what already occurred in the existing Ames dataset — it describes the data we have rather than making a prediction about a new, unseen house.",
    "Predictive. The scatter plot sets up a relationship between the independent variable and sale price that we could use to predict the price of a new house not yet in the dataset.",
    "The selected variable (e.g., Above Grade Living Area) is the independent variable because it is used to explain or predict sale price. Sale price is the dependent variable because its value depends on the independent variable.",
    "Example descriptive: 'What is the median sale price in each Ames neighborhood?' Example predictive: 'What will a 2,000 sq ft house sell for?' Descriptive questions summarize existing data; predictive questions forecast outcomes for new, unseen observations."
  ),
  tab2 = list(
    "At 70%, approximately 2,051 houses are in the training set and 879 are in the test set. Yes, 2,051 + 879 = 2,930 (the full dataset after removing rows with missing values).",
    "If we train and evaluate on the same data, the model appears more accurate than it really is — it has already 'memorized' those observations. The test set simulates new, unseen data so we get an honest measure of real-world performance.",
    "The colors (which specific houses are in each set) change when the seed changes, but the counts and percentages stay the same. The seed ensures reproducibility — two people using the same seed get the identical partition, which is critical for verifying each other's work.",
    "They should be close. If very different, one partition is unrepresentative: the model may train on unusually cheap or expensive houses and perform poorly on the test set, giving a misleading picture of accuracy."
  ),
  tab3 = list(
    "The model predicts the training set mean sale price for every house — the same value regardless of the house. That number is used because it is the single constant that minimizes total squared prediction error, making it the mathematically best constant guess.",
    "The training MAE and test MAE are typically around $57,000–$60,000 and are close to each other. They are similar because the mean model is too simple to overfit — it uses no individual house features.",
    "As the training percentage increases, both MAEs stabilize slightly and converge. Because this model is extremely simple, the split percentage has little effect — training and test MAEs remain roughly equal across all splits.",
    "The mean model ignores every feature of a specific house: its size, age, location, condition, number of rooms, etc. Knowing square footage would let us predict that larger houses sell for more and smaller ones for less, reducing prediction error for houses far from the mean."
  ),
  tab4 = list(
    "As m increases, the line tilts more steeply upward. At m = 0, the line is flat (horizontal). A negative slope tilts the line downward from left to right, showing that y decreases as x increases.",
    "When b = 0.5 (between 0 and 1), the function converges toward the horizontal asymptote y = d as x grows — this is exponential decay. When b = 2 (greater than 1), it diverges away — this is exponential growth. The base b controls both the direction and rate of change.",
    "A degree-2 polynomial bends at most once (one local extremum). Degree 3 can bend at most twice; degree 4 at most three times. In general, a degree-n polynomial has at most n - 1 direction changes (local extrema).",
    "The amplitude (a) controls the height of the peaks — larger a means taller peaks. The frequency (b) controls how many complete cycles fit in the graph window — larger b compresses the cycles, showing more of them."
  )
)

# RQ strings (shared between UI and PDF handler)
rq_text <- list(
  tab1 = c(
    "RQ 1. The histogram shows the distribution of the variable you selected. Is asking 'What is the average value of this variable across all Ames houses?' a descriptive or a predictive question? Why?",
    "RQ 2. The scatter plot shows sale price vs. your selected variable. What type of question does this plot set up: descriptive or predictive? Explain your reasoning.",
    "RQ 3. In the scatter plot, which variable is the independent variable and which is the dependent variable? How did you decide which is which?",
    "RQ 4. Using the Ames Housing data, write one descriptive question and one predictive question. How are these questions fundamentally different in what they are asking?"
  ),
  tab2 = c(
    "RQ 1. Set the training set to 70%. How many houses are in the training set? How many are in the test set? Do they add up to the total number of houses?",
    "RQ 2. Why do we need to hold back a test set? What problem would occur if we evaluated our model using the same data we used to build it?",
    "RQ 3. Change the random seed a few times. What changes on the plot? What stays the same? Why does specifying the seed matter when working with a partner?",
    "RQ 4. Compare the mean sale price of the training and test sets in the sidebar. Are they close to each other? Why is it important that the two sets have similar distributions?"
  ),
  tab3 = c(
    "RQ 1. Our baseline model predicts the same sale price for every house. What does it predict, and how was that number chosen?",
    "RQ 2. Mean Absolute Error (MAE) is the average dollar amount our predictions are off. What is the training MAE? What is the test MAE? Are they close to each other?",
    "RQ 3. Adjust the training set percentage from 50% to 90%. How does the training MAE and test MAE change? At which split do the two errors seem most similar?",
    "RQ 4. What information about a specific house does the mean model completely ignore? Why might knowing a house's square footage allow us to make a better prediction?"
  ),
  tab4 = c(
    "RQ 1. Select the Linear function. Adjust the slope (m). What happens to the line as m increases from 0? What does a negative slope look like compared to a positive slope?",
    "RQ 2. Switch to Exponential. Set a = 1 and d = 0, then compare b = 0.5 vs. b = 2. How does the base (b) control whether the function grows or decays as x increases?",
    "RQ 3. Select Polynomial with degree 2. How many times does the curve change direction? Increase the degree to 3, then 4. Based on the pattern, what is the maximum number of direction changes for a degree-n polynomial?",
    "RQ 4. Switch to Trigonometric. Which parameter controls the height of the peaks (amplitude)? Which controls how many full cycles appear in the graph window?"
  )
)

# Helper: RQ box + textarea
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

# ==========================================================================
ui <- fluidPage(

  # JavaScript: receive base64-encoded file from R and trigger browser download
  tags$head(
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
        a.href     = url;
        a.download = msg.filename;
        document.body.appendChild(a);
        a.click();
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
    "))
  ),

  titlePanel("Intro to Predictive Modeling — Ames Housing Data"),

  tabsetPanel(

    # ---- TAB 1: Types of Questions ---------------------------------------
    tabPanel("1. Types of Questions",
      sidebarLayout(
        sidebarPanel(
          h4("Choose a Variable"),
          selectInput("tab1_var", "Independent Variable:",
                      choices = num_vars, selected = "Gr.Liv.Area"),
          hr(),
          div(class = "stat-box",
            div(class = "stat-label", "Total Houses in Dataset"),
            div(class = "stat-value", comma(nrow(ames)))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Dependent Variable"),
            div(class = "stat-value", "Sale Price ($)")
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Median Sale Price"),
            div(class = "stat-value", dollar(median(ames$SalePrice)))
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

    # ---- TAB 2: Training & Test Sets -------------------------------------
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
            div(class = "stat-label", "Training Set — Mean Sale Price"),
            div(class = "stat-value", textOutput("train_mean_sp", inline = TRUE))
          ),
          div(class = "stat-box",
            div(class = "stat-label", "Test Set — Mean Sale Price"),
            div(class = "stat-value", textOutput("test_mean_sp", inline = TRUE))
          )
        ),
        mainPanel(
          h2("Partitioning Data into Training and Test Sets"),
          plotOutput("tab2_plot", height = "360px"),
          br(),
          rq_block("rq_2_1", rq_text$tab2[1]),
          rq_block("rq_2_2", rq_text$tab2[2]),
          rq_block("rq_2_3", rq_text$tab2[3]),
          rq_block("rq_2_4", rq_text$tab2[4])
        )
      )
    ),

    # ---- TAB 3: Mean Baseline Model --------------------------------------
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
          plotOutput("tab3_plot", height = "360px"),
          br(),
          rq_block("rq_3_1", rq_text$tab3[1]),
          rq_block("rq_3_2", rq_text$tab3[2]),
          rq_block("rq_3_3", rq_text$tab3[3]),
          rq_block("rq_3_4", rq_text$tab3[4])
        )
      )
    ),

    # ---- TAB 4: Families of Functions ------------------------------------
    tabPanel("4. Families of Functions",
      sidebarLayout(
        sidebarPanel(
          h4("Function Explorer"),
          selectInput("fn_family", "Select a Function Family:",
                      choices = c("Linear", "Exponential", "Polynomial", "Trigonometric")),
          conditionalPanel("input.fn_family == 'Linear'",
            sliderInput("lin_m", "Slope (m):",        min = -3, max = 3, value = 1,  step = 0.25),
            sliderInput("lin_b", "y-Intercept (b):",  min = -8, max = 8, value = 0,  step = 0.5)
          ),
          conditionalPanel("input.fn_family == 'Exponential'",
            sliderInput("exp_a", "Vertical Stretch (a):", min = -4, max = 4, value = 1,  step = 0.5),
            sliderInput("exp_b", "Base (b):",             min = 0.1, max = 4, value = 2, step = 0.1),
            sliderInput("exp_d", "Vertical Shift (d):",   min = -8, max = 8, value = 0,  step = 1)
          ),
          conditionalPanel("input.fn_family == 'Polynomial'",
            sliderInput("poly_n",  "Degree (n):",            min = 1,  max = 4, value = 2, step = 1),
            sliderInput("poly_a",  "Leading Coefficient:",   min = -2, max = 2, value = 1, step = 0.25),
            sliderInput("poly_a1", "Linear Coefficient (a1):", min = -5, max = 5, value = 0, step = 0.5),
            sliderInput("poly_a0", "Constant (a0):",         min = -8, max = 8, value = 0, step = 1)
          ),
          conditionalPanel("input.fn_family == 'Trigonometric'",
            sliderInput("trig_a", "Amplitude (a):",       min = 0.5, max = 5, value = 1,    step = 0.5),
            sliderInput("trig_b", "Frequency (b):",       min = 0.25, max = 3, value = 1,   step = 0.25),
            sliderInput("trig_c", "Horizontal Shift (c):", min = -3,  max = 3, value = 0,   step = 0.25),
            sliderInput("trig_d", "Vertical Shift (d):",  min = -5,  max = 5, value = 0,   step = 0.5)
          ),
          hr(),
          div(class = "stat-box",
            div(class = "stat-label", "Current Equation"),
            div(style = "font-size: 14px; font-weight: 600; color: #2c6fad; word-break: break-word;",
                textOutput("fn_equation"))
          )
        ),
        mainPanel(
          h2("Exploring Families of Functions"),
          plotOutput("tab4_plot", height = "380px"),
          br(),
          rq_block("rq_4_1", rq_text$tab4[1]),
          rq_block("rq_4_2", rq_text$tab4[2]),
          rq_block("rq_4_3", rq_text$tab4[3]),
          rq_block("rq_4_4", rq_text$tab4[4])
        )
      )
    ),

    # ---- TAB 5: Export ---------------------------------------------------
    tabPanel("5. Export Responses",
      fluidPage(
        br(),
        h2("Export Your Responses to PDF"),
        p("Complete the research questions in Tabs 1–4, then click the button below.",
          "The PDF is generated in your browser and will download automatically."),
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

# ==========================================================================
server <- function(input, output, session) {

  # ---- Reactive partitions -----------------------------------------------
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

  # ---- Reactive ggplot objects (reused by PDF handler) -------------------
  plt_tab1_hist <- reactive({
    var   <- input$tab1_var
    label <- names(num_vars)[num_vars == var]
    df    <- ames[!is.na(ames[[var]]), ]
    ggplot(df, aes(x = .data[[var]])) +
      geom_histogram(fill = "#2c6fad", color = "white", bins = 30) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      labs(x = label, y = "Count", title = paste("Distribution of", label)) +
      theme_minimal(base_size = 13)
  })

  plt_tab1_scatter <- reactive({
    var   <- input$tab1_var
    label <- names(num_vars)[num_vars == var]
    df    <- ames[!is.na(ames[[var]]), ]
    ggplot(df, aes(x = .data[[var]], y = SalePrice)) +
      geom_point(color = "#2c6fad", alpha = 0.3, size = 1.5) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = dollar) +
      labs(x = label, y = "Sale Price ($)", title = paste("Sale Price vs.", label)) +
      theme_minimal(base_size = 13)
  })

  plt_tab2 <- reactive({
    p  <- part2()
    tr <- p$train; tr$Set <- "Training"
    te <- p$test;  te$Set <- "Test"
    combined <- rbind(tr, te)
    ggplot(combined, aes(x = Gr.Liv.Area, y = SalePrice, color = Set)) +
      geom_point(alpha = 0.4, size = 1.5) +
      scale_color_manual(values = c("Training" = "#2c6fad", "Test" = "#e05c2a")) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = dollar) +
      labs(x = "Above Grade Living Area (sq ft)", y = "Sale Price ($)",
           title = "Sale Price vs. Living Area — Training vs. Test Set",
           color = "Data Split") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
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
               color = "#e05c2a", hjust = 0, size = 4) +
      scale_fill_manual(values = c("Training" = "#2c6fad", "Test" = "#2ca05a")) +
      scale_x_continuous(labels = dollar) +
      scale_y_continuous(labels = comma) +
      labs(x = "Sale Price ($)", y = "Count",
           title = "Sale Price Distribution with Mean Baseline Prediction",
           fill = "Data Split") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  plt_tab4 <- reactive({
    x <- seq(-8, 8, length.out = 600)
    y <- switch(input$fn_family,
      "Linear"        = input$lin_m * x + input$lin_b,
      "Exponential"   = input$exp_a * input$exp_b^x + input$exp_d,
      "Polynomial"    = input$poly_a * x^input$poly_n + input$poly_a1 * x + input$poly_a0,
      "Trigonometric" = input$trig_a * sin(input$trig_b * (x - input$trig_c)) + input$trig_d
    )
    df  <- data.frame(x = x, y = y)
    df  <- df[is.finite(df$y) & abs(df$y) < 200, ]
    pal <- c("Linear" = "#2c6fad", "Exponential" = "#e05c2a",
             "Polynomial" = "#2ca05a", "Trigonometric" = "#7b3fad")
    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = pal[[input$fn_family]], linewidth = 1.5) +
      geom_hline(yintercept = 0, color = "gray60", linetype = "dotted") +
      geom_vline(xintercept = 0, color = "gray60", linetype = "dotted") +
      labs(x = "x (independent variable)", y = "y (dependent variable)",
           title = paste(input$fn_family, "Function")) +
      theme_minimal(base_size = 13)
  })

  # ---- renderPlot --------------------------------------------------------
  output$tab1_hist    <- renderPlot({ plt_tab1_hist() })
  output$tab1_scatter <- renderPlot({ plt_tab1_scatter() })
  output$tab2_plot    <- renderPlot({ plt_tab2() })
  output$tab3_plot    <- renderPlot({ plt_tab3() })
  output$tab4_plot    <- renderPlot({ plt_tab4() })

  # ---- Tab 2 stats -------------------------------------------------------
  output$n_train       <- renderText(comma(nrow(part2()$train)))
  output$n_test        <- renderText(comma(nrow(part2()$test)))
  output$train_mean_sp <- renderText(dollar(mean(part2()$train$SalePrice)))
  output$test_mean_sp  <- renderText(dollar(mean(part2()$test$SalePrice)))

  # ---- Tab 3 stats -------------------------------------------------------
  output$model_pred <- renderText(dollar(train_mean()))
  output$train_mae  <- renderText({
    tr <- part3()$train
    dollar(mean(abs(tr$SalePrice - train_mean())))
  })
  output$test_mae <- renderText({
    p <- part3()
    dollar(mean(abs(p$test$SalePrice - train_mean())))
  })

  # ---- Tab 4 equation label ----------------------------------------------
  output$fn_equation <- renderText({
    switch(input$fn_family,
      "Linear"        = paste0("y = ", input$lin_m, "x + ", input$lin_b),
      "Exponential"   = paste0("y = ", input$exp_a, " x ", input$exp_b, "^x + ", input$exp_d),
      "Polynomial"    = paste0("y = ", input$poly_a, "x^", input$poly_n,
                               " + ", input$poly_a1, "x + ", input$poly_a0),
      "Trigonometric" = paste0("y = ", input$trig_a, " x sin(",
                               input$trig_b, "(x - ", input$trig_c, ")) + ", input$trig_d)
    )
  })

  # ---- Completion counter ------------------------------------------------
  output$completion_summary <- renderUI({
    all_ids <- paste0("rq_", rep(1:4, each = 4), "_", rep(1:4, times = 4))
    filled  <- sum(sapply(all_ids, function(id) {
      v <- input[[id]]
      !is.null(v) && nchar(trimws(v)) > 0
    }))
    div(class = "stat-box",
      div(class = "stat-label", "Questions Answered"),
      div(class = "stat-value", paste0(filled, " / 16"))
    )
  })

  # ---- Export status message ---------------------------------------------
  export_msg <- reactiveVal("")
  output$export_status <- renderUI({
    msg <- export_msg()
    if (nchar(msg) == 0) return(NULL)
    div(style = "color:#2c6fad; font-size:13px; margin-top:4px;", msg)
  })

  # ---- PDF generation via base64 → JS download --------------------------
  observeEvent(input$export_btn, {
    export_msg("Building PDF...")

    # ---- PDF drawing helpers ----
    # Write wrapped text at (x,y); return updated y position
    write_block <- function(txt, x, y, width = 84, cex = 0.82,
                             col = "black", font = 1, lh = 0.043) {
      for (ln in strwrap(txt, width = width)) {
        text(x, y, ln, adj = 0, cex = cex, col = col, font = font)
        y <- y - lh
      }
      y - 0.008
    }

    # Start a blank page with [0,1]x[0,1] user coordinates
    new_page <- function() {
      plot.new()
      par(mar = c(0.3, 0.3, 0.3, 0.3))
      plot.window(xlim = c(0, 1), ylim = c(0, 1))
    }

    # Section header; returns y for body content
    section_header <- function(title) {
      new_page()
      text(0.5, 0.975, "MA153X — Lesson 7: Intro to Predictive Modeling",
           adj = 0.5, cex = 0.72, col = "gray50")
      text(0.5, 0.940, title, adj = 0.5, cex = 1.22, font = 2)
      0.895
    }

    # Render Q&A for one tab (questions + responses + sample answers)
    qa_pages <- function(tab_title, qs, rs, ans) {
      y <- section_header(tab_title)
      for (i in seq_along(qs)) {
        if (y < 0.20) { y <- section_header(paste0(tab_title, " (cont.)")) }
        y <- write_block(qs[i], 0.02, y, cex = 0.84, col = "#1a3a6e", font = 2, lh = 0.043)
        resp <- if (nchar(trimws(rs[i])) == 0) "(No response entered.)" else rs[i]
        y <- write_block(paste0("Your response: ", resp),    0.04, y, cex = 0.79, col = "#222222", lh = 0.039)
        y <- write_block(paste0("Sample answer: ", ans[[i]]), 0.04, y, cex = 0.79, col = "#1a5c2a", lh = 0.039)
        y <- y - 0.022
      }
    }

    # Collect current responses
    rs1 <- c(input$rq_1_1, input$rq_1_2, input$rq_1_3, input$rq_1_4)
    rs2 <- c(input$rq_2_1, input$rq_2_2, input$rq_2_3, input$rq_2_4)
    rs3 <- c(input$rq_3_1, input$rq_3_2, input$rq_3_3, input$rq_3_4)
    rs4 <- c(input$rq_4_1, input$rq_4_2, input$rq_4_3, input$rq_4_4)

    tmp <- tempfile(fileext = ".pdf")

    tryCatch({
      pdf(tmp, width = 8.5, height = 11, title = "Lesson 7 Responses")

      # Cover page
      new_page()
      text(0.5, 0.72, "MA153X Data-Driven Modeling",            adj = 0.5, cex = 1.9, font = 2)
      text(0.5, 0.63, "Lesson 7: Intro to Predictive Modeling", adj = 0.5, cex = 1.4)
      text(0.5, 0.55, "Research Questions & Responses",          adj = 0.5, cex = 1.15)
      nm <- trimws(input$student_name); sc <- trimws(input$section)
      if (nchar(nm) > 0) text(0.5, 0.44, paste("Student:", nm), adj = 0.5, cex = 1.0)
      if (nchar(sc) > 0) text(0.5, 0.37, paste("Section:", sc), adj = 0.5, cex = 1.0)
      text(0.5, 0.28, paste("Date:", format(Sys.Date(), "%d %B %Y")), adj = 0.5, cex = 1.0)

      # Tab 1
      qa_pages("Tab 1: Types of Questions",    rq_text$tab1, rs1, sample_answers$tab1)
      print(plt_tab1_hist())
      print(plt_tab1_scatter())

      # Tab 2
      qa_pages("Tab 2: Training & Test Sets",  rq_text$tab2, rs2, sample_answers$tab2)
      print(plt_tab2())

      # Tab 3
      qa_pages("Tab 3: Baseline Model (Mean)", rq_text$tab3, rs3, sample_answers$tab3)
      print(plt_tab3())

      # Tab 4
      qa_pages("Tab 4: Families of Functions", rq_text$tab4, rs4, sample_answers$tab4)
      print(plt_tab4())

      dev.off()

      # Read PDF bytes and base64-encode (strips newlines for safe JS atob())
      pdf_bytes <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
      b64       <- gsub("\n", "", jsonlite::base64_enc(pdf_bytes))

      nm_clean  <- trimws(input$student_name)
      filename  <- if (nchar(nm_clean) > 0) {
        paste0("LSN7_", gsub("[^A-Za-z0-9]", "_", nm_clean), ".pdf")
      } else "LSN7_Responses.pdf"

      session$sendCustomMessage("trigger_download", list(
        b64      = b64,
        filename = filename,
        mime     = "application/pdf"
      ))

      export_msg("Done! Check your downloads folder.")

    }, error = function(e) {
      try(dev.off(), silent = TRUE)
      export_msg(paste("Error:", conditionMessage(e)))
    })
  })
}

shinyApp(ui, server)
