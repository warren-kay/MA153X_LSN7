library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# Load and clean data
ames <- read.csv("EDA_AmesHousing.csv", stringsAsFactors = FALSE)
ames <- ames[complete.cases(ames[c("SalePrice", "Gr.Liv.Area")]), ]

# Named vector: display label -> column name (dot notation from read.csv)
num_vars <- c(
  "Above Grade Living Area (sq ft)" = "Gr.Liv.Area",
  "Total Basement Area (sq ft)"     = "Total.Bsmt.SF",
  "Garage Area (sq ft)"             = "Garage.Area",
  "Year Built"                      = "Year.Built",
  "Overall Quality (1-10)"          = "Overall.Qual"
)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 14px; }
    h2 { font-size: 20px; font-weight: 600; margin-top: 0; }
    h4 { font-size: 15px; font-weight: 600; color: #333; margin-top: 18px; margin-bottom: 4px; }
    .well { background: #f7f7f7; border: 1px solid #e0e0e0; border-radius: 6px; padding: 14px; }
    .rq-box { background: #eef4fb; border-left: 4px solid #2c6fad; padding: 10px 14px;
               border-radius: 0 6px 6px 0; margin-bottom: 10px; font-size: 13px; }
    .stat-box { background: #fff; border: 1px solid #ddd; border-radius: 6px;
                padding: 10px 14px; margin-bottom: 8px; }
    .stat-label { font-size: 12px; color: #777; margin-bottom: 2px; }
    .stat-value { font-size: 22px; font-weight: 600; color: #2c6fad; }
    .nav-tabs > li > a { font-size: 13px; }
  "))),

  titlePanel("Intro to Predictive Modeling — Ames Housing Data"),

  tabsetPanel(

    # ---- TAB 1: Types of Questions (Objective 1) -------------------------
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
            column(6,
              strong("Descriptive — What happened?"),
              plotOutput("tab1_hist", height = "260px")
            ),
            column(6,
              strong("Predictive — What will happen?"),
              plotOutput("tab1_scatter", height = "260px")
            )
          ),
          br(),
          div(class = "rq-box", "RQ 1. The histogram shows the distribution of the variable you selected. Is asking 'What is the average value of this variable across all Ames houses?' a descriptive or a predictive question? Why?"),
          div(class = "rq-box", "RQ 2. The scatter plot shows sale price vs. your selected variable. What type of question does this plot set up: descriptive or predictive? Explain your reasoning."),
          div(class = "rq-box", "RQ 3. In the scatter plot, which variable is the independent variable and which is the dependent variable? How did you decide which is which?"),
          div(class = "rq-box", "RQ 4. Using the Ames Housing data, write one descriptive question and one predictive question. How are these questions fundamentally different in what they are asking?")
        )
      )
    ),

    # ---- TAB 2: Training & Test Sets (Objective 2) -----------------------
    tabPanel("2. Training & Test Sets",
      sidebarLayout(
        sidebarPanel(
          h4("Partition Controls"),
          sliderInput("split_pct", "Training Set (% of data):",
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
          div(class = "rq-box", "RQ 1. Set the training set to 70%. How many houses are in the training set? How many are in the test set? Do they add up to the total number of houses?"),
          div(class = "rq-box", "RQ 2. Why do we need to hold back a test set? What problem would occur if we evaluated our model using the same data we used to build it?"),
          div(class = "rq-box", "RQ 3. Change the random seed a few times. What changes on the plot? What stays the same? Why does specifying the seed matter when working with a partner?"),
          div(class = "rq-box", "RQ 4. Compare the mean sale price of the training and test sets in the sidebar. Are they close to each other? Why is it important that the two sets have similar distributions?")
        )
      )
    ),

    # ---- TAB 3: Mean Baseline Model (Objective 3) ------------------------
    tabPanel("3. Baseline Model (Mean)",
      sidebarLayout(
        sidebarPanel(
          h4("Partition Controls"),
          sliderInput("m_split_pct", "Training Set (% of data):",
                      min = 50, max = 90, value = 70, step = 5),
          sliderInput("m_seed", "Random Seed:",
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
          div(class = "rq-box", "RQ 1. Our baseline model predicts the same sale price for every house in the dataset. What price does it predict, and where did that number come from?"),
          div(class = "rq-box", "RQ 2. Mean Absolute Error (MAE) is the average dollar amount our predictions are off. What is the training MAE? What is the test MAE? Are they close to each other?"),
          div(class = "rq-box", "RQ 3. Adjust the training set percentage from 50% to 90%. How does the training MAE and test MAE change? At which split do the two errors seem most similar?"),
          div(class = "rq-box", "RQ 4. What information about a specific house does the mean model completely ignore? Why might knowing a house's square footage allow us to make a better prediction?")
        )
      )
    ),

    # ---- TAB 4: Families of Functions (Objective 4) ----------------------
    tabPanel("4. Families of Functions",
      sidebarLayout(
        sidebarPanel(
          h4("Function Explorer"),
          selectInput("fn_family", "Select a Function Family:",
                      choices = c("Linear", "Exponential", "Polynomial", "Trigonometric")),

          conditionalPanel("input.fn_family == 'Linear'",
            sliderInput("lin_m", "Slope (m):", min = -3, max = 3, value = 1, step = 0.25),
            sliderInput("lin_b", "y-Intercept (b):", min = -8, max = 8, value = 0, step = 0.5)
          ),

          conditionalPanel("input.fn_family == 'Exponential'",
            sliderInput("exp_a", "Vertical Stretch (a):", min = -4, max = 4, value = 1, step = 0.5),
            sliderInput("exp_b", "Base (b):", min = 0.1, max = 4, value = 2, step = 0.1),
            sliderInput("exp_d", "Vertical Shift (d):", min = -8, max = 8, value = 0, step = 1)
          ),

          conditionalPanel("input.fn_family == 'Polynomial'",
            sliderInput("poly_n", "Degree (n):", min = 1, max = 4, value = 2, step = 1),
            sliderInput("poly_a", "Leading Coefficient:", min = -2, max = 2, value = 1, step = 0.25),
            sliderInput("poly_a1", "Linear Coefficient (a₁):", min = -5, max = 5, value = 0, step = 0.5),
            sliderInput("poly_a0", "Constant (a₀):", min = -8, max = 8, value = 0, step = 1)
          ),

          conditionalPanel("input.fn_family == 'Trigonometric'",
            sliderInput("trig_a", "Amplitude (a):", min = 0.5, max = 5, value = 1, step = 0.5),
            sliderInput("trig_b", "Frequency (b):", min = 0.25, max = 3, value = 1, step = 0.25),
            sliderInput("trig_c", "Horizontal Shift (c):", min = -3, max = 3, value = 0, step = 0.25),
            sliderInput("trig_d", "Vertical Shift (d):", min = -5, max = 5, value = 0, step = 0.5)
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
          div(class = "rq-box", "RQ 1. Select the Linear function. Adjust the slope (m). What happens to the line as m increases from 0? What does a negative slope look like compared to a positive slope?"),
          div(class = "rq-box", "RQ 2. Switch to Exponential. Set a = 1 and d = 0, then compare b = 0.5 vs. b = 2. How does the base (b) control whether the function grows or decays as x increases?"),
          div(class = "rq-box", "RQ 3. Select Polynomial with degree 2. How many times does the curve change direction? Increase the degree to 3, then 4. Based on the pattern, what is the maximum number of direction changes for a degree-n polynomial?"),
          div(class = "rq-box", "RQ 4. Switch to Trigonometric. Which parameter controls the height of the peaks (amplitude)? Which controls how many full cycles appear in the graph window?")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # ---- Shared partition reactives ----------------------------------------
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

  # ---- TAB 1 -------------------------------------------------------------
  output$tab1_hist <- renderPlot({
    var   <- input$tab1_var
    label <- names(num_vars)[num_vars == var]
    df    <- ames[!is.na(ames[[var]]), ]
    ggplot(df, aes(x = .data[[var]])) +
      geom_histogram(fill = "#2c6fad", color = "white", bins = 30) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      labs(x = label, y = "Count",
           title = paste("Distribution of", label)) +
      theme_minimal(base_size = 13)
  })

  output$tab1_scatter <- renderPlot({
    var   <- input$tab1_var
    label <- names(num_vars)[num_vars == var]
    df    <- ames[!is.na(ames[[var]]), ]
    ggplot(df, aes(x = .data[[var]], y = SalePrice)) +
      geom_point(color = "#2c6fad", alpha = 0.3, size = 1.5) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = dollar) +
      labs(x = label, y = "Sale Price ($)",
           title = paste("Sale Price vs.", label)) +
      theme_minimal(base_size = 13)
  })

  # ---- TAB 2 -------------------------------------------------------------
  output$n_train       <- renderText(comma(nrow(part2()$train)))
  output$n_test        <- renderText(comma(nrow(part2()$test)))
  output$train_mean_sp <- renderText(dollar(mean(part2()$train$SalePrice)))
  output$test_mean_sp  <- renderText(dollar(mean(part2()$test$SalePrice)))

  output$tab2_plot <- renderPlot({
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

  # ---- TAB 3 -------------------------------------------------------------
  train_mean <- reactive(mean(part3()$train$SalePrice))

  output$model_pred <- renderText(dollar(train_mean()))

  output$train_mae <- renderText({
    tr   <- part3()$train
    pred <- train_mean()
    dollar(mean(abs(tr$SalePrice - pred)))
  })

  output$test_mae <- renderText({
    p    <- part3()
    pred <- train_mean()
    dollar(mean(abs(p$test$SalePrice - pred)))
  })

  output$tab3_plot <- renderPlot({
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

  # ---- TAB 4 -------------------------------------------------------------
  output$fn_equation <- renderText({
    switch(input$fn_family,
      "Linear" = paste0(
        "y = ", input$lin_m, "x + ", input$lin_b
      ),
      "Exponential" = paste0(
        "y = ", input$exp_a, " × ", input$exp_b, "^x + ", input$exp_d
      ),
      "Polynomial" = paste0(
        "y = ", input$poly_a, "x^", input$poly_n,
        " + ", input$poly_a1, "x + ", input$poly_a0
      ),
      "Trigonometric" = paste0(
        "y = ", input$trig_a, " × sin(",
        input$trig_b, "(x − ", input$trig_c, ")) + ", input$trig_d
      )
    )
  })

  output$tab4_plot <- renderPlot({
    x <- seq(-8, 8, length.out = 600)

    y <- switch(input$fn_family,
      "Linear"        = input$lin_m * x + input$lin_b,
      "Exponential"   = input$exp_a * input$exp_b^x + input$exp_d,
      "Polynomial"    = input$poly_a * x^input$poly_n + input$poly_a1 * x + input$poly_a0,
      "Trigonometric" = input$trig_a * sin(input$trig_b * (x - input$trig_c)) + input$trig_d
    )

    df <- data.frame(x = x, y = y)
    df <- df[is.finite(df$y) & abs(df$y) < 200, ]

    pal <- c(
      "Linear"        = "#2c6fad",
      "Exponential"   = "#e05c2a",
      "Polynomial"    = "#2ca05a",
      "Trigonometric" = "#7b3fad"
    )
    col <- pal[[input$fn_family]]

    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = col, linewidth = 1.5) +
      geom_hline(yintercept = 0, color = "gray60", linetype = "dotted") +
      geom_vline(xintercept = 0, color = "gray60", linetype = "dotted") +
      labs(x = "x (independent variable)", y = "y (dependent variable)",
           title = paste(input$fn_family, "Function")) +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
