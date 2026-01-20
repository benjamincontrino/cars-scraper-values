
upload_shiny_app <- function() {
  
  # Define UI
  ui <- fluidPage(
    
    # Custom theme with sleek styling
    theme = bs_theme(
      version = 5,
      bg = "#FFFFFF",
      fg = "#2C3E50",
      primary = "#3b82f6",
      secondary = "#64748b",
      base_font = font_google("Inter"),
      heading_font = font_google("Poppins"),
      font_scale = 0.85  # Zoom out everything by 15%
    ),
    
    # Custom CSS for additional styling
    tags$head(
      tags$style(HTML("
      body {
        zoom: 0.9;  /* Additional zoom out */
      }
      
      /* Fix tab text visibility */
      .nav-link {
        color: #5a6c7d !important;
        font-weight: 500;
      }
      
      .nav-link.active {
        color: #1e3a8a !important;
        background-color: rgba(59, 130, 246, 0.1) !important;
        border-bottom: 3px solid #3b82f6 !important;
        font-weight: 600;
      }
      
      .navbar {
        background: linear-gradient(135deg, #3b82f6 0%, #1e40af 100%) !important;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .well {
        background-color: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      }
      
      .about-box {
        background: white;
        border-radius: 12px;
        padding: 30px 40px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.07);
        border: 1px solid #e9ecef;
      }
      
      .about-box p {
        margin-bottom: 15px;
        line-height: 1.7;
      }
      
      .about-box ul {
        margin-left: 20px;
      }
      
      /* Fix card overflow for dropdowns */
      .card {
        overflow: visible !important;
      }
      
      .card-body {
        overflow: visible !important;
      }
      
      /* Ensure selectize dropdowns are visible */
      .selectize-dropdown {
        z-index: 10000 !important;
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #3b82f6 0%, #1e40af 100%);
        border: none;
        border-radius: 6px;
        padding: 10px 20px;
        font-weight: 500;
        transition: transform 0.2s;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(59, 130, 246, 0.4);
      }
      
      h2 {
        color: #2C3E50;
        font-weight: 600;
      }
      
      .form-control, .form-select {
        border-radius: 6px;
        border: 1px solid #dee2e6;
      }
      
      .form-control:focus, .form-select:focus {
        border-color: #3b82f6;
        box-shadow: 0 0 0 0.2rem rgba(59, 130, 246, 0.25);
      }
      
      /* Make container fluid for full width */
      .container-fluid {
        padding-left: 0 !important;
        padding-right: 0 !important;
      }
      
      /* Full width for DT table */
      #car_values_table {
        width: 100% !important;
      }
      
      .dataTables_wrapper {
        width: 100% !important;
        padding: 20px;
      }
    "))
    ),
    
    # App title with styling
    div(
      style = "background: linear-gradient(135deg, #3b82f6 0%, #1e40af 100%); 
             padding: 20px; 
             margin-bottom: 30px; 
             box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      h1("Car Market Value Analyzer", 
         style = "color: white; 
                margin: 0; 
                font-weight: 600; 
                font-size: 2rem;")
    ),
    
    # Tabset panel
    tabsetPanel(
      
      # ============================================
      # TAB 1: ABOUT
      # ============================================
      
      tabPanel(
        "About",
        
        fluidRow(
          column(
            width = 10,
            offset = 1,
            
            div(
              class = "about-box",
              style = "margin-top: 30px;",
              
              p("The following app was created to better understand the car market. The first tab 
              \"Graph Distributions\" is simply looking at the average New/Used price for various 
              makes and models."),
              
              p("The second tab \"Find Car Values\" predicts what car prices should be from your search. 
              In other words, the model is looking at cars of similar makes/models/years/mileage/new/used 
              and predicting what the price should be. Values that are negative are listed LESS than you 
              would expect. Values that are MORE are overpriced cars given their characteristics. The model 
              used to create these predicted prices was a random forest. I experimented with mixed effects 
              models, xg boost, and neural nets but random forest performed best."),
              
              p(strong("To maximize the car value predictor I would recommend filtering your search on cars.com 
              for the following criteria as this was the data the model was trained on:")),
              
              tags$ul(
                tags$li("SUVs and sedans"),
                tags$li("2016+"),
                tags$li("price less than $75,000"),
                tags$li("0 - 80,000 miles")
              ),
              
              p("You may be more descriptive with your search, but at the minimum I would recommend 
              staying inside these parameters."),
              
              p("Note: models and trims were binned together to help sample size. Example: RAV4LE, RAV4XLE, and RAV4Hybrid would all be binned to RAV4")
            )
          )
        )
      ),
      
      # ============================================
      # TAB 2: GRAPH DISTRIBUTIONS
      # ============================================
      
      tabPanel(
        "Graph Distributions",
        
        # Sidebar panel at top
        fluidRow(
          column(
            width = 12,
            
            wellPanel(
              style = "overflow: visible;",
              
              fluidRow(
                column(
                  width = 6,
                  selectInput(
                    inputId = "new_or_used",
                    label = "New or Used:",
                    choices = c("New", "Used"),
                    selected = "Used"
                  )
                ),
                
                column(
                  width = 6,
                  selectInput(
                    inputId = "make",
                    label = "Make:",
                    choices = c(
                      "Acura", "Alfa", "Audi", "Bentley", "Bmw", "Buick", "Cadillac", 
                      "Chevrolet", "Chrysler", "Dodge", "Fiat", "Fisker", "Ford", 
                      "Genesis", "Gmc", "Honda", "Hyundai", "Ineos", "Infiniti", 
                      "Jaguar", "Jeep", "Kia", "Land", "Lexus", "Lincoln", "Maserati", 
                      "Mazda", "Mercedes-Benz", "Mini", "Mitsubishi", "Nissan", 
                      "Polestar", "Porsche", "Rivian", "Subaru", "Tesla", "Toyota", 
                      "Vinfast", "Volkswagen", "Volvo"
                    ),
                    selected = "Toyota"
                  )
                )
              )
            )
          )
        ),
        
        # Main panel with plots side by side
        fluidRow(
          column(
            width = 6,
            plotOutput("distribution_plot_1", height = "400px")
          ),
          column(
            width = 6,
            plotOutput("distribution_plot_2", height = "400px")
          )
        )
      ),
      
      # ============================================
      # TAB 3: FIND CAR VALUES
      # ============================================
      
      tabPanel(
        "Find Car Values",
        
        # Sidebar panel at top
        fluidRow(
          column(
            width = 12,
            
            wellPanel(
              style = "overflow: visible;",
              
              fluidRow(
                column(
                  width = 12,
                  textInput(
                    inputId = "base_url",
                    label = "Cars.com Search URL:",
                    value = "",
                    placeholder = "Paste your Cars.com search URL here...",
                    width = "100%"
                  )
                )
              ),
              
              fluidRow(
                column(
                  width = 6,
                  sliderInput(
                    inputId = "max_pages",
                    label = "Number of Pages to Scrape:",
                    min = 1,
                    max = 25,
                    value = 5,
                    step = 1
                  ),
                  helpText("~100 cars per page. Each page adds ~10 seconds of run time.")
                ),
                
                column(
                  width = 6,
                  br(),
                  br(),
                  actionButton(
                    inputId = "run_prediction",
                    label = "Find Car Values",
                    class = "btn-primary btn-lg",
                    width = "100%"
                  )
                )
              )
            )
          )
        ),
        
        br(),
        
        # Main panel with table - full width, no extra container
        DTOutput("car_values_table")
      )
    )
  )
  
  # Define server
  server <- function(input, output, session) {
    
    # ============================================
    # TAB 2: GRAPH DISTRIBUTIONS
    # ============================================
    
    # Reactive to automatically run when inputs change
    distribution_plots <- reactive({
      # Call your function
      plot_make_prices(
        new_or_used_input = input$new_or_used,
        make_input = input$make
      )
    })
    
    # Output plot 1
    output$distribution_plot_1 <- renderPlot({
      plots <- distribution_plots()
      plots[[1]]  # First plot
    })
    
    # Output plot 2
    output$distribution_plot_2 <- renderPlot({
      plots <- distribution_plots()
      plots[[2]]  # Second plot
    })
    
    # ============================================
    # TAB 3: FIND CAR VALUES
    # ============================================
    
    # Reactive to run when button is clicked
    car_predictions <- eventReactive(input$run_prediction, {
      
      # Validate URL input
      req(input$base_url)
      
      # Show progress
      withProgress(message = 'Scraping car data...', value = 0, {
        
        # Call your prediction function
        result <- predict_cars(
          base_url = input$base_url,
          max_pages = input$max_pages,
          write_new_csv = "NO"
        )
        
        return(result)
      })
    })
    
    # Output table
    output$car_values_table <- renderDT({
      car_predictions()
    })
  }
  
  # Run the app
  shinyApp(ui = ui, server = server)
  
}