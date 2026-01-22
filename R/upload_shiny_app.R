# putting all of our information together into a dynamic shiny app where a user can enter their own cars.com search and i predict what cars are over/under priced
#' @export
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
      heading_font = font_google("Poppins")
    ),
    
    # Custom CSS for additional styling
    tags$head(
      tags$style(HTML("
      /* Animated gradient background */
      @keyframes gradientShift {
        0% { background-position: 0% 50%; }
        50% { background-position: 100% 50%; }
        100% { background-position: 0% 50%; }
      }
      
      /* Smooth fade-in animation */
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      /* Pulse animation for loading */
      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.5; }
      }
      
      /* Spin animation for loading */
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      body {
        background: linear-gradient(135deg, #f8fafc 0%, #e0e7ff 100%);
        animation: fadeIn 0.6s ease;
      }
      
      /* Fix tab text visibility with neon glow on active */
      .nav-link {
        color: #5a6c7d !important;
        font-weight: 500;
        transition: all 0.3s ease;
      }
      
      .nav-link.active {
        color: #1e3a8a !important;
        background-color: rgba(59, 130, 246, 0.1) !important;
        border-bottom: 3px solid #3b82f6 !important;
        font-weight: 600;
        box-shadow: 0 2px 15px rgba(59, 130, 246, 0.4);
      }
      
      .navbar {
        background: linear-gradient(135deg, #3b82f6 0%, #1e40af 100%) !important;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      /* Glass morphism for well panels */
      .well {
        background: rgba(255, 255, 255, 0.75);
        backdrop-filter: blur(12px);
        -webkit-backdrop-filter: blur(12px);
        border: 1px solid rgba(255, 255, 255, 0.4);
        border-radius: 12px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.07), 
                    0 0 20px rgba(59, 130, 246, 0.1);
        animation: fadeIn 0.8s ease;
      }
      
      /* Glass morphism for about box */
      .about-box {
        background: rgba(255, 255, 255, 0.8);
        backdrop-filter: blur(15px);
        -webkit-backdrop-filter: blur(15px);
        border-radius: 16px;
        padding: 30px 40px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1),
                    0 0 30px rgba(59, 130, 246, 0.15);
        border: 1px solid rgba(255, 255, 255, 0.5);
        animation: fadeIn 0.8s ease;
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
      
      /* Neon glow buttons with enhanced effects */
      .btn-primary {
        background: linear-gradient(135deg, #3b82f6 0%, #1e40af 100%);
        border: none;
        border-radius: 8px;
        padding: 12px 24px;
        font-weight: 600;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(59, 130, 246, 0.4),
                    0 0 20px rgba(59, 130, 246, 0.2);
        position: relative;
        overflow: hidden;
      }
      
      .btn-primary::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.3), transparent);
        transition: left 0.5s;
      }
      
      .btn-primary:hover::before {
        left: 100%;
      }
      
      .btn-primary:hover {
        transform: translateY(-3px);
        box-shadow: 0 6px 25px rgba(59, 130, 246, 0.6),
                    0 0 40px rgba(59, 130, 246, 0.4);
      }
      
      .btn-primary:active {
        transform: translateY(-1px);
      }
      
      h2 {
        color: #2C3E50;
        font-weight: 600;
      }
      
      /* Enhanced input styling with glow on focus */
      .form-control, .form-select {
        border-radius: 8px;
        border: 1px solid #dee2e6;
        transition: all 0.3s ease;
        background: rgba(255, 255, 255, 0.9);
      }
      
      .form-control:focus, .form-select:focus {
        border-color: #3b82f6;
        box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.15),
                    0 0 20px rgba(59, 130, 246, 0.3);
        transform: scale(1.01);
      }
      
      /* Make container fluid for full width */
      .container-fluid {
        padding-left: 0 !important;
        padding-right: 0 !important;
      }
      
      /* Full width for DT table with enhanced styling */
      #car_values_table {
        width: 100% !important;
        animation: fadeIn 1s ease;
      }
      
      .dataTables_wrapper {
        width: 100% !important;
        padding: 20px;
        background: rgba(255, 255, 255, 0.6);
        backdrop-filter: blur(10px);
        -webkit-backdrop-filter: blur(10px);
        border-radius: 12px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.07);
      }
      
      /* Enhanced table styling */
      table.dataTable {
        border-collapse: separate !important;
        border-spacing: 0 4px !important;
      }
      
      table.dataTable tbody tr {
        background: white;
        transition: all 0.3s ease;
      }
      
      table.dataTable tbody tr:hover {
        transform: scale(1.01);
        box-shadow: 0 4px 12px rgba(59, 130, 246, 0.2);
        background: rgba(59, 130, 246, 0.05);
      }
      
      table.dataTable thead th {
        background: linear-gradient(135deg, #3b82f6 0%, #1e40af 100%);
        color: white;
        font-weight: 600;
        padding: 15px 10px;
        border: none;
        text-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
      }
      
      /* Plot containers with hover effect */
      .shiny-plot-output {
        transition: all 0.3s ease;
        border-radius: 12px;
        overflow: hidden;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
        background: white;
        animation: fadeIn 0.8s ease;
      }
      
      .shiny-plot-output:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(59, 130, 246, 0.2);
      }
      
      /* Loading spinner enhancement */
      .shiny-busy-indicator {
        background: linear-gradient(135deg, #3b82f6, #1e40af);
        border-radius: 50%;
        animation: spin 1s linear infinite, pulse 2s ease-in-out infinite;
      }
      
      /* Slider styling */
      .irs-bar {
        background: linear-gradient(135deg, #3b82f6 0%, #1e40af 100%);
        box-shadow: 0 2px 8px rgba(59, 130, 246, 0.3);
      }
      
      .irs-from, .irs-to, .irs-single {
        background: #3b82f6;
        box-shadow: 0 2px 8px rgba(59, 130, 246, 0.4);
      }
      
      /* Help text styling */
      .help-block {
        font-style: italic;
        font-size: 0.9em;
        color: #64748b;
      }
    "))
    ),
    
    # App title with animated gradient styling
    div(
      style = "background: linear-gradient(135deg, #3b82f6, #1e40af, #6366f1, #3b82f6); 
               background-size: 300% 300%;
               animation: gradientShift 8s ease infinite;
               padding: 25px; 
               margin-bottom: 30px; 
               box-shadow: 0 4px 20px rgba(59, 130, 246, 0.3);",
      h1("Car Market Value Analyzer", 
         style = "color: white; 
                  margin: 0; 
                  font-weight: 700; 
                  font-size: 2.2rem;
                  text-shadow: 0 2px 10px rgba(0, 0, 0, 0.3);")
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
              
              p("Note: models and trims were binned together to help sample size. Example: RAV4LE, RAV4XLE, and RAV4Hybrid would all be binned to RAV4"),
              
              p(paste0("Model last trained on ", readRDS("data/cars_rf_model.rds")[[2]]))
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
        
        br(),
        br(),
        
        # Main panel with plots side by side
        fluidRow(
          column(
            width = 6,
            plotOutput("distribution_plot_1", height = "500px")
          ),
          column(
            width = 6,
            plotOutput("distribution_plot_2", height = "500px")
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