
library(shiny)


# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
Data <- df1



# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Life Ladder"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Fertility rate" = "mean_fer",
                    "Suicide rate" = "mean_sucide",
                    "Pollution" = "mean_d_pollut",
                    "Gini score" = "mean_g")),
      
      # Input: Checkbox for whether outliers should be included ----
      # checkboxInput("regression", "Show regression", TRUE),
      
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("fertility rate", "suicide rate", "life_ladder", "income_ineqality", "pollution")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("llPlot"),
      
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----

server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mean_ll~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$llPlot <- renderPlot({
    plot(as.formula(formulaText()),
         data = Data,
         # outline = input$regression,
         col = "#75AADB", pch = 19)
    #linear_mod = lm(as.formula(formulaText(), data = Data))
    #linear_pred = predict(linear_mod)
    #lines(Data$mean_ll, linear_pred,lwd=2,col="blue")
    
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "fertility rate" = fer,
           "suicide rate" = sus,
           "income_ineqality" = income_ineqq,
           "life_ladder" = life_lad,
           "pollution" = pollut)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
