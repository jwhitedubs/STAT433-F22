
library(shiny)
library(ggplot2)
library(tidyr)
library(readxl)
library(dplyr)

life_lad = read_excel("/Users/Jacob/Documents/STAT433/Project/Chapter2OnlineData.xls")
fer_rate = read.csv("/Users/Jacob/Documents/STAT433/Project/children-per-woman-UN.csv")
pollution = read.csv("/Users/Jacob/Documents/STAT433/Project/outdoor-pollution-deaths-1990-2017.csv")
income_ineq = read_excel("/Users/Jacob/Documents/STAT433/Project/DataInput_ChartbookOfEconomicInequality.xls")

sus_rate = read.csv("/Users/Jacob/Documents/STAT433/Project/data.csv")

life_lad = life_lad %>% 
  select("Country name", "Year", "Life Ladder", "Log GDP per capita") %>% 
  rename(country = "Country name",
         year = "Year",
         log_gdp = "Log GDP per capita")

fer = fer_rate %>% 
  select(-"Code") %>% 
  rename(ferate = "Fertility.rate...Sex..all...Age..all...Variant..estimates",
         country = "Entity",
         year = "Year")

income_ineqq = income_ineq %>% 
  filter(`meaure of inequality` == "Gini Coefficient") %>% 
  drop_na() %>% 
  mutate(g_coef = value/100) %>% 
  select("country", "year", "g_coef")

pollut = pollution %>% 
  mutate(year = as.numeric(Year)) %>% 
  rename(country = "Entity",
         d = "Deaths...Cause..All.causes...Risk..Outdoor.air.pollution...OWID...Sex..Both...Age..All.Ages..Number.") %>% 
  mutate(death = log(d, base = exp(2))) %>% 
  
  select("country", "year", "death") 

sus = sus_rate %>% 
  filter(X.1 == "Both sexes") %>% 
  rename(susrate_2019 = "Crude.suicide.rates..per.100.000.population.",
         susrate_2018 = "Crude.suicide.rates..per.100.000.population..1" ,
         susrate_2017 = "Crude.suicide.rates..per.100.000.population..2",
         susrate_2016 = "Crude.suicide.rates..per.100.000.population..3" ,
         susrate_2015 = "Crude.suicide.rates..per.100.000.population..4",
         susrate_2014 = "Crude.suicide.rates..per.100.000.population..5",
         susrate_2013 = "Crude.suicide.rates..per.100.000.population..6",
         susrate_2012 = "Crude.suicide.rates..per.100.000.population..7",
         susrate_2011 = "Crude.suicide.rates..per.100.000.population..8",
         susrate_2010 = "Crude.suicide.rates..per.100.000.population..9",
         susrate_2009 = "Crude.suicide.rates..per.100.000.population..10",
         susrate_2008 = "Crude.suicide.rates..per.100.000.population..11",
         susrate_2007 = "Crude.suicide.rates..per.100.000.population..12",
         susrate_2006 = "Crude.suicide.rates..per.100.000.population..13",
         susrate_2005 = "Crude.suicide.rates..per.100.000.population..14",
         susrate_2004 = "Crude.suicide.rates..per.100.000.population..15",
         susrate_2003 = "Crude.suicide.rates..per.100.000.population..16",
         susrate_2002 = "Crude.suicide.rates..per.100.000.population..17",
         susrate_2001 = "Crude.suicide.rates..per.100.000.population..18",
         susrate_2000 = "Crude.suicide.rates..per.100.000.population..19" ,
         country = "X") %>% 
  pivot_longer(cols = c("susrate_2019", "susrate_2018", "susrate_2017", "susrate_2016", "susrate_2015", "susrate_2014", "susrate_2013", "susrate_2012", "susrate_2011", "susrate_2010", "susrate_2009", "susrate_2008", "susrate_2007", "susrate_2006", "susrate_2005", "susrate_2004", "susrate_2003", "susrate_2002", "susrate_2001", "susrate_2000"), names_to = "Year", values_to = "susrate") %>% 
  separate(Year, c('x', 'y')) %>% 
  mutate(year = as.numeric(y)) %>% 
  mutate(r = substr(susrate, 1, 3) ) %>% 
  mutate(rate = as.numeric(r)) %>% 
  select("country", "year", "rate")



df = life_lad %>% 
  rename(ll = "Life Ladder") %>% 
  left_join(fer, by = c("country", "year")) %>% 
  left_join(sus, by = c("country", "year")) %>% 
  left_join(income_ineqq, by = c("country", "year")) %>% 
  left_join(pollut, by = c("country", "year"))

df1 = df %>% 
  group_by(country) %>% 
  summarise(mean_ll = mean(ll),
            mean_fer = mean(ferate, na.rm = T),
            mean_sucide = mean(rate, na.rm = T),
            mean_d_pollut = mean(death, na.rm = T),
            mean_g = mean(g_coef, na.rm = T))


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
