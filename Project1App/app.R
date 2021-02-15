library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(shinyWidgets)
LAPD <- read_csv('Arrest_Data_from_2020_to_Present.csv')

# Define UI for application that plots arrest data -----------
ui <- fluidPage(
  
  # Application title -----------------------------------------------
  titlePanel("LAPD Arrests"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Race and Ethnicity" = "`Descent Code`", 
                              "Sex" = "`Sex Code`", 
                              "Arrest Type" = "`Arrest Type Code`"), 
                  selected = "Descent Code"),
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Enter text for plot title ---------------------------------------------
      textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text to be used as plot title"),
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Select which areas to include ------------------------
      pickerInput(inputId = "selected_hood",
                  label = "Select area(s):",
                  choices = unique(LAPD$`Area Name`),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
    ),
    
    # Output: -------------------------------------------------------
    mainPanel(
      
      # Show timeplot --------------------------------------------
      plotOutput(outputId = "timeplot"),
      br(),        # a little bit of visual separation
      
      # Show barplot --------------------------------------------
      plotOutput(outputId = "barplot"),
      br(),        # a little bit of visual separation
      
      # Show piechart --------------------------------------------
      plotOutput(outputId = "pie"),
      br(),        # a little bit of visual separation
      
      
      # Print number of obs plotted ---------------------------------
      uiOutput(outputId = "n"),
      br(), br(),    # a little bit of visual separation
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "moviestable")
    )
  )
)

# Define server function required to create the barplot ---------
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types ------
  LAPD_subset <- reactive({
    req(input$selected_hood) # ensure availablity of value before proceeding
    filter(LAPD, `Area Name` %in% input$selected_hood)
  })
  
  # Convert plot_title toTitleCase ----------------------------------
  pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
  
  # Create scatter/line plot with count of arrests per time period --
      # first get totals
      time_data <- LAPD %>% group_by(`Booking Date`) %>% summarise(count=n())
  
  
  output$timeplot <- renderPlot({
    ggplot(data = time_data, aes_string(x = '`Booking Date`', y='count')) +
      geom_point() +
      geom_line() +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(x = 'Date',
           y = 'Arrest Count',
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = 'Arrests over Time'
      )
  })
  
  # Create barplot object the plotOutput function is expecting --
  output$barplot <- renderPlot({
    ggplot(data = LAPD_subset(), aes_string(x = input$x)) +
      geom_bar() +
      labs(x = toTitleCase(str_replace_all(input$y, "_", " ")),
           y = 'Arrest Count',
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = pretty_plot_title()
      )
  })
  
  
  # Create pie chart of offenses
  output$pie <- renderPlot({
    ggplot(data=LAPD_subset(), aes(x="",fill=`Charge Group Description`)) +
      geom_bar(width=1) +
      coord_polar("y", start=0) +
      labs(x = 'Percentage of Arrests',
           title = 'Breakdown of Offenses'
      )
  })
  
  
  # Print data table if checked -------------------------------------
  output$moviestable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = LAPD_subset()[,c(3,4,6,8:10,12:15,22)], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

