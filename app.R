# Everything before the ui function runs on startup
library(shiny)
library(tidyverse)
#library(patchwork)

# Load necessary functions. 
all_r_files <- paste0("R/", dir("R"))
lapply(all_r_files, source)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("2 Pyroxene Thermometer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("pressure", "Pressure in kilobars:", min = 1, max = 20, value = 1),
      fileInput("uploaded_data", "Upload Electron Microprobe data", accept = ".csv"),
      thermometer_selector(inputId = "thermometer", label = "In Communist Russia, thermometer choose you"),
      axis_selector(inputId = "x_axis", label = "Left Graph X-axis data"),
      axis_selector(inputId = "y_axis", label = "Left Graph Y-axis data"),
      show_full_calcs_selector(inputId = "full_table", label = "Show full table of calculations"),
      downloadButton(outputId = "download_data", inputId = "Download Temperature in CSV"),
      actionButton(inputId = "ternary_plot", label = "Print ternary plot") # THIS IS JUST A TEST
    ),
    
    # Show a table and plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Data and Plots",
          tableOutput(outputId = "processed_data_table"),
          #plotOutput(outputId = "location_Temp")
        ),
        tabPanel("Instructions", # I'd probably put this text in a separate function. Note also that it isn't using markdown
                 includeMarkdown("instructions.md"),
                 img(src = "example_file.png", height = 213, width = 570))
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output){
  
  thermometer <- reactive(input$thermometer)
  
  # upload the data separately in its own reactive
  raw_datafile <- reactive({ 
    input$uploaded_data
    # get data file
  })
  
  processed_data <- reactive({
    process_raw_data(raw_datafile(), thermometer()) # Read data
  })
  
  # Create table of temperatures
  output$processed_data_table <- renderTable({
    processed_data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
