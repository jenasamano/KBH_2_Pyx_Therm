# Everything before the ui function runs on startup
library(shiny)
library(tidyverse)
#library(patchwork)

rm(list=ls())

debugging <- TRUE

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
      fileInput("raw_data_file", "Upload Electron Microprobe data", accept = ".csv"),
      thermometer_selector(inputId = "thermometer", label = "In Communist Russia, thermometer choose you"),
      axis_selector(inputId = "x_axis", label = "Left Graph X-axis data"),
      axis_selector(inputId = "y_axis", label = "Left Graph Y-axis data"),
      show_full_calcs_selector(inputId = "full_table", label = "Show full table of calculations"),
      downloadButton(outputId = "download_data", inputId = "Download Temperature in CSV"),
      actionButton(inputId = "tern_plot_button", label = "Print ternary plot") # THIS IS JUST A TEST
    ),
    
    # Show a table and plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Data",
                 tableOutput(outputId = "processed_data_table"),
                 #plotOutput(outputId = "location_Temp")),
        tabPanel("Plots",
                 plotOutput(outputId = "ternary_plot"),
                 includeMarkdown("ternary_text.md")),
        tabPanel("Instructions", # I'd probably put this text in a separate function. Note also that it isn't using markdown
                 includeMarkdown("instructions.md"),
                 img(src = "example_file.png", height = 213, width = 570))
      )
    )
  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output){
  
  thermometer <- reactive(input$thermometer)
  
  pressure <- reactive(input$pressure)
  
  # upload the data separately in its own reactive
  raw_data_file <- reactive({ 
    input$raw_data_file
    # get data file
  })
  
  processed_data <- reactive({
    ###
    # I've set a debugging boolean to skip uploading the file
    # Remove this before production
    ###
    if(debugging) {
      process_raw_data("~/Documents/work/KBH_2_Pyx_Therm/samp_data-prototypeT.csv", thermometer(), pressure())
      warning("TURN OFF DEBUGGING BEFORE PRODUCTION YOU DIRTBAG")
    } else {
      process_raw_data(raw_data_file()$datapath, thermometer(), pressure()) # Read data
    }
  })
  
  # Create table of temperatures
  output$processed_data_table <- renderTable({
    processed_data()
  })
  
  # Ternary plot with fake data
  ternary_plot <- reactive(
    input$ternary_plot
    #make_ternary_plot(processed_data())
  )
  
  tern_plot_obj <- eventReactive(
    input$tern_plot_button,
    make_ternary_plot(processed_data())
  )
  output$ternary_plot <- renderPlot({
    print(tern_plot_obj())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
