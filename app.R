# Add standard deviation info 11/29
# Is this thing on?

library(shiny)
library(tidyverse)
library(patchwork)

# Load necessary functions. 
all_r_files <- paste0("R/", dir("R"))
source("R/calc_Brey_Kohler_temp.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("2 Pyroxene Thermometer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("pressure", "Pressure in kilobars:", min = 1, max = 20, value = 1),
      fileInput("uploaded_data", "Upload Electron Microprobe data", accept = ".csv"),
      x_axis_selector(),
      y_axis_selector(),
      show_full_calcs_selector(),
      downloadButton("download_data","Download Temperature in CSV"),
      actionButton(inputId = "ternary_plot", label = "Print ternary plot") # THIS IS JUST A TEST
    ),
    
    # Show a table and plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Data and Plots",
          tableOutput(outputId = "samp_data_prototypeT"),
          plotOutput(outputId = "location_Temp")
        ),
        tabPanel("Instructions", # I'd probably put this text in a separate function. Note also that it isn't using markdown
                 includeMarkdown("instructions.md"),
                 img(src = "example_file.png", height = 213, width = 570))   
      )
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output){
  
  output$contents <-renderTable({
    uploaded_file <- input$uploaded_data
    if(is.null(uploaded_file)) return(NULL)
    read.csv(uploaded_file$datapath, header = TRUE)
  })
  
  output$samp_data_prototypeT <- renderTable({
    
    uploaded_file <- input$uploaded_data
    
    if(is.null(input$uploaded_data)) return(NULL)
    else{
      samp_data_prototypeT <- read.csv(uploaded_file$datapath, header = TRUE) 
      if(ncol(samp_data_prototypeT)!= 12){
        print("Incorrect number of columns provided. Please see instructions")
      }
      else{
        names(samp_data_prototypeT)[str_detect(names(samp_data_prototypeT),"point_location_CPX")] <- "point_location_CPX"
        samp_data_prototypeT <- mutate(samp_data_prototypeT,X_Fe_CPX = Nb_ions_Fe_CPX/(Nb_ions_Fe_CPX + Nb_ions_Mg_CPX))
        samp_data_prototypeT
        
        pressure <- input$pressure
        
        samp_data_prototypeT <- mutate(samp_data_prototypeT,
                                       X_Fe_OPX = Nb_ions_Fe_OPX/(Nb_ions_Fe_OPX + Nb_ions_Mg_OPX),
                                       Ca_star_CPX = Nb_ions_Ca_CPX/(1-Nb_ions_Na_CPX),
                                       Ca_star_OPX = Nb_ions_Ca_OPX/(1-Nb_ions_Na_OPX),
                                       K_sub_D = (1-Ca_star_CPX)/(1-Ca_star_OPX),
                                       onehundredtwentysix_times_X_Fe_CPX=126.3*X_Fe_CPX,
                                       plustwentyfour = onehundredtwentysix_times_X_Fe_CPX+24.9,
                                       times_pressure = plustwentyfour*pressure,
                                       T_bacon_numerator = 23664 + times_pressure,
                                       ln_K_sub_D = log(K_sub_D),
                                       ln_K_sub_D_SQRD = ln_K_sub_D^2,
                                       T_bacon_denominator = 13.38 + ln_K_sub_D_SQRD + (11.59 * X_Fe_OPX),
                                       Temp_Kelvins = T_bacon_numerator/T_bacon_denominator,
                                       Temp_Celsius = Temp_Kelvins-273.15) %>% 
          filter(!is.na(Temp_Kelvins))
        samp_data_prototypeT
        
        display_table <- select(samp_data_prototypeT,
                                point_location_CPX,
                                point_location_OPX,
                                Temp_Celsius,
                                Temp_Kelvins,
                                X_Fe_CPX,
                                X_Fe_OPX,
                                point_number_CPX,
                                point_number_OPX)
        if(input$full_table=="yes") samp_data_prototypeT
        else display_table
      }    
    }})
  output$download_data <- downloadHandler(
    filename = function() {
      "Bacon_Temps.csv"
    },
    content = function(file) {
      uploaded_file <- input$uploaded_data
      
      if (is.null(input$uploaded_data))
        return(NULL)
      else{
        samp_data_prototypeT <-
          read.csv(uploaded_file$datapath, header = TRUE)
        
        
        names(samp_data_prototypeT)[str_detect(names(samp_data_prototypeT), "point_location_CPX")] <-
          "point_location_CPX"
        samp_data_prototypeT <-
          mutate(samp_data_prototypeT,
                 X_Fe_CPX = Nb_ions_Fe_CPX / (Nb_ions_Fe_CPX + Nb_ions_Mg_CPX))
        samp_data_prototypeT
        
        pressure <- input$pressure
        
        samp_data_prototypeT <- mutate(
          samp_data_prototypeT,
          X_Fe_OPX = Nb_ions_Fe_OPX / (Nb_ions_Fe_OPX + Nb_ions_Mg_OPX),
          Ca_star_CPX = Nb_ions_Ca_CPX /
            (1 - Nb_ions_Na_CPX),
          Ca_star_OPX = Nb_ions_Ca_OPX /
            (1 - Nb_ions_Na_OPX),
          K_sub_D = (1 - Ca_star_CPX) / (1 -
                                           Ca_star_OPX),
          onehundredtwentysix_times_X_Fe_CPX =
            126.3 * X_Fe_CPX,
          plustwentyfour = onehundredtwentysix_times_X_Fe_CPX +
            24.9,
          times_pressure = plustwentyfour *
            pressure,
          T_bacon_numerator = 23664 + times_pressure,
          ln_K_sub_D = log(K_sub_D),
          ln_K_sub_D_SQRD = ln_K_sub_D ^
            2,
          T_bacon_denominator = 13.38 + ln_K_sub_D_SQRD + (11.59 * X_Fe_OPX),
          Temp_Kelvins = T_bacon_numerator /
            T_bacon_denominator,
          Temp_Celsius = Temp_Kelvins - 273.15
        ) %>% 
          filter(!is.na(Temp_Kelvins))}
      write.csv(samp_data_prototypeT, file, row.names = FALSE)
    }
  )
  output$location_Temp <- renderPlot({
    uploaded_file <- input$uploaded_data
    
    if(is.null(input$uploaded_data)) return(NULL)
    else{
      samp_data_prototypeT <- read.csv(uploaded_file$datapath, header = TRUE) 
      
      
      names(samp_data_prototypeT)[str_detect(names(samp_data_prototypeT),"point_location_CPX")] <- "point_location_CPX"
      samp_data_prototypeT <- mutate(samp_data_prototypeT,X_Fe_CPX = Nb_ions_Fe_CPX/(Nb_ions_Fe_CPX + Nb_ions_Mg_CPX))
      samp_data_prototypeT
      
      pressure <- input$pressure
      
      # Calculate the temperature with intermediate steps
      samp_data_prototypeT <- calc_Brey_Kohler_temp(df = samp_data_prototypeT, pressure = pressure)
      
      r_x_axis <- reactive({
        r_x_axis <- as.symbol(input$x_axis)
      })
      
      r_y_axis <- reactive({
        r_y_axis <- as.symbol(input$y_axis)
      })
      
      
      x_axis_names <- data.frame(column_names=c("point_location_CPX",
                                                "point_location_OPX",
                                                "Temp_Celsius",
                                                "X_Fe_OPX",
                                                "X_Fe_CPX",
                                                "point_number_OPX",
                                                "point_number_CPX"),
                                 x_axis_names=c("Location of Point CPX","Location of Point OPX",
                                                "Temperature in Celsius", "X of Fe OPX", "X of Fe CPX",
                                                "Point number OPX", "Point number CPX"))
      x_axis_label <- x_axis_names[x_axis_names$column_names==input$x_axis,"x_axis_names"]
      
      
      #DO for y-axis what you did for x-axis with this code"
      y_axis_names <- data.frame(column_names=c("point_location_CPX",
                                                "point_location_OPX",
                                                "Temp_Celsius",
                                                "X_Fe_OPX",
                                                "X_Fe_CPX",
                                                "point_number_OPX",
                                                "point_number_CPX"),
                                 y_axis_names=c("Location of Point CPX","Location of Point OPX",
                                                "Temperature in Celsius", "X of Fe OPX", "X of Fe CPX",
                                                "Point number OPX", "Point number CPX"))
      y_axis_label <- y_axis_names[y_axis_names$column_names==input$y_axis,"y_axis_names"]
      
      plot_1 <- ggplot(samp_data_prototypeT,aes(x= !!r_x_axis(), y = !!r_y_axis()))+
        geom_point()+
        theme_classic()+
        labs(x= x_axis_label, y= y_axis_label)+
        theme(axis.title = element_text(size = 20),
              axis.text = element_text(size=16))+
        geom_smooth(method="lm")
      plot_1
    }})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
