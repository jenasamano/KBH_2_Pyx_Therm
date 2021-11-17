#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(flexdashboard)
library(tidyverse)
library(janitor)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2 Pyroxene Thermometer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Pressure", "Pressure in kilobars:", min = 1, max = 20, value = 1),
            fileInput("uploaded_data", "Upload Electron Microprobe data", accept = ".csv"),
            selectInput("x_axis", "X-axis data", choices = c("point_location_CPX",
                                              "point_location_OPX",
                                              "Temp_Celsius",
                                              "X_Fe_OPX",
                                              "X_Fe_CPX"),
                        selected = "Temp_Celsius"),
            
            selectInput("y_axis", "y-axis data", choices = c("point_location_CPX",
                                              "point_location_OPX",
                                              "Temp_Celsius",
                                              "X_Fe_OPX",
                                              "X_Fe_CPX"),
                        selected = "point_location_CPX"),
            
            radioButtons("full_table", "Show Full Table of Calulations",
                         c("yes"= "yes",
                           "no" = "no"), selected = "no")
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
        tabPanel("Instructions", 
                 p("It is vital your uploaded table is in the same format as the table shown here. 
                   Label you columns exactly as shown. 
                   The calculations will not execute properly unless you do this step.
                   For your convenience the column names are listed here: 
                   1. point_location_CPX
                   2. point_location_OPX
                   3. Nb_ions_Mg_CPX
                   4. Nb_ions_Mg_OPX
                   5. Nb_ions_Fe_CPX
                   6. Nb_ions_Fe_OPX
                   7. Nb_ions_Ca_CPX
                   8. Nb_ions_Ca_OPX
                   9. Nb_ions_Na_CPX
                   10. Nb_ions_Na_OPX
                   upload you table in a csv format.
                   "),
                 img(src = "batman_and_ace.jpg", height = 500, width = 500))   
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
    
    output$samp_data_prototypeT <-renderTable({
      
        uploaded_file <- input$uploaded_data
        
        if(is.null(input$uploaded_data)) return(NULL)
        else{
          samp_data_prototypeT <- read.csv(uploaded_file$datapath, header = TRUE) 
          names(samp_data_prototypeT)[str_detect(names(samp_data_prototypeT),"point_location_CPX")] <- "point_location_CPX"
       samp_data_prototypeT <- mutate(samp_data_prototypeT,X_Fe_CPX = Nb_ions_Fe_CPX/(Nb_ions_Fe_CPX + Nb_ions_Mg_CPX))
        samp_data_prototypeT

        Pressure <- input$Pressure

        samp_data_prototypeT <- mutate(samp_data_prototypeT,
                                       X_Fe_OPX = Nb_ions_Fe_OPX/(Nb_ions_Fe_OPX + Nb_ions_Mg_OPX),
                                       Ca_star_CPX = Nb_ions_Ca_CPX/(1-Nb_ions_Na_CPX),
                                       Ca_star_OPX = Nb_ions_Ca_OPX/(1-Nb_ions_Na_OPX),
                                       K_sub_D = (1-Ca_star_CPX)/(1-Ca_star_OPX),
                                       onehundredtwentysix_times_X_Fe_CPX=126.3*X_Fe_CPX,
                                       plustwentyfour = onehundredtwentysix_times_X_Fe_CPX+24.9,
                                       times_pressure = plustwentyfour*Pressure,
                                       T_bacon_numerator = 23664 + times_pressure,
                                       ln_K_sub_D = log(K_sub_D),
                                       ln_K_sub_D_SQRD = ln_K_sub_D^2,
                                       T_bacon_denominator = 13.38 + ln_K_sub_D_SQRD + (11.59 * X_Fe_OPX),
                                       Temp_Kelvins = T_bacon_numerator/T_bacon_denominator,
                                       Temp_Celsius = Temp_Kelvins-273.15)
        
        display_table <- select(samp_data_prototypeT,
                                point_location_CPX,
                                point_location_OPX,
                                Temp_Celsius,
                                Temp_Kelvins,
                                X_Fe_CPX,
                                X_Fe_OPX)
        if(input$full_table=="yes") samp_data_prototypeT
        else display_table
        }    
    })
    output$location_Temp <- renderPlot({
      if(is.null(input$uploaded_data)) return(NULL)
      else{
        
        r_x_axis <- reactive({
          r_x_axis <- as.symbol(input$x_axis)
         
        })
        
        r_y_axis <- reactive({
          r_y_axis <- as.symbol(input$y_axis)
          
        })
        
        ggplot(samp_data_prototypeT,aes(x= !!r_x_axis(), y = !!r_y_axis()))+
      geom_point()+
          geom_smooth(method="lm")
      
    }})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
