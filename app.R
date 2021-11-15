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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2 Pyroxene Thermometer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Pressure", "Pressure in kilobars:", min = 1, max = 20, value = 1),
            fileInput("uploaded_data", "Upload Electron Microprobe data", accept = ".csv")
        ),

        # Show a plot of the generated distribution
        mainPanel("results_go_here")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$contents <-renderTable({
        uploaded_file <- input$uploaded_data
        if(is.null(uploaded_file)) return(NULL)
        read.csv(uploaded_file$datapath)
        
    })
    
    output$therm_output <-renderTable({
        samp_data_prototypeT <- input$uploaded_data
        
        samp_data_prototypeT <- mutate(samp_data_prototypeT,X_Fe_CPX=Nb_ions_Fe_CPX/(Nb_ions_Fe_CPX+Nb_ions_Mg_CPX)) 
        samp_data_prototypeT
        
        Pressure <- 10
        
        samp_data_prototypeT <- mutate(samp_data_prototypeT,
                                       X_Fe_OPX = Nb_ions_Fe_OPX/(Nb_ions_Fe_OPX+Nb_ions_Mg_OPX),
                                       Ca_star_CPX = Nb_ions_Ca_CPX/(1-Nb_ions_Na_CPX),
                                       Ca_star_OPX = Nb_ions_Ca_OPX/(1-Nb_ions_Na_OPX),
                                       K_sub_D = (1-Ca_star_CPX)/(1-Ca_star_OPX),
                                       onehundredtwentysix_times_X_Fe_CPX=126.3*X_Fe_CPX,
                                       plustwentyfour = onehundredtwentysix_times_X_Fe_CPX+24.9,
                                       times_pressure = plustwentyfour*Pressure,
                                       T_bacon_numerator = 23664+times_pressure,
                                       ln_K_sub_D = log(K_sub_D),
                                       ln_K_sub_D_SQRD = ln_K_sub_D^2,
                                       T_bacon_denominator = 13.38+ln_K_sub_D_SQRD+(11.59*X_Fe_OPX),
                                       Temp_Kelvins = T_bacon_numerator/T_bacon_denominator,
                                       Temp_Celsius = Temp_Kelvins-273.15)
        
        renderTable({ samp_data_prototypeT })
      
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
