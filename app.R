
# 11-19-21 in the plot the point numbers are all mixed up and not in order 1-15.
# 11-19-21 how to make each different slider talk to the indiv plots

library(shiny)
library(tidyverse)
library(patchwork)


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
                                              "X_Fe_CPX",
                                              "point_number_OPX",
                                              "point_number_CPX"),
                        selected = "point_number_CPX"),
            
                        selectInput("y_axis", "y-axis data", choices = c("point_location_CPX",
                                              "point_location_OPX",
                                              "Temp_Celsius",
                                              "X_Fe_OPX",
                                              "X_Fe_CPX",
                                              "point_number_OPX",
                                              "point_number_CPX"),
                        selected = "Temp_Celsius"),
            selectInput("x_axis", "X-axis data", choices = c("point_location_CPX",
                                                             "point_location_OPX",
                                                             "Temp_Celsius",
                                                             "X_Fe_OPX",
                                                             "X_Fe_CPX",
                                                             "point_number_OPX",
                                                             "point_number_CPX"),
                        selected = "point_number_CPX"),
            
            selectInput("y_axis", "y-axis data", choices = c("point_location_CPX",
                                                             "point_location_OPX",
                                                             "Temp_Celsius",
                                                             "X_Fe_OPX",
                                                             "X_Fe_CPX",
                                                             "point_number_OPX",
                                                             "point_number_CPX"),
                        selected = "Temp_Celsius"),
            
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
                   3. point_number_CPX
                   4. point_number_OPX
                   5. Nb_ions_Mg_CPX
                   6 Nb_ions_Mg_OPX
                   7. Nb_ions_Fe_CPX
                   8 Nb_ions_Fe_OPX
                   9. Nb_ions_Ca_CPX
                   10. Nb_ions_Ca_OPX
                   11. Nb_ions_Na_CPX
                   12. Nb_ions_Na_OPX
                   upload you table in a csv format.
                   
                   You will need to clean and arrange your data from the microbrobe.
                   
                   your data number points for the microprobe are usually in the form: 2/1.
                   R will not recognize this form and you will need to re-number
                   them to a whole integer.
                   
                   You must use the Nb of ions from the microprobe data.
                   This code assumes you have used the correct analysis tool and will
                   treat values as correct."),
                 
                 img(src = "proper_format.jpg", height = 900, width = 1500))   
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
          if(ncol(samp_data_prototypeT)!= 12){
            print("Incorrect number of columns provided. Please see instructions")
          }
          else{
            
          
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
    output$location_Temp <- renderPlot({
      if(is.null(input$uploaded_data)) return(NULL)
      else{
        
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
                       "Point number CPX", "Point number OPX"))
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
                                              "Point number CPX", "Point number OPX"))
        y_axis_label <- y_axis_names[y_axis_names$column_names==input$y_axis,"y_axis_names"]
        
       
         plot_1 <- ggplot(samp_data_prototypeT,aes(x= !!r_x_axis(), y = !!r_y_axis()))+
      geom_point()+
          theme_classic()+
          labs(x= x_axis_label, y= y_axis_label)+
          geom_smooth(method="lm")
           
           plot_2 <- ggplot(samp_data_prototypeT,aes(x= !!r_x_axis(), y = !!r_y_axis()))+
           geom_point()+
           theme_classic()+
           labs(x= x_axis_label, y= y_axis_label)+
           geom_smooth(method="lm")
           
           plot_1+plot_2
          
      
    }})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
