# Add standard deviation info 11/29, 2/17/22
#Following error occur. Calcs for temps occur but no plot
# Listening on http://127.0.0.1:6360
#Warning: Error in data.frame: argument is missing, with no default
#169: data.frame
#168: renderPlot [C:\Users\taraw\OneDrive\Desktop\KBH_2_Pyx_Therm/app.R#212]




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
            selectInput("x_axis", "Left Graph X-axis data", choices = c("Distance_cpx",
                                                                        "Distance_opx"),
                        selected = "Distance_cpx"),
            
                        selectInput("y_axis", "Left Graph y-axis data", choices = c("Temp_Kelvins",
                                                                                    "Temp_Celsius"),
                        selected = "Temp_Celsius"),
            
            
            radioButtons("full_table", "Show Full Table of Calulations",
                         c("yes"= "yes",
                           "no" = "no"), selected = "no"),
            downloadButton("download_data","Download Temperature in CSV")
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
          if(ncol(samp_data_prototypeT)!= 14){
            print("Incorrect number of columns provided. Please see instructions")
          }
          else{
            
            Pressure <- input$Pressure
          names(samp_data_prototypeT)[str_detect(names(samp_data_prototypeT),"Distance_cpx")] <- "Distance_cpx"
       samp_data_prototypeT <- mutate(samp_data_prototypeT, 
                                       X_Fe_CPX = Fe_cpx/(Fe_cpx + Mg_cpx),        
                                       X_Fe_OPX = Fe_opx/(Fe_opx + Mg_opx),
                                       Ca_star_CPX = Ca_cpx/(1-Na_cpx),
                                       Ca_star_OPX = Ca_opx/(1-Na_opx),
                                       K_sub_D = (1-Ca_star_CPX)/(1-Ca_star_OPX),
                                       onehundredtwentysix_times_X_Fe_CPX=126.3*X_Fe_CPX,
                                       plustwentyfour = onehundredtwentysix_times_X_Fe_CPX+24.9,
                                       times_pressure = plustwentyfour*10,
                                       T_bacon_numerator = 23664 + times_pressure,
                                       ln_K_sub_D = log(K_sub_D),
                                       ln_K_sub_D_SQRD = ln_K_sub_D^2,
                                       T_bacon_denominator = 13.38 + ln_K_sub_D_SQRD + (11.59 * X_Fe_OPX),
                                       Temp_Kelvins = T_bacon_numerator/T_bacon_denominator,
                                       Temp_Celsius = Temp_Kelvins-273.15) %>% 
          filter(!is.na(Temp_Kelvins))
        samp_data_prototypeT
        
        display_table <- select(samp_data_prototypeT,
                                Temp_Celsius,
                                Temp_Kelvins,
                                Distance_cpx,
                                Distance_opx)
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
          
          
          names(samp_data_prototypeT)[str_detect(names(samp_data_prototypeT), "Distance_opx")] <-
            "Distance_opx"
          
          Pressure <- input$Pressure
          
          samp_data_prototypeT <- mutate(samp_data_prototypeT,
                                         X_Fe_CPX = Fe_cpx/(Fe_cpx + Mg_cpx),        
                                         X_Fe_OPX = Fe_opx/(Fe_opx + Mg_opx),
                                         Ca_star_CPX = Ca_cpx/(1-Na_cpx),
                                         Ca_star_OPX = Ca_opx/(1-Na_opx),
                                         K_sub_D = (1-Ca_star_CPX)/(1-Ca_star_OPX),
                                         onehundredtwentysix_times_X_Fe_CPX=126.3*X_Fe_CPX,
                                         plustwentyfour = onehundredtwentysix_times_X_Fe_CPX+24.9,
                                         times_pressure = plustwentyfour*10,
                                         T_bacon_numerator = 23664 + times_pressure,
                                         ln_K_sub_D = log(K_sub_D),
                                         ln_K_sub_D_SQRD = ln_K_sub_D^2,
                                         T_bacon_denominator = 13.38 + ln_K_sub_D_SQRD + (11.59 * X_Fe_OPX),
                                         Temp_Kelvins = T_bacon_numerator/T_bacon_denominator,
                                         Temp_Celsius = Temp_Kelvins-273.15) %>% 
          filter(!is.na(Temp_Kelvins))}
          write.csv(samp_data_prototypeT, file, row.names = FALSE)
        }
        )
    output$location_Temp <- renderPlot({
      uploaded_file <- input$uploaded_data
      
      if(is.null(input$uploaded_data)) return(NULL)
      else{
        samp_data_prototypeT <- read.csv(uploaded_file$datapath, header = TRUE) 
        
        Pressure <- input$Pressure
          
          names(samp_data_prototypeT)[str_detect(names(samp_data_prototypeT),"Distance_cpx")] <- "Distance_cpx"
          samp_data_prototypeT <- mutate(new_tble_format_temp_tool,
                                         X_Fe_CPX = Fe_cpx/(Fe_cpx + Mg_cpx),        
                                         X_Fe_OPX = Fe_opx/(Fe_opx + Mg_opx),
                                         Ca_star_CPX = Ca_cpx/(1-Na_cpx),
                                         Ca_star_OPX = Ca_opx/(1-Na_opx),
                                         K_sub_D = (1-Ca_star_CPX)/(1-Ca_star_OPX),
                                         onehundredtwentysix_times_X_Fe_CPX=126.3*X_Fe_CPX,
                                         plustwentyfour = onehundredtwentysix_times_X_Fe_CPX+24.9,
                                         times_pressure = plustwentyfour*10,
                                         T_bacon_numerator = 23664 + times_pressure,
                                         ln_K_sub_D = log(K_sub_D),
                                         ln_K_sub_D_SQRD = ln_K_sub_D^2,
                                         T_bacon_denominator = 13.38 + ln_K_sub_D_SQRD + (11.59 * X_Fe_OPX),
                                         Temp_Kelvins = T_bacon_numerator/T_bacon_denominator,
                                         Temp_Celsius = Temp_Kelvins-273.15
                                         )
    
        
        r_x_axis <- reactive({
          r_x_axis <- as.symbol(input$x_axis)
         
        })
        
        r_y_axis <- reactive({
          r_y_axis <- as.symbol(input$y_axis)
          
          })
        
       
        x_axis_names <- data.frame(column_names=c("Distance_cpx",
          "Distance_opx"),
            )
         x_axis_names=c("Distance in microns of cpx values","Distance in microns of opx values",
                       )
        x_axis_label <- x_axis_names[x_axis_names$column_names==input$x_axis,"x_axis_names"]
        
       
        #DO for y-axis what you did for x-axis with this code"
        y_axis_names <- data.frame(column_names=c("Temp_Kelvins",
                                                "Temp_Celsius"),
                                                )
                                 y_axis_names=c("Temperature in Celsius", "Temperature in Kelvins")
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
