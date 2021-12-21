show_full_calcs_selector <- function() {
  selectInput("x_axis", "Left Graph X-axis data", choices = c("point_location_CPX",
                                                              "point_location_OPX",
                                                              "Temp_Celsius",
                                                              "X_Fe_OPX",
                                                              "X_Fe_CPX",
                                                              "point_number_OPX",
                                                              "point_number_CPX"),
              selected = "point_number_CPX")
}