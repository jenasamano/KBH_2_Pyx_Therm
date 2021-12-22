##' Calculates Brey and Kohler 2-pyroxene thermometer 
##' 
##' I'm like 80 percent sure this is correct
##' 
##' @export
##' 
##' @param df A data frame containing appropriately-formatted microprobe data
##' @value The same data frame, with temperature data added
##' 
calc_Brey_Kohler_temp <- function(df, pressure) {
  
  df <- df %>% mutate(
    X_Fe_CPX = Nb_ions_Fe_CPX/(Nb_ions_Fe_CPX + Nb_ions_Mg_CPX),
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
    Temp_Celsius = Temp_Kelvins-273.15) #%>% 
    #filter(!is.na(Temp_Kelvins))
  
  df
}