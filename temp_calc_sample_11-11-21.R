# 2 Pyroxene Thermometer

library(readr)
library(tidyverse)
samp_data_prototypeT <- read_csv("samp_data-prototypeT.csv")
View(samp_data_prototypeT)

Pressure <- 10

samp_data_prototypeT <- mutate(samp_data_prototypeT,
                               X_Fe_CPX = Nb_ions_Fe_CPX / (Nb_ions_Fe_CPX + Nb_ions_Mg_CPX),
                               X_Fe_OPX = Nb_ions_Fe_OPX / (Nb_ions_Fe_OPX + Nb_ions_Mg_OPX),
                               Ca_star_CPX = Nb_ions_Ca_CPX / (1 - Nb_ions_Na_CPX),
                               Ca_star_OPX = Nb_ions_Ca_OPX / (1 - Nb_ions_Na_OPX),
                               K_sub_D = (1 - Ca_star_CPX) / (1 - Ca_star_OPX),
                               const_3 = 126.3 * X_Fe_CPX,
                               const_2 = const_3 + 24.9,
                               mult_P = const_2 * Pressure,
                               T_bacon_numerator = 23664 + mult_P,
                               ln_K_sub_D = log(K_sub_D),
                               ln_val_sqrd = ln_K_sub_D^2,
                               T_bacon_denominator = 13.38 + ln_val_sqrd + (11.59 * X_Fe_OPX),
                               Temp_Kelvins = T_bacon_numerator / T_bacon_denominator,
                               Temp_Celsius = Temp_Kelvins - 273.15)
samp_data_prototypeT

view(samp_data_prototypeT)

                               

                               
                               
                               

                               
                               
                               


                               

                               
                          





