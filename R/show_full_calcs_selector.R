show_full_calcs_selector <- function(inputId, label) {
  radioButtons(inputId, label,
               c("yes"= "yes",
                 "no" = "no"), selected = "no")
}