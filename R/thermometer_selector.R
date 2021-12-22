thermometer_selector <- function(inputId, label) {
  radioButtons(inputId = inputId, 
               label = label, 
               choices = c("Brey-Kohler"),
               selected = "Brey-Kohler")
}