##'
##'
##' @param 
##' @param 
##' 
process_raw_data <- function(filename, method, pressure) {
  
  if(is.null(filename)) {
    return(NULL) # There's a better way to handle the startup case 
  }
  warning("I got past the is.null(filename) test")
  d <- readr::read_csv(filename)
  
  processing_function <- switch(method, 
                                "Brey-Kohler" = calc_Brey_Kohler_temp,
                                stop("Invalid thermometer"))
  
  processed_data <- processing_function(d, pressure)
  
  processed_data
}