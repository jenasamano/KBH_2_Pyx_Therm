##'
##'
##' @param 
##' @param 
##' 
process_raw_data <- function(filename, method) {
  
  if(is.null(filename)) {
    return(NULL) # There's a better way to handle the startup case 
  }
  
  d <- readr::read_csv(filename)
  
  processing_function <- switch(method, 
                                "Brey-Kohler" = calc_Brey_Kohler_temp,
                                stop("Invalid thermometer"))
  
  processed_data <- processing_function(d)
  
  d
}