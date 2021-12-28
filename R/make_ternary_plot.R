##' Make a ternary plot
##' 
##' Currently generates fake data
##' 
make_ternary_plot <- function(df) {

  #browser()
  
  color <- sample(colors(), size=1)
  
  fake_data <- as.data.frame(lapply(1:3, rnorm, n=10), col.names = c("x", "y", "z"))
  
  p <- ggtern::ggtern(fake_data, aes(x=x, y=y, z=z)) + 
    geom_point(color = color) + 
    theme_classic()
  
  p
  
}