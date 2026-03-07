# global.R
library(shiny)
library(ggplot2)

#source("R/my_functions.R")
generate_alpha_ids <- function(n) {
  
  ids <- c()
  i <- 1
  
  while(length(ids) < n) {
    
    id <- ""
    x <- i
    
    while(x > 0) {
      x <- x - 1
      id <- paste0(LETTERS[(x %% 26) + 1], id)
      x <- x %/% 26
    }
    
    ids <- c(ids, id)
    i <- i + 1
  }
  
  return(ids)
}