library(shiny)
library(tidyverse)
library(plotly)
library(rvmethod)
library(shinylive)
library(httpuv)
library(ggplot2)

## Data
Gammas <-  read_csv("OUTPUTS/ND_Gamma_Dec2023_BKnown.csv")



Predict_B <- function(mult, Gammas){
  #### Generating the data in the app
  #TYPE <- mult
  TYPE <- "M1"
  Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
    mutate(log_B = log10(B), ln_B = log(B))
  
  #K <- round(1+3.322*log10(nrow(Gammas_E)))
  #K <- 20 ## Set manually ... actually using freedman draconis for now
  h <- hist(Gammas_E$log_B, breaks = "FD")
  
  ## Initially set mean and sd via histogram mean and and sd
  mean_logB <- sum(h$counts*h$mids)/(nrow(Gammas_E))
  sd_logB <- sqrt( sum(  (h$counts*(h$mids- mean_logB)^2))/(nrow(Gammas_E)-1))
  
  ###### First optimise the mean and sd to fit the data
  ### Have to increment counts by 1 since chisq produces division by zero error
  Counts <- h$counts + 1
  
  ## Gaussian fit to optimize
  func <- function(mean, sd, x) {
    max(h$counts)*gaussfunc(x, mean, sd)
  }
  
  #Chisquared optimisation method ## param[1] is mean_logB and param[2] is sd_logB
  chisq2 <- function(param) { 
    return( sum((Counts - func(param[1], param[2], h$mids))^2 / Counts) ) 
  }
  
  ### Parameters for the optimisation
  params <- c(mean_logB, sd_logB)
  ## Optimisation
  optim_mean_logB <- optim(params, chisq2, hessian=TRUE)$par[1]
  optim_sd_logB <- optim(params, chisq2, hessian=TRUE)$par[2]
  
  
  ### Set X and Y vals for plotting of gaussian
  xvals <- seq(min(h$mids), max(h$mids), 0.1)
  yvals <- max(h$counts)*gaussfunc(xvals, optim_mean_logB, optim_sd_logB)
  
  ## Final plotting
  h <- hist(Gammas_E$log_B, breaks = "FD", xlab="Log(B-value, w.u.)",
            main ="Histogram and Gaussian fit of logarthmic B-values")
  lines(xvals, yvals)
  mean_string <- paste("Mean is:", signif(10^optim_mean_logB, 3), " w.u.")
  text(0.7*min(Gammas_E$log_B), max(h$counts), mean_string)
  sd_string <- paste0("sd is: +/- log(", signif(10^optim_sd_logB, 3), ") w.u.")
  text(0.7*min(Gammas_E$log_B), 0.9*max(h$counts), sd_string)
  
  # ## Try plotting with ggplot
  # h <- hist(Gammas_E$log_B, breaks = "FD", plot=FALSE)
  # ggplot(Gammas_E, aes(x=log_B)) + 
  #   geom_histogram(bins=length(h$counts)) +
  #   geom_line(aes(x=xvals, y=yvals))
    
  return(10^optim_mean_logB)
  
}


Predict_B("E1", Gammas)





