library(shiny)
library(tidyverse)
library(plotly)
library(rvmethod)
library(shinylive)
library(httpuv)

## Data
Gammas <-  read_csv("OUTPUTS/ND_Gamma_Dec2023_BKnown.csv")

# ## Filtering
# TYPE <- "M1"
# Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
#   mutate(log_B = log10(B))
# 
# data <- Gammas_E$log_B
# 
# ## Optimise number of bins for B value
# #Plot range and Number of Bins
# Emin <- min(data)
# Emax <- max(data)
# range <- Emax - Emin
# ## Use sturges rule
# K <- round(1+3.322*log10(length(data)))

 
#Plot with theorised parameters
## c(max bin counts, Theorised mean, standard deviation)

## Calculated the weighted mean of the hist data (weighted standard deviation will need attention in future)
#mean_h <- sum(h$counts*h$mids)/length(data)

##### Need to do an optimiser on the number of bins and/or on the function parameters


#### Generating the data in the app
TYPE <- "E1"
Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
  mutate(log_B = log10(B), ln_B = log(B))

#K <- round(1+3.322*log10(nrow(Gammas_E)))
K <- 50 ## Set manually
h <- hist(Gammas_E$log_B, breaks = K)

## Initially set mean and sd via histogram mean and and sd
mean_logB <- sum(h$counts*h$mids)/(nrow(Gammas_E))
sd_logB <- sqrt( sum(  (h$counts*(h$mids- mean_logB)^2))/(nrow(Gammas_E)-1))

###### First re-optimise the mean and sd to fit the data
### Have to incremend counts by 1 since chisq produces division by zero error
Counts <- h$counts + 1

## Gaussian fit to optimize
func <- function(mean, sd, x) {
  max(h$counts)*gaussfunc(x, mean, sd)
}

#Chisquared optimisation method ## param[1] is mean_logB and param[2] is sd_logB
chisq2 <- function(param) { 
  # ## Rehistogram based on K (num)
  # K <- param[3]
  # h <- hist(Gammas_E$log_B, breaks = K)
  # Counts <- h$counts + 1
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
h <- hist(Gammas_E$log_B, breaks = K)
lines(xvals, yvals)
mean_string <- paste("Mean is:", signif(10^optim_mean_logB, 3), " w.u.")
text(0.7*min(Gammas_E$log_B), max(h$counts), mean_string)
sd_string <- paste0("sd is: +/- log(", signif(10^optim_sd_logB, 3), ") w.u.")
text(0.7*min(Gammas_E$log_B), 0.9*max(h$counts), sd_string)





# ### The bin values
# # Evals are logarithmic and set above
# Evals <- seq(log10(Emin), log10(Emax), (log10(Emax)-log10(Emin))/N_Eparts)
# Evals_invLog <- 10^Evals
# Emids <- rep(0, length(Evals))
# widths <- rep(0, length(Evals))
# #width=0.5
# for(i in 1:length(Evals)){
#   if(i==length(Evals)){
#     widths[i] <- ((Evals[i]))/2
#     Emids[i] <- ((Evals[i])) + widths[i]
#   } else {
#     widths[i] <- ((Evals[i+1])-(Evals[i]))/2
#     Emids[i] <- ((Evals[i])) + ((Evals[i+1])-(Evals[i]))/2
#   }
# }



###### plot used in the app #######
p2 <- plot_ly(Gammas_E, x=~log_B, type="histogram", nbinsx=round(K))%>%
  layout(
    xaxis = list(title="Bvalue, Weisskopt units"),
    yaxis = list(title="Count")
  ) %>%
  #add_lines(x=xvals2, y=Gauss(r1$par, xvals2))
  add_trace(x = xvals,
            y = yvals,
            type = "scatter",
            mode = "lines")
#name = Fit_String)
#line = list(color = "black", width = 10)
p2



# ### Fitting function for a single h tibble
# fit_gauss <- function(counts, log_Bvals, Bmids){
#   a <- c(max(counts),mean(log_Bvals), sd(log_Bvals))
#   #Counts <- h$counts
#   chisq1 <- function(vals) {return(sum((counts - Gauss(vals, x))^2/counts))}
#   r1 <- optim(a, chisq1, hessian=TRUE)
#   return(r1)
# }

yvals <- gaussfunc(h$mids, mean_logB, sd_logB)



p2 <- plot_ly(Gammas_E, x=~log_B, type="histogram", nbinsx=round(K))%>%
  layout(
    xaxis = list(title="Bvalue, Weisskopt units"),
    yaxis = list(title="Count")
  ) %>%
  #add_lines(x=xvals2, y=Gauss(r1$par, xvals2))
  add_trace(x = xvals,
            y = yvals,
            type = "scatter",
            mode = "lines")
#name = Fit_String)
#line = list(color = "black", width = 10)
p2
#### TEsting some app code ####


#Optimise Num_bins with theorised parameters
chisq0 <- function(a, Num_bins) { 
  h2 <- hist(data, breaks=Num_bins)
  return(sum((h2$counts - func1(a, x))^2/h2$counts)) 
}

#Goodness of fit check for number of bins optimisiation
Gof <- function(Num_bins){ 
  chisq <- chisq0(Th_a, Num_bins)
  return(chisq / (Num_bins - length(Th_a))) }


res <- optim(Num_bins, Gof)
print(res)

Num_bins <- res$par
sprintf("Optimum number of bins for 2 parameter fit: %f", res$par)
h <- hist(data, breaks=Num_bins)
Freq <- h$counts


