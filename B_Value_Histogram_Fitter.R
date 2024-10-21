library(rsconnect)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)  

## Data
Gammas <-  read_csv("OUTPUTS/ND_Gamma_Dec2023_BKnown.csv")

## Filtering
TYPE <- "E2"
Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
  mutate(log_B = log10(B))

data <- Gammas_E$log_B

## Optimise number of bins for B value
#Plot range and Number of Bins
mi <- min(data)
ma <- max(data)
range <- ma - mi
## Use sturges rule
Num_bins <- round(1+3.322*log10(length(data)))

#Stepsize
step <- range / (Num_bins - 1)
Bins <- seq(mi, ma, step)

h <- hist(data, breaks=Num_bins)
Freq <- h$counts

#Plot with theorised parameters
## c(max bin counts, Theorised mean, standard deviation)

## Calculated the weighted mean of the hist data (weighted standard deviation will need attention in future)
mean_h <- sum(h$counts*h$mids)/length(data)

Th_a <- c(max(Freq), mean_h, sd(h$mids))
x <- h$mids
y <- Th_a[1]*exp((-(x-Th_a[2])^2)/Th_a[3])
func1 <- function(a, x) {
  return(a[1]*exp((-(x-a[2])^2)/a[3]))
}
lines(x,y)



#### TEsting some app code ####
K <- round(1+3.322*log10(nrow(Gammas_E)))
h <- hist(Gammas_E$log_B, breaks = K)
xvals <- h$mids
mean_logB <- sum(h$counts*h$mids)/(nrow(Gammas_E))
sd_logB <- sd(h$mids)

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


