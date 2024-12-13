library(rsconnect)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)  ## for data tables

Gammas <-  read_csv("OUTPUTS/ND_Gamma_Dec2023_BKnown.csv")
#Gammas <-  filter(Gammas, !is.na(B), B>0)
#write.table(Gammas, file="OUTPUTS/ND_Gamma_Dec2023_BKnown.csv", append=T, row.names=F, col.names=T,  sep=",")

Gauss <- function(a, x) { return(a[1]*exp((-(x-a[2])^2)/a[3])) }

### Fitting function for a single GHist (h) tibble
fit_gauss <- function(counts, log_Bvals, Bmids){
  a <- c(max(counts),mean(log_Bvals), sd(log_Bvals))
  #Counts <-h$counts
  chisq1 <- function(vals) {return(sum((counts - Gauss(vals, x))^2/counts))}
  r1 <- optim(a, chisq1, hessian=TRUE)
  return(r1)
}

## Filtering
TYPE <- "E2"
Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
   mutate(log_B = log10(B))
 
 
## Look at scatter plot of data
p1 <- plot_ly(Gammas_E, x = ~Egam, y = ~B, type = 'scatter', showlegend=F) %>%
   layout(yaxis = list(type="log", title = TeX("B Value, Weisskopf units"), exponentformat='E', tick0=floor(log10(min(Gammas_E$B))),
                       dtick=log10(1E1)), #range=c(log10(0),log10(15))),
          xaxis = list( type="log", tickangle = -45, title="Gamma Energy keV")) %>%
   config(mathjax = "cdn") #%>%
p1

################################################################################ 
##### Histogram parameters based on filtered data ##############################
################################################################################
## Initial guess of num_bins based on Sturge's rule
K<- 1+3.322*log10(nrow(Gammas_E))

GHist <- hist(Gammas_E$log_B, breaks = K)


###### FItting to a single histogram ###########################################
### Fitting function for a singleh tibble
fit_gauss <- function(counts, log_Bvals){
  a <- c(max(counts),mean(log_Bvals), sd(log_Bvals))
  #Counts <-h$counts
  chisq1 <- function(vals) {return(sum((counts - Gauss(a, log_Bvals))^2/counts))}
  r1 <- optim(a, chisq1, hessian=TRUE)
  return(r1)
}

fit_gauss(GHist$counts,h$mids)

mean(GHist$mids)

# #Load necessary library
# library(ggplot2)
# 
# # Generate random data
# data <- rnorm(1000)
# 
# # Create a histogram with Gaussian fit
# ggplot(data.frame(x = data), aes(x)) +
#   geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
#   stat_function(fun = dnorm, args = list(mean = mean(data), sd = sd(data)), color = "red", size = 1) +
#   labs(title = "Histogram with Gaussian Fit", x = "Value", y = "Density")

################################################################################


### For the number of energy partitions
Emin <- min(Gammas_E$Egam)
Emax <- max(Gammas_E$Egam)
N_Eparts <- 12

## Histogram slice
p2 <- plot_ly(Gammas_E, x=~log_B, type="histogram", nbinsx=round(K))%>%
    layout(
      xaxis = list(title="Bvalue, Weisskopt units"),
      yaxis = list(title="Count")
    )# %>%
  #add_lines(x=xvals2, y=Gauss(r1$par, xvals2))
p2


### The bin values
# Evals are logarithmic
Evals <- seq(log10(Emin), log10(Emax), (log10(Emax)-log10(Emin))/N_Eparts)
Evals_invLog <- 10^Evals
Emids <- rep(0, length(Evals))
widths <- rep(0, length(Evals))
#width=0.5
for(i in 1:length(Evals)){
  if(i==length(Evals)){
    widths[i] <- ((Evals[i]))/2
    Emids[i] <- ((Evals[i])) + widths[i]
  } else {
    widths[i] <- ((Evals[i+1])-(Evals[i]))/2
    Emids[i] <- ((Evals[i])) + ((Evals[i+1])-(Evals[i]))/2
  }
}

### Nested tibble that contains the separate histogram tibbles... essentially has the structure of an XML file
GHists <- tibble(
  Emids = Emids,
  data = list(
    tibble(w=double(1), x=double(1), y=double(1), z=double(1))
    #tibble(w=NULL, x=NULL, y=NULL, z=NULL)
  ),
  fit = list(
    tibble(x=double(1), y=double(1), z=double(1))
  )
)

K <- 9
i <- 1
while(i < length(Evals)){ 
  #i<-3
    G <- filter(Gammas, Egam >= Evals_invLog[i] , Egam < Evals_invLog[i+1], Mult_Single == TYPE) %>%
      mutate(log_B = log10(B))
    #K <- 2
    GH <- hist(G$log_B, breaks = K)
   GHists$data[[i]] <-  tibble(w=widths[i], x=Emids[i], y=GH$mids, z=GH$counts)
    
    ## Add fitted gauss data
    a <- c(max(GH$counts), mean(G$log_B), sd(G$log_B))
    #GHists$fit[[i]] <-  tibble(x=Emids[i], y=GH$mids, z=Gauss(a, GH$mids))
    GHists$fit[[i]] <-  tibble(max = max(GH$counts), mean = mean(G$log_B), sd = sd(G$log_B))
    i <- i + 1
}


# Define a function to add 3D bars
add_3Dbar <- function(p, x,y,z, width) {
  w <- width
  add_trace(p, type="mesh3d",
            x = c(x-w, x-w, x+w, x+w, x-w, x-w, x+w, x+w),
            y = c(y-w, y+w, y+w, y-w, y-w, y+w, y+w, y-w),
            z = c(0, 0, 0, 0, z, z, z, z),
            i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
            j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
            k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
            facecolor = rep(toRGB(viridisLite::inferno(6)), each = 2))
}



### Gaussian fit data
#fit_gauss <- function(counts, log_Bvals, GH){
fit_gauss(GHists$data[[3]]$z, GHists$data[[3]]$y)

# range(GHists$data[[3]]$y)
# #B_array <- seq(max(GHists$data[[3]]$y), max(GHists$data[[3]]$y), 0.1)
# B_array <- GHists$data[[3]]$y
# Fit_Data <- tibble(x=Emids[3], y=B_array, z=

# Draw the 3D histogram
fig <- plot_ly()
#for (k1 in 1:nrow(z_mtx)) {
  #for (k2 in 1:ncol(z_mtx)) {

### Draw the 3D bars
for(i in 1:length(Emids)){
 # i<-3
  for(j in 1:nrow(GHists$data[[i]])){
    fig <- fig %>% add_3Dbar(GHists$data[[i]]$x[j], 
                             GHists$data[[i]]$y[j], 
                             GHists$data[[i]]$z[j], 
                             GHists$data[[i]]$w[j])
  }
}

#### Legend text strings
#Fit_String <- 


### Add the fitlines
for(i in 1:(length(Emids)-1)){
  B_vals <- seq(min(GHists$data[[i]]$y), max(GHists$data[[i]]$y), 0.1)
  xval <- GHists$data[[i]]$x[1]
  
  Fit_String <- paste(c("At Energy", round(10^xval, 4), "keV", " Mean B value is ", round(10^GHists$fit[[i]]$mean, 4)), sep = "", collapse=" ")
  
  fig <- fig %>% add_trace(x = xval,
                         y = B_vals,
                         z = Gauss(c(GHists$fit[[i]]$max, GHists$fit[[i]]$mean, GHists$fit[[i]]$sd) , B_vals ),
                         type = "scatter3d",
                         mode = "lines",
                         name = Fit_String
                         #line = list(color = "black", width = 10)
  )
}  

fig %>%  layout( 
  scene=list(
    #legend = list(x=100,y=100,z=100),
    xaxis = list(title="log(Energy, keV)"),
    yaxis = list(title="log(B_value, W.u,)"),
    zaxis = list(title="Count")
  )
)

