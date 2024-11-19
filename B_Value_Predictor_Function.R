library(tidyverse)
library(stringr)
library(plotly)
library(XML)
library(processx)
library(pbapply)
library(RColorBrewer)


Gammas <-  read_csv("OUTPUTS/ND_Gamma_Dec2023_BKnown.csv")


Gauss <- function(a, x) { 
  return(a[1]*exp((-(x-a[2])^2)/a[3])) 
}

### Fitting function for a single GHist tibble
fit_gauss <- function(counts, log_Bvals, Bmids){
  a <- c(max(counts),mean(log_Bvals), sd(log_Bvals))
  #Counts <- GHist$counts
  chisq1 <- function(vals) {return(sum((counts - Gauss(vals, x))^2/counts))}
  r1 <- optim(a, chisq1, hessian=TRUE)
  return(r1)
}


#### Just have one slice and fill a histogram between energy ranges and within a mass array

Predict_B <- function(mult, Gammas, N_Eparts, Emin, Emax, Mass_array){
  
  ## Filtering
  TYPE <- mult
  
  Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
    mutate(log_B = log10(B))
  
  ################################################################################ 
  ##### Histogram parameters based on filtered data ##############################
  ################################################################################
  
  ## Initial guess of num_bins based on Sturge's rule
  K<- 1+3.322*log10(nrow(Gammas_E))
  GHist <- hist(Gammas_E$log_B, breaks = K)
  
  ### For the number of energy partitions
  Emin <- min(Gammas_E$Egam)
  Emax <- max(Gammas_E$Egam)

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
  
  ### Fits the Gaussians
  K <- 100
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
  
 ### Find the maximum counts value in this GHists$fit[[i]]$max
 for(i in 1:(length(GHists$fit)-1)){
   print(i)
    if(i==1){
      MAx <- GHists$fit[[i]]$max
    } else {
     MAx <- GHists$fit[[i-1]]$max
   }
   if(GHists$fit[[i]]$max >= MAx ){
     MAx_save <- GHists$fit[[i]]$max
   }
 }  
  
  # ### Fitting function for a single GHist tibble
  # fit_gauss <- function(counts, log_Bvals, Bmids){
  #   a <- c(max(counts),mean(log_Bvals), sd(log_Bvals))
  #   #Counts <- GHist$counts
  #   chisq1 <- function(vals) {return(sum((counts - Gauss(vals, x))^2/counts))}
  #   r1 <- optim(a, chisq1, hessian=TRUE)
  #   return(r1)
  # }
 xvals <- seq(min(Gammas_E$log_B), max(Gammas_E$log_B), ((max(Gammas_E$log_B)-min(Gammas_E$log_B))/(length(Gammas_E$log_B))))
 #### Make a nice plotly plot output
 i<- 1
 
 # OutPlot <- plot_ly(name="E1") %>%
 #   add_histogram(Gammas_E$log_B, marker=list(color="orange"), showlegend=T) %>%
 #   add_lines( x=xvals, y=9*MAx_save*dnorm(xvals,  GHists$fit[[i]]$mean, GHists$fit[[i]]$sd), showlegend=F) %>%
 #   layout(
 #     xaxis=list(title="log(B-value)"),
 #     yaxis=list(title="Count")
 #    
 #   )
  

 #if(Plotting==F){
   return(10^(GHists$fit[[i]]$mean))
# } else {
  # return(OutPlot)
 #}
  
}


M2 <- Predict_B("M2", Gammas, T)


Predict_B <- function(mult, Gammas, N_Eparts, Emin, Emax, Mass_array)


E1
E2
M1
M2

subplot(E1, E2, M1, M2, nrows=2, shareX=FALSE ) %>%
  layout(
    xaxis=list(title="log(B-value)"),
    yaxis=list(title="Count")
    
  )


  #dnorm(x, mean, sd)
xv <- 
dnorm(seq(-10, 5, 0.1), 1, 1)

plot_ly() %>%
  add_histogram(Gammas_E$log_B) %>%
  add_lines( x=seq(-10, 5, 0.1), y=max(GHists$fit[[1]]$max)*dnorm(seq(-10, 5, 0.1),  GHists$fit[[i]]$mean, GHists$fit[[i]]$sd))

plot( x=seq(-10, 5, 0.1), y=dnorm(seq(-10, 5, 0.1),  GHists$fit[[i]]$mean, GHists$fit[[i]]$sd))
)
GHists$fit[[8]]$max
