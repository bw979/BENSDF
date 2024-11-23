library(shiny)
library(tidyverse)
library(plotly)
library(rvmethod)
library(shinylive)
library(httpuv)

Gammas <-  read_csv("OUTPUTS/ND_Gamma_Dec2023_BKnown.csv")

Gauss <- function(a, x) { return(a[1]*exp((-(x-a[2])^2)/a[3])) }

TYPE <- "M1"

if(TYPE=="M1"){
  ## Define a function to add 3D bars
  add_3Dbar <- function(p, x,y,z, width, width2) {
    w <- width
    bw <- width2
    add_trace(p, type="mesh3d",
              x = c(x-w, x-w, x+w, x+w, x-w, x-w, x+w, x+w),
              y = c(y-bw, y+bw, y+bw, y-bw, y-bw, y+bw, y+bw, y-bw),
              z = c(0, 0, 0, 0, z, z, z, z),
              i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
              j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
              k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
              facecolor = rep(toRGB(viridisLite::turbo(6)), each = 2))
  }
  ## Initial setting of number of Ebins and Bvalue bins respectively
  N_Eparts <- 10
  K <- 30
} else if(TYPE=="M2"){
  ## Define a function to add 3D bars
  add_3Dbar <- function(p, x,y,z, width, width2) {
    w <- width
    bw <- width2
    add_trace(p, type="mesh3d",
              x = c(x-w, x-w, x+w, x+w, x-w, x-w, x+w, x+w),
              y = c(y-bw, y+bw, y+bw, y-bw, y-bw, y+bw, y+bw, y-bw),
              z = c(0, 0, 0, 0, z, z, z, z),
              i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
              j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
              k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
              facecolor = rep(toRGB(viridisLite::inferno(6)), each = 2))
  }
  ## Initial setting of number of Ebins and Bvalue bins respectively
  N_Eparts <- 12
  K <- 30
} else if(TYPE=="E1"){
  ## Define a function to add 3D bars
  add_3Dbar <- function(p, x,y,z, width, width2) {
    w <- width
    bw <- width2
    add_trace(p, type="mesh3d",
              x = c(x-w, x-w, x+w, x+w, x-w, x-w, x+w, x+w),
              y = c(y-bw, y+bw, y+bw, y-bw, y-bw, y+bw, y+bw, y-bw),
              z = c(0, 0, 0, 0, z, z, z, z),
              i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
              j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
              k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
              facecolor = rep(toRGB(viridisLite::rocket(6)), each = 2))
  }
  ## Initial setting of number of Ebins and Bvalue bins respectively
  N_Eparts <- 7
  K <- 30
} else if(TYPE=="E2"){
  ## Define a function to add 3D bars
  add_3Dbar <- function(p, x,y,z, width, width2) {
    w <- width
    bw <- width2
    add_trace(p, type="mesh3d",
              x = c(x-w, x-w, x+w, x+w, x-w, x-w, x+w, x+w),
              y = c(y-bw, y+bw, y+bw, y-bw, y-bw, y+bw, y+bw, y-bw),
              z = c(0, 0, 0, 0, z, z, z, z),
              i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
              j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
              k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
              facecolor = rep(toRGB(viridisLite::mako(6)), each = 2))
  }
  ## Initial setting of number of Ebins and Bvalue bins respectively
  N_Eparts <- 20
  K <- 30
}




Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
  mutate(log_B = log(B))

################################################################################ 
##### Histogram parameters based on filtered data ##############################
################################################################################

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
    tibble(w=double(1), bw=double(1), x=double(1), y=double(1), z=double(1))
    #tibble(w=NULL, x=NULL, y=NULL, z=NULL)
  ),
  fit = list(
    tibble(x=double(1), y=double(1), z=double(1))
  )
)

### Fills the nested tibble with B-value histograms for each E-value bin
bwidths <- rep(0, length(Evals))
# Kstep
# Kvec <- seq(min(G$log_B), max(G$log_B), )
i <- 1
while(i < length(Evals)){ 
  #i<-6
  G <- filter(Gammas, Egam >= Evals_invLog[i] , Egam < Evals_invLog[i+1], Mult_Single == TYPE) %>%
    mutate(log_B = log(B))
 
  
  ### Histogram the current set of Bvalue data
  GH <- hist(G$log_B, breaks = K)
  
  ## Get the Bvalue widths from this histogram
  bwidths[i] <- 0.5*(max(GH$mids) - min(GH$mids))/(length(GH$mids) -1)
  
  ## Add histogram data
  GHists$data[[i]] <-  tibble(w=widths[i], bw = bwidths[i], x=Emids[i], y=GH$mids, z=GH$counts)
  
  ## Add fitted gauss data
  a <- c(max(GH$counts), mean(G$log_B), sd(G$log_B))
  #GHists$fit[[i]] <-  tibble(x=Emids[i], y=GH$mids, z=Gauss(a, GH$mids))
  GHists$fit[[i]] <-  tibble(max = max(GH$counts), mean = mean(G$log_B), sd = sd(G$log_B))
  i <- i + 1
}

################################################################################
#########  3D histogram using plotly 3D_bar function ###########################
################################################################################

  fig <- plot_ly(showlegend=F, height="600")
  ### Draw the 3D bars
  for(i in 1:length(Emids)){
    # i<-3
    for(j in 1:nrow(GHists$data[[i]])){
      fig <- fig %>% add_3Dbar(GHists$data[[i]]$x[j], 
                               GHists$data[[i]]$y[j], 
                               GHists$data[[i]]$z[j], 
                               GHists$data[[i]]$w[j],
                               GHists$data[[i]]$bw[j])
    }
  }
  
  # ### Add the fitlines
  # for(i in 1:length(Emids)){
  #   B_vals <- seq(min(GHists$data[[i]]$y), max(GHists$data[[i]]$y), 0.1)
  #   xval <- GHists$data[[i]]$x[1]
  #   fig <- fig %>% add_trace(x = xval,
  #                            y = B_vals,
  #                            z = Gauss(c(GHists$fit[[i]]$max, GHists$fit[[i]]$mean, GHists$fit[[i]]$sd) , B_vals ),
  #                            type = "scatter3d",
  #                            mode = "lines",
  #                            name = "Hello"
  #                           #line = list(color = "black", width = 10)
  #   )
  # }
  
  ### Add the fitlines and label
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
      xaxis = list(title="log(Energy, keV)"),
      yaxis = list(title="log(B_value, W.u,)"),
      zaxis = list(title="Count")
    )
  )
  