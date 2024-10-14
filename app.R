library(shiny)
library(tidyverse)
library(plotly)

Gammas <-  read_csv("OUTPUTS/ND_Gamma_Dec2023_BKnown.csv")

## Gauss function, input vector a=c[1,2,3] to fit and data x value 
Gauss <- function(a, x) { return(a[1]*exp((-(x-a[2])^2)/a[3])) }

## Define a function to add 3D bars
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

ui <- fluidPage(
  titlePanel("Gamma Ray Predictor"),
  # Create a tabbed panel
  tabsetPanel(
    tabPanel("Explore",
             h3("Welcome to Tab 1"),
             p("This is the content of the first tab."),
             sidebarLayout(
               sidebarPanel(
                 #selectInput("Multipol", "Multipolarity:", choices=levels(as.factor(Gammas$Mult_Single)), selected="M1"),
                 selectInput("Multipol", "Multipolarity:", choices=c("M1", "M2", "E1", "E2"), selected="M1"),
                 numericInput("N_Ebins", "Number of Energy Bins:", value = 5),
                 actionButton("plotButton", "PLOT")
               ),
               mainPanel(
                 plotlyOutput("plot")
               )
              )
    ),
    
    ## This tab filters based on selecting a mass range or input list of masses
    tabPanel("Evaluate",
             h3("Welcome to Tab 2"),
             p("This is the content of the second tab."),
             # Set energy range
             sidebarLayout(
             sidebarPanel(
               selectInput("Multipol2", "Multipolarity:", choices=c("M1", "M2", "E1", "E2"), selected="M1"),
               splitLayout(cellWidths = c("50%", "50%"), 
                         numericInput(inputId = "E_min", label = "Min Energy (keV):", value=0),
                         numericInput(inputId = "E_max", label = "Max Energy (keV):", value=max(Gammas$Egam))),
               # select odd-odd, even-even, or all
               radioButtons(inputId="Mass_type", label="Type of Mass number?", 
                          choices=c("Odd-Odd","Even-Even", "Odd Mass", "All"), selected="All"),
               # Set Mass range
               splitLayout(cellWidths = c("50%", "50%"), 
                         numericInput(inputId = "M_min", label = "Min Energy (keV):", value=0),
                         numericInput(inputId = "M_max", label = "Max Energy (keV):", value=max(Gammas$M))),
               actionButton("plotButton2", "PLOT and CALCULATE")
               ),
               
             
             mainPanel(
               plotlyOutput("plot2")
             )),
             
             
    )
  )
  )


server <- function(input, output) {
  
  ######### TAB 1 #####################
  observeEvent(input$plotButton,{
  
  TYPE <- input$Multipol
  N_Eparts <- input$N_Ebins
  
  Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
    mutate(log_B = log(B))

  ################################################################################ 
  ##### Histogram parameters based on filtered data ##############################
  ################################################################################
  ## Initial guess of num_bins based on Sturge's rule
  K<- 1+3.322*log10(nrow(Gammas_E))
  #GHist <- hist(Gammas_E$log_B, breaks = K)
  
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
  
  K <- 9
  #for(i in 1:length(Evals)){
  i <- 1
  while(i < length(Evals)){ 
    #i<-3
    G <- filter(Gammas, Egam >= Evals_invLog[i] , Egam < Evals_invLog[i+1], Mult_Single == TYPE) %>%
      mutate(log_B = log(B))
    #K <- 2
    GH <- hist(G$log_B, breaks = K)
    
    ## Add histogram data
    GHists$data[[i]] <-  tibble(w=widths[i], x=Emids[i], y=GH$mids, z=GH$counts)
    
    ## Add fitted gauss data
    a <- c(max(GH$counts), mean(G$log_B), sd(G$log_B))
    #GHists$fit[[i]] <-  tibble(x=Emids[i], y=GH$mids, z=Gauss(a, GH$mids))
    GHists$fit[[i]] <-  tibble(max = max(GH$counts), mean = mean(G$log_B), sd = sd(G$log_B))
    i <- i + 1
  }
  
  #3D histogram
  output$plot <- renderPlotly({
    fig <- plot_ly()
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
    
    #end render plotly
    })
  #end tab one plot button observe event 
  })
  
  ######### TAB 2 #####################
  observeEvent(input$plotButton2,{
  
    TYPE <- input$Multipol2
    
    Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
                filter(Egam >= input$E_min, Egam <= input$E_max) %>%
                filter(M >= input$M_min, M <= input$M_max) %>%
                mutate(log_B = log10(B))

    
    ## Histogram slice
    output$plot2 <- renderPlotly({
      
      
      
    K <- 9
    p2 <- plot_ly(Gammas_E, x=~log_B, type="histogram", nbinsx=round(K))%>%
      layout(
        xaxis = list(title="Bvalue, Weisskopt units"),
        yaxis = list(title="Count")
      )# %>%
    #add_lines(x=xvals2, y=Gauss(r1$par, xvals2))
    p2
    
    #end render plotly
    })
    
  #end tab 2 plot button observe event 
  })
  
  
}


shinyApp(ui = ui, server = server)