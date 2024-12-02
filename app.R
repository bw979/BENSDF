library(shiny)
library(tidyverse)
library(plotly)
library(rvmethod)
library(shinylive)
library(httpuv)
library(bslib)
library(ggplot2)

Gammas <-  read_csv("OUTPUTS/ND_Gamma_Dec2023_BKnown.csv")

## Gauss function, input vector a=c[1,2,3] to fit and data x value 
Gauss <- function(a, x) { return(a[1]*exp((-(x-a[2])^2)/a[3])) }


# ########## NORMAL FLUID PAGE ##################################################
# ui <- fluidPage(
#   titlePanel("Gamma Ray Predictor"),
#   # Create a tabbed panel
#   tabsetPanel(
#     tabPanel("Explore",
#              h3("Tab 1"),
#              p("The B value data is text scraped from the raw ENSDF database"),
#              sidebarLayout(
#                sidebarPanel(
#                  #selectInput("Multipol", "Multipolarity:", choices=levels(as.factor(Gammas$Mult_Single)), selected="M1"),
#                  selectInput("Multipol", "Multipolarity:", choices=c("M1", "M2", "E1", "E2"), selected="M1"),
#                  numericInput("N_Ebins", "Number of Energy Bins:", value = 7),
#                  actionButton("plotButton", "PLOT")
#                ),
#                mainPanel(
#                  plotlyOutput("plot0"),
#                  plotlyOutput("plot")
#                )
#               )
#     ),
# 
#     ## This tab filters based on selecting a mass range or input list of masses
#     tabPanel("Evaluate",
#              h3("Tab 2"),
#              p("On th is tab you can filer all the B-value data and fit a logarithmic gaussian, giving you a very good idea of a starting point for any transition matrix element. This is much
#                better than using Weisskopf units on their own, as you are using a best educated guess fitted to all known transitions of the set type."),
#              # Set energy range
#              sidebarLayout(
#              sidebarPanel(
#                selectInput("Multipol2", "Multipolarity:", choices=c("M1", "M2", "E1", "E2"), selected="M1"),
#                splitLayout(cellWidths = c("50%", "50%"),
#                          numericInput(inputId = "E_min", label = "Min Energy (keV):", value=0),
#                          numericInput(inputId = "E_max", label = "Max Energy (keV):", value=max(Gammas$Egam))),
#                # select odd-odd, even-even, or all
#                radioButtons(inputId="Mass_type", label="Type of Mass number?",
#                           choices=c("Odd-Odd","Even-Even", "Odd Mass", "All"), selected="All"),
#                # Set Mass range
#                splitLayout(cellWidths = c("50%", "50%"),
#                          numericInput(inputId = "M_min", label = "Lower Mass:", value=0),
#                          numericInput(inputId = "M_max", label = "Upper Mass:", value=max(Gammas$M))),
#                # set number of B-value bins
#                numericInput(inputId = "K", label = "Number of B-value bins", value=50),
#                actionButton("plotButton2", "PLOT and CALCULATE")
#                ),
# 
# 
#              mainPanel(
#                plotOutput("plot2")
#              )),
# 
# 
#     )
#   )
#   )

 #################### CARDS UI #################################################
ui <- page_fillable(
  #titlePanel("Gamma Ray Predictor"),
    card(card_header("Explore"),
             h3("Tab 1"),
             p("The B value data is text scraped from the raw ENSDF database"),
             #sidebarLayout(
               #sidebarPanel(
                 #selectInput("Multipol", "Multipolarity:", choices=levels(as.factor(Gammas$Mult_Single)), selected="M1"),
                 selectInput("Multipol", "Multipolarity:", choices=c("M1", "M2", "E1", "E2"), selected="M1"),
                 numericInput("N_Ebins", "Number of Energy Bins:", value = 7),
                 actionButton("plotButton", "PLOT"),

               #mainPanel(
                 plotlyOutput("plot0"),
                 plotlyOutput("plot")
               #)
             #),

    ),

    ## This tab filters based on selecting a mass range or input list of masses
    card(card_header("Evaluate"),
             h3("Tab 2"),
             p("On th is tab you can filer all the B-value data and fit a logarithmic gaussian, giving you a very good idea of a starting point for any transition matrix element. This is much
               better than using Weisskopf units on their own, as you are using a best educated guess fitted to all known transitions of the set type."),
             # Set energy range
             # sidebarLayout(
             #   sidebarPanel(
                 selectInput("Multipol2", "Multipolarity:", choices=c("M1", "M2", "E1", "E2"), selected="M1"),
                 splitLayout(cellWidths = c("50%", "50%"),
                             numericInput(inputId = "E_min", label = "Min Energy (keV):", value=0),
                             numericInput(inputId = "E_max", label = "Max Energy (keV):", value=max(Gammas$Egam))),
                 # select odd-odd, even-even, or all
                 radioButtons(inputId="Mass_type", label="Type of Mass number?",
                              choices=c("Odd-Odd","Even-Even", "Odd Mass", "All"), selected="All"),
                 # Set Mass range
                 splitLayout(cellWidths = c("50%", "50%"),
                             numericInput(inputId = "M_min", label = "Lower Mass:", value=0),
                             numericInput(inputId = "M_max", label = "Upper Mass:", value=max(Gammas$M))),
                 # set number of B-value bins
                 numericInput(inputId = "K", label = "Number of B-value bins", value=50),
                 actionButton("plotButton2", "PLOT and CALCULATE"),



               #mainPanel(
                 imageOutput("plot2")
              # ),


    )

)


# ######################## Accordion UI  #########################################
# ui <- page_sidebar(
#   title="Gamma Ray Predictor",
#   sidebar = sidebar(
#     bg = "white",
#     accordion(
#       accordion_panel(
#         "Primary Controls",
#         selectInput("Multipol", "Multipolarity:", choices=c("M1", "M2", "E1", "E2"), selected="M1"),
#         numericInput("N_Ebins", "Number of Energy Bins:", value = 7),
#         actionButton("plotButton", "PLOT")
#       ),
#       accordion_panel(
#         "Secondary Controls",
#                          selectInput("Multipol2", "Multipolarity:", choices=c("M1", "M2", "E1", "E2"), selected="M1"),
#                          splitLayout(cellWidths = c("50%", "50%"),
#                                      numericInput(inputId = "E_min", label = "Min Energy (keV):", value=0),
#                                      numericInput(inputId = "E_max", label = "Max Energy (keV):", value=max(Gammas$Egam))),
#                          # select odd-odd, even-even, or all
#                          radioButtons(inputId="Mass_type", label="Type of Mass number?",
#                                       choices=c("Odd-Odd","Even-Even", "Odd Mass", "All"), selected="All"),
#                          # Set Mass range
#                          splitLayout(cellWidths = c("50%", "50%"),
#                                      numericInput(inputId = "M_min", label = "Lower Mass:", value=0),
#                                      numericInput(inputId = "M_max", label = "Upper Mass:", value=max(Gammas$M))),
#                          # set number of B-value bins
#                          numericInput(inputId = "K", label = "Number of B-value bins", value=50),
#                          actionButton("plotButton2", "PLOT and CALCULATE")
#       )
#     )
#   ),
# 
#   accordion(
#     open = c("Bill Length", "About"),
#     accordion_panel(
#       "All data",
#       #plotlyOutput("plot0")
#       plotlyOutput("plot")
#     ),
#     accordion_panel(
#       "Filter data",
#       plotOutput("plot2")
#     )
#   )
# )


server <- function(input, output) {
  
  ######### TAB 1 #####################
  output$plot0 <- renderPlotly({
  ######## PLot the 2D plot  ################
  E.1 <- filter(Gammas, Mult_Single=="E1")
  pE1 <- plot_ly(E.1, x =~Egam, y =~B, type="scatter", mode="markers", name=~Mult_Single, marker=list(color="brown")) %>%
    layout(xaxis=list(title="Gamma Energy (keV)", exponentformat="E", type='log'),
           yaxis=list(title="B (Weisskopf Units)", type='log', exponentformat="E"),
           showlegend=T
    )

  E.2 <- filter(Gammas, Mult_Single=="E2")
  pE2 <- plot_ly(E.2, x =~Egam, y =~B, type="scatter", mode="markers", name=~Mult_Single, marker=list(color="orange")) %>%
    layout(xaxis=list(title="Gamma Energy (keV)", exponentformat="E", type='log'),
           yaxis=list(title="B (Weisskopf Units)", type='log', exponentformat="E"),
           showlegend=T
    )


  M.1 <- filter(Gammas, Mult_Single=="M1")
  pM1 <- plot_ly(M.1, x =~Egam, y =~B, type="scatter", mode="markers", name=~Mult_Single, marker=list(color="green")) %>%
    layout(xaxis=list(title="Gamma Energy (keV)", exponentformat="E", type='log'),
           yaxis=list(title="B (Weisskopf Units)", type='log', exponentformat="E"),
           showlegend=T
    )

  M.2 <- filter(Gammas, Mult_Single=="M2")
  pM2 <- plot_ly(M.2, x =~Egam, y =~B, type="scatter", mode="markers", name=~Mult_Single, marker=list(color="blue")) %>%
    layout(xaxis=list(title="Gamma Energy (keV)", exponentformat="E", type='log'),
           yaxis=list(title="B (Weisskopf Units)", type='log', exponentformat="E"),
           showlegend=T
    )


  all <- subplot(pE1, pE2, pM1, pM2, nrows=2, titleY=T, titleX=T, margin=0.035)
  all

  #end plot0
  })
  
  ####### Plot the 3D plot ##################
  observeEvent(input$plotButton,{
    
  withProgress({
  
  TYPE <- input$Multipol
  N_Eparts <- input$N_Ebins
  
  Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
    mutate(log_B = log(B))
  
  if(TYPE=="M1"){
    BC <- viridisLite::turbo(10)[5]
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
    #N_Eparts <- 10
    K <- 30
  } else if(TYPE=="M2"){
    BC <- viridisLite::inferno(10)[5]
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
    #N_Eparts <- 12
    K <- 30
  } else if(TYPE=="E1"){
    BC <- viridisLite::rocket(10)[5]
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
    #N_Eparts <- 7
    K <- 30
  } else if(TYPE=="E2"){
    BC <- viridisLite::mako(10)[5]
    # Define a function to add 3D bars
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
    #N_Eparts <- 20
    K <- 30
  }
  
  
  K <- 30

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
  
  #3D histogram
  output$plot <- renderPlotly({
    fig <- plot_ly(showlegend=F, height="600")
    #fig <- plot_ly(showlegend=F)
    
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
  #end progress bar
  })
  #end tab one plot button observe event 
  })
  
  ##############################################################################
  ################################# TAB 2 ######################################
  ##############################################################################

  observeEvent(input$plotButton2,{

    TYPE <- input$Multipol2

    Gammas_E <- filter(Gammas, Mult_Single == TYPE) %>%
                filter(Egam >= input$E_min, Egam <= input$E_max) %>%
                filter(M >= input$M_min, M <= input$M_max) %>%
                mutate(log_B = log10(B))

    Gammas_E$N <- Gammas_E$M - Gammas_E$Z

    ### Odd/even mass filtering
    # ## odd mass
    # if((Gammas_E$M %% 2) !=0 ) {
    #   Gammas_E <- filter(Gammas_E,  )
    # ## or even even
    # } else if ( ((Gammas_E$N %% 2) !=0) && ((Gammas_E$Z %% 2) != 0) ){
    #
    # ## or odd-odd
    # } else if ( ((Gammas_E$N %% 2) ==0) && ((Gammas_E$Z %% 2) == 0)  )

    #K <- round(1+3.322*log10(nrow(Gammas_E)))
    K <- input$K ## Set manually
    h <- hist(Gammas_E$log_B, breaks = "FD", plot=FALSE)

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


    if(TYPE=="M1"){
      BC <- viridisLite::turbo(10)[5]
    } else if(TYPE=="M2"){
      BC <- viridisLite::inferno(10)[5]
    } else if(TYPE=="E1"){
      BC <- viridisLite::rocket(10)[5]
    } else if(TYPE=="E2"){
      BC <- viridisLite::mako(10)[5]
    }
      
    

    ## Histogram slice
    #output$plot2 <- renderPlot({
    
    ## Have to render image of base R plot otherwise shiny cards sets the plot margins too large
    output$plot2 <-  renderImage({
        img <- htmltools::capturePlot({
          ## Final plotting
          h <- hist(Gammas_E$log_B, breaks = "FD", col = BC,
                    xlab="Log(B-value, w.u.)",
                    main ="Histogram and Gaussian fit of logarthmic B-values")
          lines(xvals, yvals)
          mean_string <- paste("Mean is:", signif(10^optim_mean_logB, 3), " w.u.")
          text(0.7*min(Gammas_E$log_B), max(h$counts), mean_string)
          sd_string <- paste0("sd is: +/- log(", signif(10^optim_sd_logB, 3), ") w.u.")
          text(0.7*min(Gammas_E$log_B), 0.9*max(h$counts), sd_string)
        }, height = 400, width = 400)
        list(src = img, width = 500, height = 500)
      }, deleteFile = TRUE)  
      
      # ## Final plotting
      # h <- hist(Gammas_E$log_B, breaks = "FD", xlab="Log(B-value, w.u.)",
      #           main ="Histogram and Gaussian fit of logarthmic B-values")
      # lines(xvals, yvals)
      # mean_string <- paste("Mean is:", signif(10^optim_mean_logB, 3), " w.u.")
      # text(0.7*min(Gammas_E$log_B), max(h$counts), mean_string)
      # sd_string <- paste0("sd is: +/- log(", signif(10^optim_sd_logB, 3), ") w.u.")
      # text(0.7*min(Gammas_E$log_B), 0.9*max(h$counts), sd_string)
      
      #ggplot(Gammas_E, aes(x=Gammas_E$log_B), height=100, width=100) + geom_histogram()


    # p2 <- plot_ly(Gammas_E, x=~log_B, type="histogram", nbinsx=round(K))%>%
    #   layout(
    #     xaxis = list(title="Bvalue, Weisskopt units"),
    #     yaxis = list(title="Count")
    #   ) %>%
    # #add_lines(x=xvals2, y=Gauss(r1$par, xvals2))
    #   add_trace(x = xvals,
    #             y = yvals,
    #             type = "scatter",
    #             mode = "lines")
    #             #name = Fit_String)
    #             #line = list(color = "black", width = 10)
    # p2

    #end render plotly
    })

  ##end tab 2 plot button observe event
  #})
  
  
}


shinyApp(ui = ui, server = server)