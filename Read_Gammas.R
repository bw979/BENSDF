  library(stringr)
  library(tidyverse)
  library(readr)
  
  ###########################################
  ###  Reads ENSDF Gamma ray properties  ####
  ###  -Includes Continuation Card      #####
  ###  -REGEX's continuations for B values ##
  ###  -Then cleans and tidies       ########
  ########################################### 
  #### Written by B.Wallis ##################
  
  
  ## This program also grabs the feeding level energy from the level card preceding the gamma card
  ## This kind of technique should be used by beta or alpha reads as well
  
 source("Filenames.R")
  ## Unlinks old stuff if rerunning and clears big variables
   unlink(Init_Output_Name)
   unlink(FINAL_OUTPUT)
  #rm(NNDC)
  #rm(ND_Gamma)

  
  #### conv for addition of Z column ####
  Conv <- read.csv("Dependencies/Z_Conversion.csv")
  conv <- data.frame(Z=Conv[1], Symb=Conv[2], Name=Conv[3])
  conv$Symb <- toupper(conv$Symb)
  ### Handy Function
  #Load Z column into NNDC
  whi <- function(a) {
    return(which(conv$Symb==a))
  }
  ## Unlist each string (line) in mXr 
  unstring <- function(raw){
    matrix(unlist(strsplit(raw, split="")), ncol=eol, byrow=T)
  }
  
  #### READ DATA ####
  ####### RUN over all MASS files as character streams #########
  ## mXr : raw
  ## mX : matrix with rows = raw lines and colums = each character
  ## mXd : data card parsed from specified card ... e.g. mXG is a gamma data.frame
for(run in 1:295){
#for(run in 1:293){
    r <- run
    mass <- sprintf("Dependencies/ensdf_231101/ensdf.%003.f", run)
    mXr <- scan(mass, what=character(0), sep="\n")
    
    #### CREATE mXd (the current one we are gonna write to file) data.frame ####
    ## ASSIGN CARD CHARACTERS access mX[line, character] SLOW BIT
    eol <- 80
    
    mX <- unstring(mXr)
    
    #set counters and initialise raw G card
    counter <- 1;  contCounter <- 1
    mXG <- rep("", length(mXr)*eol) ### Currently this is as big as the data could ever be (not the best way of doing it, needs updating)
    ## set dimensions
    dim(mXG) <- c(length(mXr), eol)
  
  
    ###### READ IN EITHER GAMMA OR associated CARDS
    #### Run through mX whole mass matrix, read into mXd if its a certain "card" ####
    for(i in 1:length(mX[,1])) {
      # assume its not a continuation card, this is not needed in the end
      cont <- FALSE 
      
      # # if looks like a "Parent" Record, feed in that line
      # if(( mX[i,7] == " ") && (mX[i,8] == "P")){
      #   #feed in the characters in that line
      #   for(j in 1:eol) {
      #     mXG[counter,j] <- mX[i, j]
      #   }
      #   counter <- counter + 1
      #   #store the most recent level index
      #   #prevL <- i
      # }
      
      # if looks like a "Normalisation" Record, feed in that line
      if(( mX[i,7] == " ") && (mX[i,8] == "N")){
        #feed in the characters in that line
        for(j in 1:eol) {
          mXG[counter,j] <- mX[i, j]
        }
        counter <- counter + 1
        #store the most recent level index
        #prevL <- i
      }
      
      # if looks like a "Product Normalisation" Record, feed in that line
      if(( mX[i,7] == "P") && (mX[i,8] == "N")){
        #feed in the characters in that line
        for(j in 1:eol) {
          mXG[counter,j] <- mX[i, j]
        }
        counter <- counter + 1
        #store the most recent level index
        #prevL <- i
      }
      
      
      ## if looks like a "Level" Record, feed in that line
      if((mX[i,8] == "L") && (mX[i,7] == " ") && (mX[i,80] != "?")  && (mX[i,6] == " ")){
        #feed in the characters in that line
        for(j in 1:eol) {
          mXG[counter,j] <- mX[i, j]		
        }
        counter <- counter + 1
        #OR more simple, but leaves NA in LCard[1,]
        #mXL <- rbind(mXL, mX[i,])
      }
      
      ## if looks like a "Gamma" Record, feed in that line
      if((mX[i,8] == "G") && (mX[i,7] == " ") && (mX[i,80] != "?") ){
        #feed in the characters in that line
        for(j in 1:eol) {
          mXG[counter,j] <- mX[i, j]		
        }
        counter <- counter + 1
 	      #store the most recent level index
        #prevL <- i
      }
      
  
    }
    
    #number of level data entries for that mass
    dl <- counter 
     
    ##Where mXd used to be assigned (is still assigned actually)
    #SHOULD MODERNISE THIS BIT
    mXd <- data.frame(AX=c(rep(0, dl)), AX_parent=c(rep(0, dl)), El=c(rep(0,dl)), Egam=c(rep(NA,dl)), DE=c(rep(0,dl)), RI=c(rep(0,dl)), DRI=c(rep(0,dl)), NRxBR=c(rep(0,dl)), TI=c(rep(0,dl)), NTxBR=c(rep(0,dl)),  Mult=c(rep(0,dl)), MR=c(rep(0,dl)), DMR=c(rep(0,dl)), CC=c(rep(0,dl)), DCC=c(rep(0,dl)), E_parent=c(rep(0, dl)), E_feed=c(rep(0,dl)), Continuation=c(rep(0,dl)) ) #L=c(rep(0,dl)), S=c(rep(0,dl)), DS=c(rep(0,dl)), MS=c(rep(0,dl)), Q=c(rep(0,dl)))
    # ##  Gamma card prototype
    # mXd <- tibble(Key=character(1), El=character(1), Egam=double(1), DE=double(1), RI=double(1), DRI=double(1), M=double(1), MR=double(1), DMR=double(1), CC=double(1), DCC=double(1), BE1=double(1), BE1W=double(1), CE=double(1), CEK=double(1), ECC=double(1), EKC=double(1), FL=double(1), FLAG=double(1), KC=double(1), Continuation = character(1) )
  
    row <- 1
    
    ### Card type: Gamma with level and normalization info ###
    while (row < dl){
      
      ### Gamma continuation card
      if( (mXG[row, 6] != " ") && (mXG[row, 6] != "1") && (mXG[row, 8] == "G") ){
      #AX
      mXd$AX[row_gamma_prev] <- paste(mXG[row,1:5], sep="", collapse="")
      #El
      mXd$El[row_gamma_prev] <- paste(mXG[row,1:5], sep="", collapse="")
      #Continuation
      mXd$Continuation[row_gamma_prev] <- paste(mXG[row,10:80], sep="", collapse="")
      
      ## if its not the first continuation, then paste it onto the last
      if(row_gamma_prev + 1 < row){
        mXd$Continuation[row_gamma_prev] <- paste(mXd$Continuation[row_gamma_prev], mXG[row,10:80], sep="", collapse="")
      }
      
      #go to next step of loop
      row <- row + 1
      next
      }
      
      # ### Set as NA in case N record not found
      # NT <- NA
      # BR <- NA
      
      # ### If its normalisation record grab the NT*BR value for normalising TI, if
      # if(( mXG[row,7] == " ") && (mXG[row,8] == "P") ){
      #   AX_parent<-paste(mXG[row,1:5], sep="", collapse="")
      #   E_parent<- as.double(paste(mXG[row,10:19], sep="", collapse=""))
      # }
      
      ### If its normalisation record grab the NT*BR value for normalising TI, if
      if(( mXG[row,7] == " ") && (mXG[row,8] == "N") ){
        # mXd$TI_norm[row_gamma_prev] <- as.double(paste(mXG[row_gamma_prev,22:29], sep="", collapse=""))
        # mXd$RI_norm[row_gamma_prev] <- as.double(paste(mXG[row_gamma_prev,10:19], sep="", collapse=""))
        NR <- as.double(paste(mXG[row,10:19], sep="", collapse=""))
        NT <- as.double(paste(mXG[row,22:29], sep="", collapse=""))
        BR <- as.double(paste(mXG[row,32:39], sep="", collapse=""))
        ### Set as NA in case N record not found
        # NT <- NA
        # BR <- NA
      }
      
      
      ### If its a production normalisation record grab the NT*BR value for normalising TI, if
      if(( mXG[row,7] == "P") && (mXG[row,8] == "N") ){
        # mXd$TI_norm[row_gamma_prev] <- as.double(paste(mXG[row_gamma_prev,22:29], sep="", collapse=""))
        # mXd$RI_norm[row_gamma_prev] <- as.double(paste(mXG[row_gamma_prev,10:19], sep="", collapse=""))
        NRxBR <- as.double(paste(mXG[row,10:19], sep="", collapse=""))
        NTxBR <- as.double(paste(mXG[row,22:29], sep="", collapse=""))
        ### Set as NA in case N record not found
        #NTxBR <- NA
      }
      
      ### if its a level card, store the feeding energy
      if((mXG[row,8] == "L") ){
        Feed_Level <- as.double(paste(mXG[row,10:19], sep="", collapse=""))
      }
      
      
      if(( mXG[row,8] == "G") ){
      ### read initial one card entries  normally
      row_gamma_prev <- row
      #AX
      mXd$AX[row] <- paste(mXG[row,1:5], sep="", collapse="")
      #El
      mXd$El[row] <- paste(mXG[row,1:5], sep="", collapse="")
      #Energy (keV)
      mXd$Egam[row] <- as.double(paste(mXG[row,10:19], sep="", collapse=""))
      #DE
      mXd$DE[row] <- as.double(paste(mXG[row,20:21], sep="", collapse=""))
      #RI Relative photon Intensity
      mXd$RI[row] <- as.double(paste(mXG[row,22:29], sep="", collapse=""))
      #DRI
      mXd$DRI[row] <- as.double(paste(mXG[row,30:31], sep="", collapse=""))
      #TI
      mXd$TI[row] <- as.double(paste(mXG[row,65:74], sep="", collapse=""))
      #M Multipolarity
      mXd$Mult[row] <- as.character(paste(mXG[row,32:41], sep="", collapse=""))
      #MR Mixing Ratio
      mXd$MR[row] <- as.double(paste(mXG[row,42:49], sep="", collapse=""))
      #DMR
      mXd$DMR[row] <- as.double(paste(mXG[row,50:55], sep="", collapse=""))
      #CC Total IC coeff
      mXd$CC[row] <- as.double(paste(mXG[row,56:62], sep="", collapse=""))
      #DCC
      mXd$DCC[row] <- as.double(paste(mXG[row,64:64], sep="", collapse=""))
      
      # ### The parent... should = AX and Feed level
      # mXd$E_parent[row] <- E_parent
      # mXd$AX_parent[row] <- AX_parent
      
      ### THE FEED LEVEL
      mXd$E_feed[row] <- Feed_Level
      
      ### Feed in the previous intensity info that fed that line
      if(is.na(NTxBR)  && !is.na(NT) && !is.na(BR)){
        NTxBR <- NT * BR
        mXd$NTxBR[row] <- NTxBR
        #NT <- NA
        #BR <- NA
      } else if(!is.na(NTxBR) ) {
        mXd$NTxBR[row] <- NTxBR
        #NTxBR <- NA
      }
      
      ### Feed in the previous intensity info that fed that line
      if(is.na(NRxBR)  && !is.na(NR) && !is.na(BR)){
        NRxBR <- NR * BR
        mXd$NRxBR[row] <- NRxBR
        #NR <- NA
        #BR <- NA
      } else if(!is.na(NRxBR) ) {
        mXd$NRxBR[row] <- NRxBR
        #NRxBR <- NA
      }# else if( is.na(NRxBR)  && !is.na(NR) && !is.na(BR)){
      #  
      #}
      
      
      # ### Feed in the previous intensity info that fed that line
      # if(is.na(NRxBR)){
      #   NRxBR <- NR * BR
      # }
      }
      
      row <- row + 1
    }
    
    #assign M and Z columns (were joint up before; couldn't do in while loop)
    mXd$M <- as.numeric(str_extract(mXd$El, "[0-9]+"))
    mXd$El <- str_extract(mXd$El, "[aA-zZ]+")
    
    #TRIM white space in ID
    mXd$El <- trimws(mXd$El)
    mXd$AX <- trimws(mXd$AX)
    mXd$Continuation <- trimws(mXd$Continuation)
    
    #Split into factors
    #mXf <- split(mXd, factor(mXd$El))
    
    ### Append to a final df 
    if(run==1) {
      #write.table(mXd, file=name, append=T, row.names=F, col.names=T,  sep=",")
      mXT <- mXd
    }
    else {
      #write.table(mXd, file=name, append=T, row.names=F, col.names=F,  sep=",")
      mXT <- rbind(mXT, mXd)
    }
    print(run)
}
  
  
  # test <- filter(mXT, TI!=0)
  # 
  # 
  # ### Add the Bval parse from a previous read
  # 
  # 
  # match_BVal <- function(){
  #  mXT  
  # }
  
  
  mXT <- filter(mXT, AX !=0)
  ##### END NNDC READ ####
  ######################NDGam <- filter(NDGam, !is.na(El)) .... this was causing the problem with continuation read
  mXT <- filter(mXT, El != "NN")
  mXT$Z <- as.character(map(mXT$El, whi))

  write.table(mXT, file=Init_Output_Name, append=T, row.names=F, col.names=T,  sep=",")

  
  
  
  
  #######################################
  ####### NOW THE REGEX BIT  ############
  ######## heavy going this bit    ######
  # Have merged Continuation_Cleaner.R ##
  #######################################

  ##Trying to extract the "B###" values from the 'Continuation' variable in "Dependencies/ND_Gamma_TI.csv"
  ## Read_ICC.R extracts the "Dependencies/ND_Gamma.csv" data from a raw database in "Dependencies/NNDC" and works
  #source("Wins.R")
  #Init_Output_Name <- "Dependencies/ND_Gamma_ContUnproc_TI.csv"
  
  ND_Gamma <- read_csv(Init_Output_Name)
  
  ##Does all the mutating and wrangling
  ND_Gamma <- mutate(ND_Gamma, Delete = logical(1)) %>%
    select(-AX_parent, -E_parent) %>%
  mutate(Btype = character(1), B = double(1), FL = double(1) ) %>%
  mutate(Mult_Single=character(1), .after = Btype) %>%
    filter(!is.na(Egam))
 
  #mutate(Braw = character(1), Btype = character(1), B = double(1), B2raw = character(1), B2type = character(1), B2 = double(1), FLraw = character(1), FL = double(1) )
  #ND_Gamma <- mutate(ND_Gamma, CCraw = character(1), CCtype = character(1), B = double(1) )

##################################################################################
### Function that will read the continuation cards and can be used with map func##
##function formatL       (continuation string,  Input= E.g. "B", Output="Btype")
Extract_Braw <- function(Continuation, Input, Output){
  #for(i in 1:500){  ## if its a continuation card then look for the "BE" or "BM data in that string
  if( Continuation == 0) { return(NA)}
  if( Continuation != 0 ) {
    #i <- 16

    if(Output=="Continuation") return(Continuation)
    
    #### NEED TO SORT OUT INSTANCES SUCH AS THIS:
    #Continuation <- "BM1W<0.00035$BE2W=1.03 9"
    
    ### B variable
    if(Input=="B"){
      # "B\\S*" ... B followed by any number of non-spaces then W followed by any number of non spaces
      Braw <- str_extract(Continuation, "B\\S*W\\S*")
      if(Output=="Braw") return(Braw)
      ## ".+(?<=\\=)" ... positive look behind an "=" symbol but not including it
      Btype <- str_extract(Braw, ".+(?<=\\=)") %>%
        str_extract("[^\\=]*")
      if(Output=="Btype") return(Btype)

      ## "\\=[\\d\\.E\\-]+" ... "=" followed by more than one digit, ".", "E" or "-" symbol
      B <- str_extract(Braw, "\\=[\\d\\.E\\-]+") %>%
        str_extract("[^\\=]+") %>%
        as.double()
      if(Output=="B") return(B)
    }

    ##### B2 variable
    if(Input=="B2"){
      FLraw <- str_extract(Continuation, "FL=\\S*|FL=\\S*\\+|FL=\\S*\\$")
      FL <- str_extract(FLraw, "\\=[\\d\\.E\\-]+") %>%
        str_extract("[^\\=]+") %>%
        as.double()
      if(Output=="FL") return(FL)
      
      
      # B2raw <- str_extract(Continuation, "(?<=(\\$[\\s\\S]{0,100}\\$))B\\S*W\\S*|(?<=(\\$[\\s\\S]{0,100}\\$))\\sB\\S*W\\S*")
      # #B2type <- str_extract(B2raw, ".+(?<=\\=)")
      # #B2 <- str_extract(B2raw, "\\=[\\d\\.E\\-]+") %>%
      # #  str_extract("[^\\=]+") %>%
      # #  as.double()
      # 
      # 
      # if(  (Continuation !=0 ) && is.na(B2raw) ){
      #   ## read the single dollar sign case
      #   B2raw <- str_extract(Continuation, "(?<=\\$)\\sB\\S*W\\S*|(?<=\\$)B\\S*W\\S*")
      #   B2type <- str_extract(B2raw, ".+(?<=\\=)")
      #   if(Output=="B2type") return(B2type)
      #   B2 <- str_extract(B2raw, "\\=[\\d\\.E\\-]+") %>%
      #     str_extract("[^\\=]+") %>%
      #     as.double()
      #   if(Output=="B2") return(B2)
      #   
      # }
    }

    # ### FL (Final Level) variable
    # if(Input=="B2"){
    #   FLraw <- str_extract(Continuation, "FL=\\S*|FL=\\S*\\+|FL=\\S*\\$")
    #   FL <- str_extract(FLraw, "\\=[\\d\\.E\\-]+") %>%
    #     str_extract("[^\\=]+") %>%
    #     as.double()
    #   if(Output=="FL") return(FL)
    # }
  #print(i)
   }
}
###############################################################################
  
ND_Gamma$B <- map(ND_Gamma$Continuation, Extract_Braw, "B", "B")
ND_Gamma$Btype <- map(ND_Gamma$Continuation, Extract_Braw, "B", "Btype")

ND_Gamma$FL <- map(ND_Gamma$Continuation, Extract_Braw, "B2", "FL")

remove_BW <- function(Btype){return(str_remove_all(Btype, "[BW]"))}
ND_Gamma$Mult_Single <- lapply(ND_Gamma$Btype, remove_BW)

#### Getting rid of NANA and weird list variabels ###
ND_Gamma$B <- as.double(ND_Gamma$B)
ND_Gamma$Btype <- as.character(ND_Gamma$Btype)
ND_Gamma$FL <- as.double(ND_Gamma$FL)
ND_Gamma$Mult_Single <- as.character(ND_Gamma$Mult_Single)


write.table(ND_Gamma, FINAL_OUTPUT, append=T, row.names=F, col.names=T,  sep=",")


   
#   ##### WORKS #####
#   ## Extract a Btype variable from the continuation strings  
#   
# for(i in 1:length(ND_Gamma$Continuation)) {
# 
#   #for(i in 1:500){  ## if its a continuation card then look for the "BE" or "BM data in that string
#     if( ND_Gamma$Continuation[i] != 0 ) {
#       #i <- 16
#   ### Extract the B variables
#   # "B\\S*" ... B followed by any number of non-spaces then W followed by any number of non spaces
#   ND_Gamma$Braw[i] <- str_extract(ND_Gamma$Continuation[i], "B\\S*W\\S*")
#   ## ".+(?<=\\=)" ... positive look behind an "=" symbol but not including it
#   ND_Gamma$Btype[i] <- str_extract(ND_Gamma$Braw[i], ".+(?<=\\=)") %>%
#     str_extract("[^\\=]*")
#   ## "\\=[\\d\\.E\\-]+" ... "=" followed by more than one digit, ".", "E" or "-" symbol
#   ND_Gamma$B[i] <- str_extract(ND_Gamma$Braw[i], "\\=[\\d\\.E\\-]+") %>%
#     str_extract("[^\\=]+") %>%
#     as.double()
#   print(i)
# 
#       ##### B2
#       ND_Gamma$B2raw[i] <- str_extract(ND_Gamma$Continuation[i], "(?<=(\\$[\\s\\S]{0,100}\\$))B\\S*W\\S*|(?<=(\\$[\\s\\S]{0,100}\\$))\\sB\\S*W\\S*")
#       ND_Gamma$B2type[i] <- str_extract(ND_Gamma$B2raw[i], ".+(?<=\\=)")
#       ND_Gamma$B2[i] <- str_extract(ND_Gamma$B2raw[i], "\\=[\\d\\.E\\-]+") %>%
#         str_extract("[^\\=]+") %>%
#         as.double()
# 
#        if(  (ND_Gamma$Continuation[i] !=0 ) && is.na(ND_Gamma$B2raw[i]) ){
#         ## read the single dollar sign case
#         ND_Gamma$B2raw[i] <- str_extract(ND_Gamma$Continuation[i], "(?<=\\$)\\sB\\S*W\\S*|(?<=\\$)B\\S*W\\S*")
#         ND_Gamma$B2type[i] <- str_extract(ND_Gamma$B2raw[i], ".+(?<=\\=)")
#         ND_Gamma$B2[i] <- str_extract(ND_Gamma$B2raw[i], "\\=[\\d\\.E\\-]+") %>%
#           str_extract("[^\\=]+") %>%
#           as.double()
#       }
# 
#       ND_Gamma$FLraw[i] <- str_extract(ND_Gamma$Continuation[i], "FL=\\S*|FL=\\S*\\+|FL=\\S*\\$")
#       ND_Gamma$FL[i] <- str_extract(ND_Gamma$FLraw[i], "\\=[\\d\\.E\\-]+") %>%
#         str_extract("[^\\=]+") %>%
#         as.double()
#     }
#     #print(i) 
# }
  
  # write.table(ND_Gamma, "ENSDF_Reader_Program/Parsed_Raw_Data/ND_Gamma_NewContinuation.csv", append=T, row.names=F, col.names=T,  sep=",")
  

  # #####  Put continuations with the correct level (generally the observation previous of n previous if n continuations) ####
  # #can use paste(..., sep = " ", collapse = NULL)
  # c_number <- 1
  # for(i in 1:length(ND_Gamma$Continuation)){
  #   #for(i in 1:500){
  #   #i<-7
  #   ### This one resents the counter ... ie if its not a continuation
  #   if( ND_Gamma$Continuation[i] == 0){
  #     c_number <- 0
  #   }
  # 
  #   ### If its a continuation card and its the first
  #   if( (ND_Gamma$Continuation[i] != 0) && (c_number == 1)   )  {
  #     #if( (is.na(ND_Gamma$Egam[i])) && (!is.na(ND_Gamma$Btype[i])) &&  (c_number == 1) ) {
  #     #if( (is.na(ND_Gamma$Egam[i])) && ( !is.na((ND_Gamma$Btype[i] != "") || (!is.na(ND_Gamma$Btype[i]))) )  ) {
  #     ND_Gamma$Btype[i-1] <- ND_Gamma$Btype[i]
  #     ND_Gamma$B[i-1] <- ND_Gamma$B[i]
  #     ND_Gamma$Braw[i-1] <- ND_Gamma$Braw[i]
  #     #   ND_Gamma$Continuation[i-c_number] <- ND_Gamma$Continuation[i]
  # 
  #     ## B2's
  #     ND_Gamma$B2type[i-1] <- ND_Gamma$B2type[i]
  #     ND_Gamma$B2[i-1] <- ND_Gamma$B2[i]
  #     ND_Gamma$B2raw[i-1] <- ND_Gamma$B2raw[i]
  # 
  #     if( !is.na(ND_Gamma$FLraw[i]) ){
  #       ND_Gamma$FL[i-1] <- ND_Gamma$FL[i]
  #       ND_Gamma$FLraw[i-1] <- ND_Gamma$FLraw[i]
  #     }
  # 
  #     ## delete the current level
  #     ND_Gamma$Delete[i] <- TRUE
  #   }
  # 
  #   ### If its a continuation card and its above the first
  #   if( (ND_Gamma$Continuation[i] != 0)  && (c_number > 1) ){
  #     ND_Gamma$Delete[i] <- TRUE
  # 
  # 
  #     ## IF theres new Btype information add that
  #     if( !is.na(ND_Gamma$Btype[i]) ){
  #       ND_Gamma$Btype[i-c_number] <- ND_Gamma$Btype[i]
  #       ND_Gamma$B[i-c_number] <- ND_Gamma$B[i]
  #       ND_Gamma$Braw[i-c_number] <- ND_Gamma$Braw[i]
  #     }
  # 
  #     ## IF theres new B2type information add that
  #     if( !is.na(ND_Gamma$B2type[i]) ){
  #       ND_Gamma$B2type[i-c_number] <- ND_Gamma$B2type[i]
  #       ND_Gamma$B2[i-c_number] <- ND_Gamma$B2[i]
  #       ND_Gamma$B2raw[i-c_number] <- ND_Gamma$B2raw[i]
  #     }
  # 
  #     if( !is.na(ND_Gamma$FLraw[i]) ){
  #       ND_Gamma$FL[i-c_number] <- ND_Gamma$FL[i]
  #       ND_Gamma$FLraw[i-c_number] <- ND_Gamma$FLraw[i]
  #     }
  # 
  #   }
  # 
  #   c_number <- c_number + 1
  #   print(i)
  # }

  # ND_Gamma <- filter(ND_Gamma, Delete == FALSE)
  # #ND_Gamma <- filter(ND_Gamma, !is.na(Egam))
  # 
  # ## The final database
  # write.table(ND_Gamma, "OUTPUTS/ND_Gamma.csv", append=T, row.names=F, col.names=T,  sep=",")
  # 
  # 
  
  #ND_Gamma1 <- filter(ND_Gamma, RI_norm !=0)




  
  
  
  
  
