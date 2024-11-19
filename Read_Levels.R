      library(stringr)
      library(tidyverse)
      library(readr)
      

      source("Filenames.R")
      
      unlink(name)
      unlink(Output_Name)
      
      source("tidy_functions2.R")
      
      #### conv for addition of Z ####
      Conv <- read.csv("Dependencies/Z_Conversion.csv")
      conv <- data.frame(Z=Conv[1], Symb=Conv[2], Name=Conv[3])
      conv$Symb <- toupper(conv$Symb)
      
      ### Handy Functions
      #Load Z column into NNDC
      whi <- function(a) {return(which(conv$Symb==a))}
      
      #### READ DATA ####
      ####### RUN over all MASS' and read lines in as strings
      for(run in 1:295){
        #run <- 10
        print(run)
        mass <- sprintf("Dependencies/ensdf_231101/ensdf.%003.f", run)
        mXr <- scan(mass, what=character(0), sep="\n")
        
        #### CREATE mXd data.frame ####
        ## ASSIGN CARD CHARACTERS access mX[line, character] SLOW BIT
        eol <- 80
       
        
        ## Unlist each string (line) in mXr 
        unstring <- function(raw){
          if( !is.null(raw) ) { matrix(unlist(strsplit(raw, split="")), ncol=eol, byrow=T) }
        }
        
        mX <- unstring(mXr)
        head(mX)
        
        ## Read in the actual level characters raw LEVELs only ... slow bit
        counter <- 1
        #Ends up with large empty tail
        mXL <- rep("", length(mXr)*eol)
        dim(mXL) <- c(length(mXr), eol)
        #mXL <- matrix(ncol=eol)
        
        ## Run through mX, read into d if Level 
        for(i in 1:length(mX[,1])) {
          ####! FUNCTION?	
          #if looks like a level, feed in that line
          if((mX[i,8] == "L") && (mX[i,7] == " ") && (mX[i,80] != "?") && (mX[i,6] == " ")){
            
            #feed in the characters in that line
            for(j in 1:eol) {
              mXL[counter,j] <- mX[i, j]		
            }
            counter <- counter + 1
            #OR more simple, but leaves NA in LCard[1,]
            #mXL <- rbind(mXL, mX[i,])
          }
        }
      
        ## Categorize the Level Card entry characters
        #level data entries
        dl <- counter
        mXd <- data.frame(Key=c(rep(0, dl)), M=c(rep(0,dl)), El=c(rep(0,dl)), Z=c(rep(0,dl)), E=c(rep(0,dl)), DE=c(rep(0,dl)), J=c(rep(0,dl)), Thalf=c(rep(0,dl)), DThalf=c(rep(0,dl)) ) #L=c(rep(0,dl)), S=c(rep(0,dl)), DS=c(rep(0,dl)), MS=c(rep(0,dl)), Q=c(rep(0,dl)))
        row <- 1
        
        
        ### Can change for any card type
        while (row < dl){
          #Key
          mXd$Key[row] <- paste(mXL[row,1:5], sep="", collapse="")
          #El
          mXd$El[row] <- paste(mXL[row,1:5], sep="", collapse="")
          #mXd$El[row] <- str_extract(mXd$temp[row], "[aA-zZ]+")
          #M
          #mXd$M[row] <- as.numeric(str_extract(mXd$temp, "[0-9]+"))
          #Z
          #mXd$Z[row] <- whi(mXd$El[row])
          #for(i in 1:length(ND$El)){
          #ND$Z[i] = whi(ND$El[i])
          #}
          #Energy (keV)
          mXd$E[row] <- as.double(paste(mXL[row,10:19], sep="", collapse=""))
          #DE
          mXd$DE[row] <- as.double(paste(mXL[row,20:21], sep="", collapse=""))
          #J
          mXd$J[row] <- paste(mXL[row,22:39], sep="", collapse="")
          #Th = half-life (energy units so WIDTH) ... T1/2(s) ≅ ℏ X ln2 / Γ(MeV)
          mXd$Thalf[row] <- paste(mXL[row,40:49], sep="", collapse="")
          #DT
          mXd$DThalf[row] <- as.double(paste(mXL[row,50:55], sep="", collapse=""))
          #L, angular momentum transfer
          #mXd$L[row] <- paste(mXL[row,56:64], sep="", collapse="")
          #S, spectroscopic strength
          #mXd$S[row] <- paste(mXL[row,65:74], sep="", collapse="")
          #DS
          #mXd$DS[row] <- paste(mXL[row,75:76], sep="", collapse="")
          #MS
          #mXd$MS[row] <- paste(mXL[row,78:79], sep="", collapse="")
          #Q
          #mXd$Q[row] <- paste(mXL[row,80], sep="", collapse="")  
          row <- row + 1
        }
        
        #assign M and Z columns (were joint up before; couldnt do in while loop)
        mXd$M <- as.numeric(str_extract(mXd$El, "[0-9]+"))
        mXd$El <- str_extract(mXd$El, "[aA-zZ]+")
        
        #TRIM white space in ID
        mXd$El <- trimws(mXd$El)
        mXd$Key <- trimws(mXd$Key)
        
        # #Split into factors
        # mXf <- split(mXd, factor(mXd$El))
        
        # ### Append to a final df 
        # if(run==1) {
        #   #write.table(mXd, file=name, append=T, row.names=F, col.names=T,  sep=",")
        #   mXT <- mXd
        # }
        # else {
        #   #write.table(mXd, file=name, append=T, row.names=F, col.names=F,  sep=",")
        #   mXT <- rbind(mXT, mXd)
        # }
        
        #### Spit out an mXd.csv file ####
        ##### By writing straight to file it convers whitespace entries straight to NA
        if(run==1) {write.table(mXd, file=name, append=T, row.names=F, col.names=T,  sep=",")}
        else {write.table(mXd, file=name, append=T, row.names=F, col.names=F,  sep=",")}
      
      }
      
      ##### END NNDC READ #####
      NNDC <- read_csv(name)
      
      ### Tidy Up NNDC
      NNDC <- filter(NNDC, !is.na(El), El != "NN")
      NNDC <- unique(NNDC)

      ## Sort out the Z and AX column
      NNDC$Z <- as.character(lapply(NNDC$El, whi))
      NNDC <- NNDC %>% rename(AX=Key)
      NNDC <- tidy_Jpi(NNDC)
      NNDC <- filter(NNDC, Multiple_Jpi == FALSE) 
      
      #NNDC <- tidy_Jpi(NNDC)
      write.table(NNDC, file=Levels_Output_Name, append=T, row.names=F, col.names=T,  sep=",")

      
      
      
      
      
