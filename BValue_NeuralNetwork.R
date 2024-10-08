library(tidyverse)
library(stringr)
library(plotly)
library(XML)
library(processx)
library(pbapply)
library(RColorBrewer)


Gammas <-  read_csv("OUTPUTS/ND_Gamma_Dec2023_BKnown.csv")

### Training variables: Mass, Multipolarity, Energy
