# 00_main.R

library(tidyverse)
library(patchwork)
library(xtable)
library(ggraph)
library(igraph)
library(Matrix)

# __ Clean environment _________________________________________________________
rm(list = ls())

# __ Preprocess data ___________________________________________________________
source("scripts/11_data_load.R")

source("scripts/12_data_preprocess.R")

# __ Predictions _______________________________________________________________
source("scripts/20_final_predictions.R")

 # __ Validation ________________________________________________________________
source("scripts/50_validation.R")