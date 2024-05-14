#--------------------------------------------------------------------------
# Script Name: main_script.R
# 
# Author: Marcus Hagman
# Date: 2024-04-26
# 
# Purpose: Runs all the other scripts
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox/gas-col"))

source("02_R/01_scripts/01_data_cleaning_population.R")
source("02_R/01_scripts/02_data_cleaning_prices.R")
source("02_R/01_scripts/03_data_cleaning_stations.R")
source("02_R/01_scripts/04_oil_cleaning.R")
source("02_R/01_scripts/05_gas_stations_descriptives.R")
source("02_R/01_scripts/06_data_merging.R")
source("02_R/01_scripts/07_collusion_identification.R")
source("02_R/01_scripts/08_ecm_data_prep.R")
source("02_R/01_scripts/09_ecm_analysis.R")
source("02_R/01_scripts/10_price_descriptives.R")
source("02_R/01_scripts/11_effect_openings.R")