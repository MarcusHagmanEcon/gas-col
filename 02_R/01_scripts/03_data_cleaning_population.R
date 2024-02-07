#--------------------------------------------------------------------------
# Script Name: 00_data_cleaning_population.R.R
# 
# Author: Marcus Hagman
# Date: 2023-10-18
# 
# Purpose: This script puts the population dataset in a workable format.
#
# Input: - 01_data/01_raw/population.csv
# 
# Output: - 01_data/02_processed/cleaned_population.rds
#
# Instructions: 
#
# Revision History:
#--------------------------------------------------------------------------

rm(list=ls())

setwd("C:/Users/marcu/Documents/gas-col")


#install.packages("sf")
library(tidyverse)

# Load data
population <- read.csv("01_data/01_raw/population.csv", sep = ";")

population <- population %>% mutate(north = substr(Gitter_ID_1km, 17, 23) %>% as.integer(),
                                    east = substr(Gitter_ID_1km, 25, 31)  %>% as.integer()) %>% 
                             rename(inhabitants = Einwohner) %>%
                             select( c(north, east, inhabitants))

saveRDS(population, file = "01_data/02_processed/cleaned_population.rds")