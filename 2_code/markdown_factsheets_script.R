# Load required libraries -------------------------------------------------
library(rmarkdown)
library(tidyverse)

# Load required data ------------------------------------------------------
swfc_16_init <- "1_data/swfc_2016_machine_readable.csv" %>% 
  read.csv()

# Create list of regions --------------------------------------------------
regions <- swfc_16_init$Government_Office_Region_Name %>% 
  levels()

# Produce report for each -------------------------------------------------
lapply(regions, function(region_name)
  render('2_code/region_factsheet.Rmd',
         output_file =  paste0(region_name,".html"),
         output_dir = '3_outputs'))
