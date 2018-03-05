# Load required libraries -------------------------------------------------
library(rmarkdown)
library(tidyverse)

# Load required data ------------------------------------------------------
swfc_16_init <- "Data/swfc_2016_machine_readable.csv" %>% 
  read.csv()

# Create list of regions --------------------------------------------------
regions <- swfc_16_init$Government_Office_Region_Name %>% 
  levels()

# Produce report for each -------------------------------------------------
lapply(regions, function(region_name)
  render('region_factsheet.Rmd',
         output_file =  paste0(region_name,".html"),
         output_dir = 'Outputs'))
