# Run markdown to produce output ------------------------------------------
rmarkdown::render("2_code/r_training.Rmd",
                  output_file = "r_training.html",
                  output_dir = "3_outputs")
rm(list = ls())
