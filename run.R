# Run markdown to produce output ------------------------------------------
rmarkdown::render("r_training.Rmd",
                  output_file = "r_training.html",
                  output_dir = "outputs")
rm(list = ls())
