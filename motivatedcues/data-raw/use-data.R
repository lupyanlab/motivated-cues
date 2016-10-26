library(devtools)
library(readr)
library(stringr)

data_files <- list.files("data-raw", pattern = "*.csv", full.names = TRUE)
stem <- function(path) str_replace_all(strsplit(basename(path), "\\.")[[1]][1], "-", "_")

for(path in data_files) {
  frame <- read_csv(path)
  name <- stem(path)
  assign(name, frame)
}

imageratings <- read_csv("data-raw/TYP/imageratings.csv")

use_data(
  exp1_final_rep,
  typ_final,
  imageratings,
  overwrite = TRUE
)
