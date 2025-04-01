# R/functions/data_import_functions.R
# Functions to import your main datasets

library(readr)
library(readxl)

read_supp_data <- function() {
  data <- read_csv(PATH_SUPP_DATA, show_col_types = FALSE)
  data <- data[,-1]
  data
}

read_citizen_data <- function() {
  read_csv(PATH_CITIZEN_DATA, show_col_types = FALSE)
}

read_classifier <- function() {
  read_excel(PATH_CLASSIFIER)
}