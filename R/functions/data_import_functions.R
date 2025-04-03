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

library(readxl)

read_medication_aggregate <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "medication_aggregate")
}

read_seizure_history <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "seizure_history")
}

read_clinical_diagnosis <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "clinical_diagnosis")
}

read_development_data <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "development")
}

read_adverse_effects <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "adverse_effects")
}

read_hospitalizations <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "hospital_admission")
}

read_overlap_patients <- function() {
  read_excel(PATH_OVERLAP_PATIENTS)
}