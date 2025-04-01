# R/functions/config.R
# Global configuration for file paths and parameters

# Data directories
DATA_RAW       <- "./data/raw/"
DATA_PROCESSED <- "./data/processed/"

# Data files
PATH_SUPP_DATA    <- file.path(DATA_RAW, "Supplementary_Table - Study Data by Groups.csv")
PATH_CITIZEN_DATA <- file.path(DATA_RAW, "Ciitizen_SCN8A_UArizona_2024.02.09.xlsx")
PATH_ONSET_AGES   <- file.path(DATA_RAW, "onset_ages.csv")

# Classifier file
PATH_CLASSIFIER <- "./data/classifiers/ciitizen_health_classifier.xlsx"

# Additional index files
PATH_TC_INDEX <- file.path(DATA_RAW, "tonic-clonic_index.xlsx")
PATH_FOCAL_INDEX <- file.path(DATA_RAW, "focal_index.xlsx")
PATH_MYOCLONIC_INDEX <- file.path(DATA_RAW, "myoclonic_index.xlsx")
PATH_ABSENCE_INDEX <- file.path(DATA_RAW, "absence_index.xlsx")
PATH_TONIC_INDEX <- file.path(DATA_RAW, "tonic_index.xlsx")

# Global parameters
AGE_CUTOFF_DAYS <- 1095  # 3 years in days