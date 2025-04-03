# R/functions/config.R
# Global configuration for file paths and parameters

# Data directories
DATA_RAW       <- "./data/raw/"
DATA_PROCESSED <- "./data/processed/"
DATA_CLASSIFIERS <- "./data/classifiers/"

# Data files
PATH_SUPP_DATA    <- file.path(DATA_RAW, "Supplementary_Table - Study Data by Groups.csv")
PATH_CITIZEN_DATA <- file.path(DATA_RAW, "Ciitizen_SCN8A_UArizona_2024.02.09.xlsx")
PATH_ONSET_AGES   <- file.path(DATA_RAW, "onset_ages.csv")

# Classifier files
PATH_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "ciitizen_health_classifier.xlsx")
PATH_HOSPITALIZATION_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "Grouping Hospitalizations.xlsx")
PATH_OVERLAP_PATIENTS <- file.path(DATA_CLASSIFIERS, "Overlap Patients corrected.xlsx")
PATH_EFFECTS_SEVERITY <- file.path(DATA_CLASSIFIERS, "effects_severity.xlsx")

# Additional index files
PATH_TC_INDEX <- file.path(DATA_RAW, "tonic-clonic_index.xlsx")
PATH_FOCAL_INDEX <- file.path(DATA_RAW, "focal_index.xlsx")
PATH_MYOCLONIC_INDEX <- file.path(DATA_RAW, "myoclonic_index.xlsx")
PATH_ABSENCE_INDEX <- file.path(DATA_RAW, "absence_index.xlsx")
PATH_TONIC_INDEX <- file.path(DATA_RAW, "tonic_index.xlsx")

# Global parameters
AGE_CUTOFF_DAYS <- 1095  # 3 years in days