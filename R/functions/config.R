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

# Medication names to use in cleaning
MEDS_TO_USE <- paste(
  c("Adrenocorticotropin (ACTH 1-18),I-125 (TYR)", "Clonazepam", "Levetiracetam", 
    "Phenytoin", "Oxcarbazepine", "Carbamazepine", "Phenobarbital", 
    "Lamotrigine", "Briveracetam", "Cannabidiol", "Clobazam", "Epidiolex", 
    "Eslicarbazepine", "Ethosuximide", "Felbamate", "Gabapentin", 
    "Prednisolone", "Lacosamide", "Primidone", "Rufinamide", "Topiramate", 
    "Valproate", "Vigabatrin", "Zonisamide", "Stiripentol", "Tiagabine", 
    "Perampanel"),
  collapse = "|"
)

# Abbreviations for recoding medications and seizure types
ABBREVIATIONS_MEDS <- c(
  "Oxcarbazepine" = "OXC",
  "Lacosamide" = "LCM",
  "Phenytoin" = "PHT", 
  "Valproate" = "VPA", 
  "Lamotrigine" = "LTG", 
  "Carbamazepine" = "CBZ", 
  "Rufinamide" = "RFM",
  "Eslicarbazepine" = "ESL", 
  "Clonazepam" = "CLZ",
  "Clobazam" = "CLB",
  "Phenobarbital" = "PBT", 
  "Vigabatrin" = "VBG", 
  "Felbamate" = "FBM",
  "Primidone" = "PRM", 
  "Stiripentol" = "STP", 
  "Tiagabine" = "TGB", 
  "Zonisamide" = "ZNS",
  "Gabapentin" = "GBP",
  "Ethosuximide" = "ETX", 
  "Levetiracetam" = "LEV", 
  "Briveracetam" = "BRV", 
  "ACTH" = "ACTH",
  "Epidiolex/CBD" = "CBD",
  "Topiramate" = "TPM",
  "Prednisolone" = "PRD",
  "Perampanel" = "PER",
  "None" = "None"
)
ABBREVIATIONS_SEIZURES <- c(
  "Tonic-clonic" = "BTC",
  "Focal" = "FOC",
  "Absence" = "ABS",
  "Tonic" = "TON",
  "Myoclonic" = "MYC"
)