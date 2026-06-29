# R/descriptive/patient_tracker.R
# Create a patient-level tracker for analysis inclusion.

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
}))

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))
source(file.path(".", "R", "cleaning_utilities.R"))
source(file.path(".", "R", "descriptive", "functions", "cleaning_functions.R"))

clean_patient_ids <- function(x) {
  x <- str_trim(as.character(x))
  sort(unique(x[!is.na(x) & nzchar(x)]))
}

patient_ids_from <- function(df) {
  if (!"patient_uuid" %in% names(df)) {
    stop("Expected a patient_uuid column.", call. = FALSE)
  }

  clean_patient_ids(df$patient_uuid)
}

compute_growth_zscores <- function(df_growth) {
  df_growth <- df_growth %>%
    mutate(
      age_months = round(age_days / 30),
      sex_code   = if_else(sex == "male", 1L, 2L)
    )

  lms_refs <- list(
    "Body weight"        = load_who_lms("Body weight"),
    "Body height"        = load_who_lms("Body height"),
    "Head circumference" = load_who_lms("Head circumference")
  )

  bind_rows(lapply(names(lms_refs), function(parameter_name) {
    ref_tbl <- lms_refs[[parameter_name]] %>% rename(age_months = age)

    df_growth %>%
      filter(parameter == parameter_name) %>%
      left_join(ref_tbl, by = c("age_months", "sex_code" = "sex")) %>%
      mutate(
        z = if_else(
          L == 0,
          log(value / M) / S,
          (((value / M) ^ L) - 1) / (L * S)
        )
      )
  }))
}

read_3yr_cluster_patients <- function() {
  cluster_path <- file.path(DATA_PROCESSED, "unknown_excluded", "3yr_clusters.csv")

  if (!file.exists(cluster_path)) {
    stop("No unknown-excluded 3-year cluster file found in data/processed.", call. = FALSE)
  }

  read_csv(cluster_path, show_col_types = FALSE) %>%
    patient_ids_from() %>%
    clean_patient_ids()
}

# Descriptive analysis cohorts
seizure_patients <- suppressWarnings(clean_seizure_data(include_spasms = TRUE)) %>%
  filter(tolower(type) != "clonic") %>%
  patient_ids_from()

medication_patients <- clean_medication_data() %>%
  patient_ids_from()

growth_patients <- clean_growth_data() %>%
  compute_growth_zscores() %>%
  filter(!is.na(z), age_months >= 0, age_months <= 60) %>%
  patient_ids_from()

diagnosis_patients <- clean_diagnoses_data()$diagnoses %>%
  patient_ids_from()

hospitalization_patients <- clean_hospitalization_data() %>%
  patient_ids_from()

# Timeline cohort
suppressPackageStartupMessages({
  source(file.path(".", "R", "timeline", "functions", "cleaning_functions.R"))
  source(file.path(".", "R", "timeline", "functions", "analysis_functions.R"))
})

df_duration <- clean_medication_data()
censor_ages <- compute_censor_ages(df_duration) %>%
  mutate(censor_age_months = censor_age_days / 30)

df_type <- suppressWarnings(clean_seizure_data()) %>%
  left_join(censor_ages, by = "patient_uuid") %>%
  filter(age_days <= censor_age_days) %>%
  select(-censor_age_days, -censor_age_months)

appointment_summary <- compute_appointment_summary() %>%
  left_join(censor_ages %>% select(patient_uuid, censor_age_months), by = "patient_uuid") %>%
  mutate(
    last_appointment  = pmin(last_appointment, censor_age_months),
    first_appointment = pmin(first_appointment, censor_age_months)
  ) %>%
  select(-censor_age_months)

timeline_patients <- calculate_seizure_index_comparisons(
  df_type = df_type,
  df_duration = df_duration,
  appointment_summary = appointment_summary
) %>%
  patient_ids_from()

cluster_patients <- read_3yr_cluster_patients()

patient_sets <- list(
  seizure         = seizure_patients,
  medication      = medication_patients,
  growth          = growth_patients,
  diagnosis       = diagnosis_patients,
  hospitalization = hospitalization_patients,
  timeline        = timeline_patients,
  cluster         = cluster_patients
)

all_patients <- patient_sets %>%
  unlist(use.names = FALSE) %>%
  clean_patient_ids()

patient_tracker <- tibble(
  patient_uuid    = all_patients,
  seizure         = as.integer(patient_uuid %in% seizure_patients),
  medication      = as.integer(patient_uuid %in% medication_patients),
  growth          = as.integer(patient_uuid %in% growth_patients),
  diagnosis       = as.integer(patient_uuid %in% diagnosis_patients),
  hospitalization = as.integer(patient_uuid %in% hospitalization_patients),
  timeline        = as.integer(patient_uuid %in% timeline_patients),
  cluster         = as.integer(patient_uuid %in% cluster_patients)
)

out_path <- file.path(DATA_PROCESSED, "patient_tracker.csv")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write_csv(patient_tracker, out_path)

message("Patient tracker written to: ", normalizePath(out_path))
message("Rows: ", nrow(patient_tracker))
print(summarise(patient_tracker, across(-patient_uuid, sum)))
