# R/scripts/longitudinal_patient_analysis.R
# Generate longitudinal patient table

# Import Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Source Files
source(file.path(".", "R", "functions", "config.R"))
source(file.path(".", "R", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "functions", "data_import_functions.R"))
source(file.path(".", "R", "functions", "analysis_functions.R"))

# Data Cleaning and Setup
df_type       <- suppressWarnings(clean_seizure_data())
df_duration   <- clean_medication_data()
appointment_summary <- compute_appointment_summary()  

# Create Table from Seizure Data
df_table_data <- df_type %>%
  select(patient_uuid, type, index, age_days) %>%
  mutate(
    age_in_months = age_days / 30,
    type = recode(type, !!!ABBREVIATIONS_SEIZURES)
  )

# Compute Summary Measures
seizure_counts <- get_seizure_counts(df_table_data)
seizure_gaps   <- get_seizure_gaps(df_table_data)

# Seizure Index Comparisons
seizures_summary_combined <- calculate_seizure_index_comparisons(df_type, df_duration, appointment_summary)
sz_comb_sum <- seizures_summary_combined %>%
  mutate(
    medication = str_replace(medication, "\\s+\\d+$", ""),
    medication = str_trim(medication),
    medication = case_when(
      medication %in% names(ABBREVIATIONS_MEDS) ~ ABBREVIATIONS_MEDS[medication],
      TRUE ~ medication
    )
  ) %>%
  group_by(patient_uuid) %>%
  mutate(follow_up = round(max(last_appointment, na.rm = TRUE) - min(first_appointment, na.rm = TRUE), 2)) %>%
  ungroup()

# Process Medication Data
medication_df <- df_duration %>%
  mutate(
    medication = str_replace(medication, "\\s+\\d+$", ""),
    medication = str_trim(medication),
    medication = case_when(
      medication %in% names(ABBREVIATIONS_MEDS) ~ ABBREVIATIONS_MEDS[medication],
      TRUE ~ medication
    )
  ) %>%
  group_by(patient_uuid) %>%
  summarise(
    number_med_types = n_distinct(medication),
    med_types = paste(unique(medication), collapse = ", "),
    .groups = "drop"
  )

# Compute Current vs. Weened Medications
current_weaned_df <- get_medication_status(df_duration, appointment_summary)

# Determine Medications During Gaps
gap_meds_join <- get_gap_medications(seizure_gaps, sz_comb_sum)

# Combine
combined_df <- seizure_counts %>%
  left_join(seizure_gaps, by = "patient_uuid") %>%
  left_join(medication_df, by = "patient_uuid") %>%
  left_join(current_weaned_df, by = "patient_uuid") %>%
  left_join(gap_meds_join, by = "patient_uuid") %>%
  left_join(sz_comb_sum %>% select(patient_uuid, follow_up) %>% distinct(), by = "patient_uuid") %>%
  mutate(
    number_med_types_gap = as.character(number_med_types_gap),
    number_med_types_gap = if_else(is.na(number_med_types_gap), "None", number_med_types_gap),
    med_types_gap = if_else(is.na(med_types_gap), "None", med_types_gap),
    gap_period = gsub("-", " to ", gap_period)
  )

output_path <- file.path(".", "output", "results", "combined_longitudinal_table.csv")
write.csv(combined_df, output_path, row.names = FALSE)