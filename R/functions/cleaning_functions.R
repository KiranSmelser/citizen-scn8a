# R/functions/cleaning_functions.R
# Functions for common data cleaning tasks

# Load packages
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(lubridate)
library(tibble)

# Source configuration for file paths
source(file.path(".", "R", "functions", "config.R"))

# Recode unique_types: convert values 2-5 to "2+"
recode_unique_types <- function(x) {
  x <- as.character(x)
  x[x %in% c("2", "3", "4", "5")] <- "2+"
  factor(x, levels = c("0", "1", "2+"))
}

# Convert Onset_group to an ordered factor
factor_onset_group <- function(x) {
  factor(x, levels = c("1-4 Months", "Neonatal", "4-7 Months", "7-12 Months", "12+ Months"))
}

# Function to convert specified columns to factor
convert_to_factor <- function(df, cols) {
  for (col in cols) {
    df[[col]] <- as.factor(df[[col]])
  }
  return(df)
}

# Conversion functions for inequality symbols in seizure values
convert_greater_than <- function(x) {
  if (grepl(">", x)) {
    x <- as.numeric(sub(">", "", x)) + 1
  }
  return(x)
}
convert_less_than <- function(x) {
  if (grepl("<", x)) {
    x <- as.numeric(sub("<", "", x)) - 1
  }
  return(x)
}
convert_greater_than_equal_to <- function(x) {
  if (grepl("≥", x)) {
    x <- as.numeric(sub("≥", "", x))
  }
  return(x)
}
convert_less_than_equal_to <- function(x) {
  if (grepl("≤", x)) {
    x <- as.numeric(sub("≤", "", x))
  }
  return(x)
}

# One-hot encode columns based on classifier mapping.
one_hot_encode <- function(df, classifier, prefix) {
  for (col_name in colnames(classifier)) {
    corresponding_cols <- paste0(prefix, classifier[[col_name]])
    existing_cols <- corresponding_cols %in% colnames(df)
    if (any(existing_cols)) {
      df[[col_name]] <- ifelse(rowSums(df[corresponding_cols[existing_cols]]) > 0, 1, 0)
    }
  }
  return(df)
}

# Merge overlapping intervals for medication data
merge_intervals <- function(intervals_df) {
  intervals_df <- intervals_df %>%
    arrange(start_med_age)
  
  if (nrow(intervals_df) == 0) {
    return(
      tibble(
        start_med_age = numeric(0),
        end_med_age   = numeric(0)
      )
    )
  }
  
  # Initialize first interval
  current_start <- intervals_df$start_med_age[1]
  current_end   <- intervals_df$end_med_age[1]
  merged_list   <- list()
  
  if (nrow(intervals_df) > 1) {
    for (i in seq(2, nrow(intervals_df))) {
      s <- intervals_df$start_med_age[i]
      e <- intervals_df$end_med_age[i]
      
      if (s <= current_end) {
        # Overlapping intervals then merge
        current_end <- max(current_end, e, na.rm = TRUE)
      } else {
        # No overlap then add previous interval and reset
        merged_list[[length(merged_list) + 1]] <- tibble(
          start_med_age = current_start,
          end_med_age   = current_end
        )
        current_start <- s
        current_end   <- e
      }
    }
  }
  
  merged_list[[length(merged_list) + 1]] <- tibble(
    start_med_age = current_start,
    end_med_age   = current_end
  )
  
  bind_rows(merged_list)
}

# Clean and transform medication data
clean_medication_data <- function() {
  df_med <- read_excel(PATH_CITIZEN_DATA, sheet = "medication_aggregate")
  
  df_med <- df_med %>%
    mutate(
      medication = ifelse(
        grepl("ACTH", medication),
        "ACTH",
        medication
      ),
      medication = recode(
        medication,
        `Epidiolex` = "Epidiolex/CBD",
        `Cannabidiol` = "Epidiolex/CBD"
      )
    ) %>%
    filter(grepl(MEDS_TO_USE, medication))
  
  # Build intervals & merge them by patient + medication
  df_duration <- df_med %>%
    transmute(
      patient_uuid,
      medication,
      start_med_age = medication_age_days_firstDate,
      end_med_age   = medication_age_days_lastDate
    ) %>%
    distinct() %>%
    arrange(patient_uuid, medication, start_med_age, end_med_age) %>%
    group_by(patient_uuid, medication) %>%
    group_modify(~ merge_intervals(.x)) %>%
    ungroup() %>%
    # Drop any intervals <= 0 days
    filter((end_med_age - start_med_age) > 0) %>%
    group_by(patient_uuid, medication) %>%
    mutate(
      interval_id = row_number(),
      intervals_for_this_med = n()
    ) %>%
    ungroup() %>%
    mutate(
      medication = if_else(
        intervals_for_this_med > 1,
        paste0(medication, " ", interval_id),
        medication
      )
    )
  
  return(df_duration)
}

# Clean and transform seizure data
clean_seizure_data <- function() {
  df_sz <- read_excel(PATH_CITIZEN_DATA, sheet = "seizure_history")
  
  # Apply conversions for each type of inequality symbol
  df_sz$value[is.na(df_sz$seizure_history_value)] <- 1
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_greater_than)
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_greater_than_equal_to)
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_less_than)
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_less_than_equal_to)
  df_sz$seizure_history_value <- as.numeric(str_trim(df_sz$seizure_history_value))
  
  names(df_sz) <- make.names(sub('^seizure_history_', '', names(df_sz)), unique = TRUE)
  
  # Read classifier
  classifier <- read_excel(PATH_CLASSIFIER)
  classifier <- classifier[grepl("seizure_", names(classifier))]
  names(classifier) <- sub('^seizure_', '', names(classifier))
  
  # Filter for relevant seizure types from classifier
  df_type <- df_sz %>% 
    filter(type %in% c(classifier$`tonic-clonic`, classifier$focal, 
                       classifier$absence, classifier$tonic, classifier$myoclonic)) %>%
    mutate(type = case_when(
      type %in% classifier$`tonic-clonic` ~ "Tonic-clonic",
      type %in% classifier$focal ~ "Focal",
      type %in% classifier$absence ~ "Absence",
      type %in% classifier$tonic ~ "Tonic",
      type %in% classifier$myoclonic ~ "Myoclonic",
      TRUE ~ type
    ))
  
  # Read seizure indices
  tc_index       <- read_excel(PATH_TC_INDEX)      %>% mutate(type = "Tonic-clonic")
  focal_index    <- read_excel(PATH_FOCAL_INDEX)   %>% mutate(type = "Focal")
  absence_index  <- read_excel(PATH_ABSENCE_INDEX) %>% mutate(type = "Absence")
  tonic_index    <- read_excel(PATH_TONIC_INDEX)   %>% mutate(type = "Tonic")
  myoclonic_index <- read_excel(PATH_MYOCLONIC_INDEX) %>% mutate(type = "Myoclonic")
  
  # Combine into single index
  index <- bind_rows(tc_index, focal_index, absence_index, tonic_index, myoclonic_index)
  names(index) <- sub('^seizure_history_', '', names(index))
  names(index) <- sub('^seizure_', '', names(index))
  index <- subset(index, select = -c(3:5, 7))
  index$value <- as.numeric(index$value)
  
  # Join index values into df_type
  df_type <- left_join(
    df_type,
    unique(index),
    by = c("type", "value", "unit"),
    relationship = "many-to-many"
  )
  df_type$index[is.na(df_type$index)] <- 1
  
  # Remove LOF patients
  if ("subgroup_lof" %in% names(classifier)) {
    lof_patients <- classifier$subgroup_lof
    df_type <- df_type %>%
      filter(!patient_uuid %in% lof_patients)
  }
  
  return(df_type)
}

# Compute appointment summary
compute_appointment_summary <- function() {
  df_med_apts <- read_excel(PATH_CITIZEN_DATA, sheet = "medication_aggregate") %>%
    select(patient_uuid, appointment_age_days = medication_age_days_firstDate)
  
  df_sz_apts <- read_excel(PATH_CITIZEN_DATA, sheet = "seizure_history") %>%
    select(patient_uuid, appointment_age_days = seizure_history_age_days)
  
  df_diagnosis_apts <- read_excel(PATH_CITIZEN_DATA, sheet = "clinical_diagnosis") %>%
    select(patient_uuid, appointment_age_days = clinical_diagnosis_age_days_firstDate)
  
  appointment_data_all <- bind_rows(
    df_med_apts,
    df_sz_apts,
    df_diagnosis_apts
  ) %>%
    distinct() %>%
    mutate(appointment_age_months = appointment_age_days / 30)
  
  appointment_summary <- appointment_data_all %>%
    group_by(patient_uuid) %>%
    summarise(
      first_appointment = min(appointment_age_months, na.rm = TRUE),
      last_appointment  = max(appointment_age_months, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(appointment_summary)
}

# Function that computes the mean but if no values are present, 0 is returned.
safe_mean <- function(x) {
  if(length(x) == 0) return(0)
  m <- mean(x, na.rm = TRUE)
  if(is.nan(m)) return(0) else return(m)
}

# Clean and transform data for timeline plot
timeline_data <- function(df_sz, classifier) {
  # Subset classifier
  classifier <- classifier[grepl("^seizure_", names(classifier))]
  names(classifier) <- sub("^seizure_", "", names(classifier))
  
  # Apply conversions for each type of inequality symbol
  df_sz$value[is.na(df_sz$value)] <- 1
  df_sz$value <- sapply(df_sz$value, convert_greater_than)
  df_sz$value <- sapply(df_sz$value, convert_greater_than_equal_to)
  df_sz$value <- sapply(df_sz$value, convert_less_than)
  df_sz$value <- sapply(df_sz$value, convert_less_than_equal_to)
  df_sz$value <- as.numeric(str_trim(df_sz$value))
  
  genetics <- read_excel(PATH_CITIZEN_DATA, sheet = "genetic_findings")
  demographics <- read_excel(PATH_CITIZEN_DATA, sheet = "demographics")
  df_diag <- read_excel(PATH_CITIZEN_DATA, sheet = "diagnostic_procedures")
  hospitalizations <- read_excel(PATH_CITIZEN_DATA, sheet = "hospital_admission")
  
  # Captures all of a patient's appointments
  df_med_apts <- read_excel(PATH_CITIZEN_DATA, sheet = "medication_aggregate")
  df_sz_apts <- df_sz %>% 
    select(patient_uuid, age_days)
  df_diagnosis_apts <- read_excel(PATH_CITIZEN_DATA, sheet = "clinical_diagnosis") %>%
    select(patient_uuid, clinical_diagnosis_age_days_firstDate)
  
  overlap_patients <- read_excel(PATH_OVERLAP_PATIENTS)
  
  # Filter adverse effects for "Moderate" and "Severe"
  adverse_effects <- read_excel(PATH_CITIZEN_DATA, sheet = "adverse_effects")
  adverse_effect_severity <- read_excel(PATH_EFFECTS_SEVERITY)
  adverse_effects <- adverse_effects %>%
    inner_join(
      adverse_effect_severity %>% 
        filter(severity_score %in% c("Moderate", "Severe")), 
      by = "adverse_effect"
    )
  
  # Identify infantile spasms from raw seizure data
  df_spasms <- df_sz %>% 
    filter(type %in% classifier$spasms) %>% 
    mutate(type = "Infantile Spasms")
  
  # Create periods where seizure reports are not 0
  df_spasm_periods <- df_spasms %>%
    arrange(patient_uuid, age_days) %>%
    group_by(patient_uuid) %>%
    mutate(
      age_months = age_days / 30,
      value_zero = (value == 0),
      period_id = cumsum(lag(value_zero, default = TRUE) & !value_zero)
    ) %>%
    filter(value > 0) %>%
    group_by(patient_uuid, period_id) %>%
    summarise(
      spasm_start_age = min(age_months, na.rm = TRUE),
      spasm_end_age   = max(age_months, na.rm = TRUE),
      num_reports     = n(),
      .groups = "drop"
    ) %>%
    mutate(
      is_single_report = (num_reports == 1)
    )
  
  # EEG & hypsarrhythmia
  df_eeg <- df_diag %>%
    filter(str_detect(tolower(procedure), "eeg")) %>%
    mutate(age_months = procedure_age_days / 30)
  
  df_hyps <- df_diag %>%
    filter(str_detect(tolower(procedure_findings), "hypsarrhythmia")) %>%
    mutate(age_months = procedure_age_days / 30)
  
  # Return everything needed for timeline/plotting
  list(
    genetics           = genetics,
    demographics       = demographics,
    df_diag            = df_diag,
    hospitalizations   = hospitalizations,
    df_med_apts        = df_med_apts,
    df_sz_apts         = df_sz_apts,
    df_diagnosis_apts  = df_diagnosis_apts,
    overlap_patients   = overlap_patients,
    adverse_effects     = adverse_effects,
    df_spasms          = df_spasms,
    df_spasm_periods   = df_spasm_periods,
    df_eeg             = df_eeg,
    df_hyps            = df_hyps
  )
}

# Prepare per‐patient chart data for patient charts
prepare_patient_chart_data <- function(pt, seizures_summary_combined, df_duration, df_type, timeline_data, df_dev, df_sz, demographics) {
  # Extract already loaded data from timeline data
  genetics           <- timeline_data$genetics
  overlap_patients   <- timeline_data$overlap_patients
  df_med_apts        <- timeline_data$df_med_apts
  df_sz_apts         <- timeline_data$df_sz_apts
  df_diagnosis_apts  <- timeline_data$df_diagnosis_apts
  hospitalizations   <- timeline_data$hospitalizations
  adverse_effects    <- timeline_data$adverse_effects
  df_spasm_periods   <- timeline_data$df_spasm_periods
  df_eeg             <- timeline_data$df_eeg
  df_hyps            <- timeline_data$df_hyps
  
  # Retrieve patient's protein mutation from genetics or overlap data
  protein_mutation <- genetics %>%
    filter(patient_uuid == pt, gene == "SCN8A") %>%
    pull(variant_protein) %>%
    first()
  if (is.na(protein_mutation)) {
    protein_mutation <- overlap_patients %>%
      filter(`Patient ID` == pt) %>%
      pull(`p.`) %>%
      first()
  }
  protein_mutation <- ifelse(is.na(protein_mutation), "NA", protein_mutation)
  
  # Retrieve patient registry number
  patient_reg_num <- overlap_patients %>%
    filter(`Patient ID` == pt) %>%
    pull(`Registry #`) %>%
    first()
  patient_reg_num <- ifelse(!is.na(patient_reg_num), paste0(" (Registry #", patient_reg_num, ")"), "")
  
  # Check if patient has infantile spasms
  has_infantile_spasms <- df_sz %>%
    filter(patient_uuid == pt, type == "Infantile spasms") %>%
    nrow() > 0
  title_suffix <- if (has_infantile_spasms) " - IF" else ""
  
  # Create timeline title
  timeline_title <- paste(pt, patient_reg_num, " (", protein_mutation, ")", title_suffix, sep = "")
  
  # Subset seizure summary data for patient
  pt_data <- seizures_summary_combined %>% filter(patient_uuid == pt)
  
  # Prepare medication duration data for patient
  pt_data_duration <- df_duration %>%
    filter(patient_uuid == pt) %>%
    mutate(
      start_med_age_months = start_med_age / 30,
      end_med_age_months   = end_med_age / 30,
      first_3_months_end   = pmin(start_med_age_months + 3, end_med_age_months),
      medication_base      = sub(" \\d+$", "", medication)
    )
  
  # Compute total duration per medication and order medications
  duration_order <- pt_data_duration %>%
    group_by(medication) %>%
    summarise(
      total_duration = sum(end_med_age - start_med_age, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_duration))
  
  pt_data <- pt_data %>%
    left_join(duration_order, by = "medication") %>%
    arrange(total_duration) %>%
    mutate(medication = factor(medication, levels = unique(medication)))
  
  # Prepare data for line plot (seizure index vs. time)
  pt_data_type <- df_type %>%
    filter(patient_uuid == pt) %>%
    mutate(age_months = age_days / 30)
  
  # Collect appointment data and convert to months
  appointment_data <- bind_rows(
    df_med_apts %>% filter(patient_uuid == pt) %>% select(appointment_age_days = medication_age_days_firstDate),
    df_sz_apts %>% filter(patient_uuid == pt) %>% select(appointment_age_days = age_days),
    df_diagnosis_apts %>% filter(patient_uuid == pt) %>% select(appointment_age_days = clinical_diagnosis_age_days_firstDate)
  ) %>%
    distinct() %>%
    mutate(appointment_age_months = appointment_age_days / 30)
  
  # Demographics for patient
  pt_demographics <- demographics %>%
    filter(patient_uuid == pt) %>%
    mutate(most_recent_record_age_months = most_recent_records_age_days / 30)
  
  # Adverse effects data for patient
  pt_data_adverse <- adverse_effects %>%
    filter(patient_uuid == pt) %>%
    mutate(age_months = adverse_effect_age_days_firstDate / 30)
  
  # Hospitalizations for status events
  pt_data_status <- hospitalizations %>%
    filter(patient_uuid == pt, admission_diagnosis == "Status epilepticus") %>%
    mutate(age_months = admission_age_days_firstDate / 30)
  
  # Developmental milestone data for patient
  pt_dev_data <- df_dev %>%
    filter(patient_uuid == pt) %>%
    group_by(domain_milestone) %>%
    arrange(domain_age_days_firstDate) %>%
    mutate(
      age_months = domain_age_days_firstDate / 30,
      status_change = case_when(
        domain_status == "Unable" & lag(domain_status) == "Able" ~ "Loss",
        domain_status == "Able" & lag(domain_status) == "Unable" ~ "Gained",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(status_change)) %>%
    mutate(status_change = factor(status_change, levels = c("Gained", "Loss"))) %>%
    group_by(age_months, status_change, domain) %>%
    mutate(
      n_points = n(),
      y_offset = row_number() - (n_points + 1) / 2
    ) %>%
    ungroup()
  
  # Order medications based on earliest start age
  pt_data_duration <- pt_data_duration %>%
    group_by(medication_base) %>%
    mutate(earliest_start = min(start_med_age_months)) %>%
    ungroup()
  med_order <- pt_data_duration %>%
    distinct(medication_base, earliest_start) %>%
    arrange(earliest_start) %>%
    pull(medication_base)
  
  # Filter spasm, EEG, and hypsarrhythmia data
  pt_spasm_periods <- df_spasm_periods %>% filter(patient_uuid == pt)
  pt_eeg           <- df_eeg %>% filter(patient_uuid == pt)
  pt_hyps          <- df_hyps %>% filter(patient_uuid == pt)
  
  # Return list with all patient-specific data for plotting
  list(
    timeline_title   = timeline_title,
    pt_data          = pt_data,
    pt_data_duration = pt_data_duration,
    med_order        = med_order,
    pt_data_type     = pt_data_type,
    appointment_data = appointment_data,
    pt_demographics  = pt_demographics,
    pt_data_adverse  = pt_data_adverse,
    pt_data_status   = pt_data_status,
    pt_dev_data      = pt_dev_data,
    pt_spasm_periods = pt_spasm_periods,
    pt_eeg           = pt_eeg,
    pt_hyps          = pt_hyps
  )
}

# Read onset ages file and compute onset group
clean_onset_data <- function() {
  onset_data <- read_csv(PATH_ONSET_AGES, show_col_types = FALSE)
  
  onset_data <- onset_data %>%
    group_by(UUID) %>%
    summarise(age_onset = min(Age_onset, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      Onset_group = case_when(
        age_onset <= 30 ~ "Neonatal",
        age_onset > 30 & age_onset <= 120 ~ "1-4 Months",
        age_onset > 120 & age_onset <= 210 ~ "4-7 Months",
        age_onset > 210 & age_onset <= 365 ~ "7-12 Months",
        TRUE ~ "12+ Months"
      ),
      age_onset_m = age_onset / 30
    )
  return(onset_data)
}

# Clean initial seizure types
clean_initial_seizure_types <- function() {
  initial_classifier <- read_excel(PATH_INITIAL_CLASSIFIER)
  
  seizure_data <- read_seizure_history() %>%
    select(patient_uuid, seizure_history_type, seizure_history_age_days) %>%
    filter(seizure_history_age_days < AGE_CUTOFF_DAYS) %>%
    rename(UUID = patient_uuid)
  
  seizure_data <- seizure_data %>%
    mutate(
      focal         = ifelse(seizure_history_type %in% initial_classifier$focal, 1, 0),
      bilateral_tc  = ifelse(seizure_history_type %in% initial_classifier$bilateral_tonic_clonic, 1, 0),
      absence       = ifelse(seizure_history_type %in% initial_classifier$absence, 1, 0),
      infantile     = ifelse(seizure_history_type %in% initial_classifier$infantile_spasms, 1, 0)
    )
  
  initial_seizures <- seizure_data %>%
    group_by(UUID) %>%
    summarise(
      focal = sum(focal, na.rm = TRUE),
      bilateral_tc = sum(bilateral_tc, na.rm = TRUE),
      absence = sum(absence, na.rm = TRUE),
      infantile = sum(infantile, na.rm = TRUE)
    ) %>%
    mutate(
      focal = ifelse(focal > 0, 1, 0),
      bilateral_tc = ifelse(bilateral_tc > 0, 1, 0),
      absence = ifelse(absence > 0, 1, 0),
      infantile = ifelse(infantile > 0, 1, 0)
    )
  return(initial_seizures)
}

# Clean EEG data for abnormal EEG reports
clean_abnormal_eeg <- function() {
  classifier <- readxl::read_excel(PATH_CLASSIFIER)
  eeg_data <- readxl::read_excel(PATH_CITIZEN_DATA, sheet = "diagnostic_procedures") %>%
    filter(stringr::str_detect(procedure, "EEG")) %>%
    select(patient_uuid, procedure_findings, procedure_age_days) %>%
    filter(procedure_age_days < AGE_CUTOFF_DAYS) %>%
    mutate(abnormal_eeg = ave(!(procedure_findings %in% classifier$eeg_normal),
                                     patient_uuid,
                                     FUN = function(x) as.integer(any(x)))) %>%
    rename(UUID = patient_uuid) %>%
    distinct(UUID, abnormal_eeg)
  return(eeg_data)
}