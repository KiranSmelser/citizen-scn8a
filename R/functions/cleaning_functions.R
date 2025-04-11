# R/functions/cleaning_functions.R
# Functions for common data cleaning tasks


# Library imports
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)

# Source configuration
source(file.path("R", "functions", "config.R"))
source(file.path("R", "functions", "data_import_functions.R"))
source(file.path("R", "functions", "cleaning_utilities.R"))

# Merge overlapping intervals for medication duration data
merge_intervals <- function(intervals_df) {
  intervals_df <- intervals_df %>% arrange(start_med_age)
  
  if (nrow(intervals_df) == 0) {
    return(tibble(start_med_age = numeric(0), end_med_age = numeric(0)))
  }
  
  current_start <- intervals_df$start_med_age[1]
  current_end   <- intervals_df$end_med_age[1]
  merged_list   <- list()
  
  if (nrow(intervals_df) > 1) {
    for (i in seq(2, nrow(intervals_df))) {
      s <- intervals_df$start_med_age[i]
      e <- intervals_df$end_med_age[i]
      
      if (s <= current_end) {
        current_end <- max(current_end, e, na.rm = TRUE)
      } else {
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
  df_med <- read_medication_aggregate()
  
  df_med <- df_med %>%
    mutate(
      medication = ifelse(grepl("ACTH", medication), "ACTH", medication),
      medication = recode(medication,
                          `Epidiolex` = "Epidiolex/CBD",
                          `Cannabidiol` = "Epidiolex/CBD")
    ) %>%
    filter(grepl(MEDS_TO_USE, medication))
  
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
    filter((end_med_age - start_med_age) > 0) %>%
    group_by(patient_uuid, medication) %>%
    mutate(
      interval_id = row_number(),
      intervals_for_this_med = n()
    ) %>%
    ungroup() %>%
    mutate(
      medication = if_else(intervals_for_this_med > 1,
                           paste0(medication, " ", interval_id),
                           medication)
    )
  return(df_duration)
}

# Clean seizure data
clean_seizure_data <- function(include_spasms = FALSE) {
  df_sz <- read_seizure_history()
  
  # Apply conversions for inequality symbols
  df_sz$value[is.na(df_sz$seizure_history_value)] <- 1
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_greater_than)
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_greater_than_equal_to)
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_less_than)
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_less_than_equal_to)
  df_sz$seizure_history_value <- as.numeric(str_trim(df_sz$seizure_history_value))
  
  names(df_sz) <- make.names(sub('^seizure_history_', '', names(df_sz)), unique = TRUE)
  
  classifier <- read_classifier()
  classifier <- classifier[grepl("seizure_", names(classifier))]
  names(classifier) <- sub('^seizure_', '', names(classifier))
  
  valid_types <- c(classifier$`tonic-clonic`, classifier$focal, classifier$absence,
                   classifier$tonic, classifier$myoclonic)
  if (include_spasms) {
    valid_types <- c(valid_types, classifier$spasms)
  }
  
  df_type <- df_sz %>% 
    filter(type %in% valid_types) %>%
    mutate(type = case_when(
      type %in% classifier$`tonic-clonic` ~ "Tonic-clonic",
      type %in% classifier$focal ~ "Focal",
      type %in% classifier$absence ~ "Absence",
      type %in% classifier$tonic ~ "Tonic",
      type %in% classifier$myoclonic ~ "Myoclonic",
      include_spasms & type %in% classifier$spasms ~ "Spasms",
      TRUE ~ type
    ))
  
  # Read and combine index sheets
  tc_index        <- read_excel(PATH_TC_INDEX)      %>% mutate(type = "Tonic-clonic")
  focal_index     <- read_excel(PATH_FOCAL_INDEX)     %>% mutate(type = "Focal")
  absence_index   <- read_excel(PATH_ABSENCE_INDEX)   %>% mutate(type = "Absence")
  tonic_index     <- read_excel(PATH_TONIC_INDEX)     %>% mutate(type = "Tonic")
  myoclonic_index <- read_excel(PATH_MYOCLONIC_INDEX) %>% mutate(type = "Myoclonic")
  
  index <- bind_rows(tc_index, focal_index, absence_index, tonic_index, myoclonic_index)
  names(index) <- sub('^seizure_history_', '', names(index))
  names(index) <- sub('^seizure_', '', names(index))
  index <- subset(index, select = -c(3:5, 7))
  index$value <- as.numeric(index$value)
  
  df_type <- left_join(
    df_type,
    unique(index),
    by = c("type", "value", "unit"),
    relationship = "many-to-many"
  )
  df_type$index[is.na(df_type$index)] <- 1
  
  if ("subgroup_lof" %in% names(classifier)) {
    lof_patients <- classifier$subgroup_lof
    df_type <- df_type %>% filter(!patient_uuid %in% lof_patients)
  }
  
  return(df_type)
}

# Compute appointment summary
compute_appointment_summary <- function() {
  df_med_apts <- read_medication_aggregate() %>%
    select(patient_uuid, appointment_age_days = medication_age_days_firstDate)
  
  df_sz_apts <- read_seizure_history() %>%
    select(patient_uuid, appointment_age_days = seizure_history_age_days)
  
  df_diagnosis_apts <- read_excel(PATH_CITIZEN_DATA, sheet = "clinical_diagnosis") %>%
    select(patient_uuid, appointment_age_days = clinical_diagnosis_age_days_firstDate)
  
  appointment_data_all <- bind_rows(
    df_med_apts,
    df_sz_apts,
    df_diagnosis_apts
  ) %>% distinct() %>% mutate(appointment_age_months = appointment_age_days / 30)
  
  appointment_summary <- appointment_data_all %>%
    group_by(patient_uuid) %>%
    summarise(
      first_appointment = min(appointment_age_months, na.rm = TRUE),
      last_appointment  = max(appointment_age_months, na.rm = TRUE),
      .groups = "drop"
    )
  return(appointment_summary)
}

# Prepare timeline data for plotting
timeline_data <- function(df_sz, classifier) {
  classifier <- classifier[grepl("^seizure_", names(classifier))]
  names(classifier) <- sub("^seizure_", "", names(classifier))
  
  df_sz$value[is.na(df_sz$value)] <- 1
  df_sz$value <- sapply(df_sz$value, convert_greater_than)
  df_sz$value <- sapply(df_sz$value, convert_greater_than_equal_to)
  df_sz$value <- sapply(df_sz$value, convert_less_than)
  df_sz$value <- sapply(df_sz$value, convert_less_than_equal_to)
  df_sz$value <- as.numeric(str_trim(df_sz$value))
  
  genetics         <- read_excel(PATH_CITIZEN_DATA, sheet = "genetic_findings")
  demographics     <- read_excel(PATH_CITIZEN_DATA, sheet = "demographics")
  df_diag          <- read_excel(PATH_CITIZEN_DATA, sheet = "diagnostic_procedures")
  hospitalizations <- read_hospitalizations()
  df_med_apts      <- read_medication_aggregate()
  df_sz_apts       <- df_sz %>% select(patient_uuid, age_days)
  df_diagnosis_apts<- read_excel(PATH_CITIZEN_DATA, sheet = "clinical_diagnosis") %>%
    select(patient_uuid, clinical_diagnosis_age_days_firstDate)
  overlap_patients <- read_excel(PATH_OVERLAP_PATIENTS)
  
  adverse_effects <- read_excel(PATH_CITIZEN_DATA, sheet = "adverse_effects")
  adverse_effect_severity <- read_excel(PATH_EFFECTS_SEVERITY)
  adverse_effects <- adverse_effects %>% 
    inner_join(
      adverse_effect_severity %>% filter(severity_score %in% c("Moderate", "Severe")), 
      by = "adverse_effect"
    )
  
  df_spasms <- df_sz %>% filter(type %in% classifier$spasms) %>% mutate(type = "Infantile Spasms")
  
  df_spasm_periods <- df_spasms %>% arrange(patient_uuid, age_days) %>% group_by(patient_uuid) %>%
    mutate(
      age_months = age_days / 30,
      value_zero = (value == 0),
      period_id = cumsum(lag(value_zero, default = TRUE) & !value_zero)
    ) %>% filter(value > 0) %>% group_by(patient_uuid, period_id) %>% summarise(
      spasm_start_age = min(age_months, na.rm = TRUE),
      spasm_end_age   = max(age_months, na.rm = TRUE),
      num_reports     = n(),
      .groups = "drop"
    ) %>% mutate(is_single_report = (num_reports == 1))
  
  df_eeg <- df_diag %>% filter(str_detect(tolower(procedure), "eeg")) %>% mutate(age_months = procedure_age_days / 30)
  df_hyps <- df_diag %>% filter(str_detect(tolower(procedure_findings), "hypsarrhythmia")) %>% mutate(age_months = procedure_age_days / 30)
  
  list(
    genetics           = genetics,
    demographics       = demographics,
    df_diag            = df_diag,
    hospitalizations   = hospitalizations,
    df_med_apts        = df_med_apts,
    df_sz_apts         = df_sz_apts,
    df_diagnosis_apts  = df_diagnosis_apts,
    overlap_patients   = overlap_patients,
    adverse_effects    = adverse_effects,
    df_spasms          = df_spasms,
    df_spasm_periods   = df_spasm_periods,
    df_eeg             = df_eeg,
    df_hyps            = df_hyps
  )
}

# Prepare per‐patient chart data
prepare_patient_chart_data <- function(pt, seizures_summary_combined, df_duration, df_type, timeline_data, df_dev, df_sz, demographics) {
  genetics         <- timeline_data$genetics
  overlap_patients <- timeline_data$overlap_patients
  df_med_apts      <- timeline_data$df_med_apts
  df_sz_apts       <- timeline_data$df_sz_apts
  df_diagnosis_apts<- timeline_data$df_diagnosis_apts
  hospitalizations <- timeline_data$hospitalizations
  adverse_effects  <- timeline_data$adverse_effects
  df_spasm_periods <- timeline_data$df_spasm_periods
  df_eeg           <- timeline_data$df_eeg
  df_hyps          <- timeline_data$df_hyps
  
  protein_mutation <- genetics %>% filter(patient_uuid == pt, gene == "SCN8A") %>% pull(variant_protein) %>% first()
  if (is.na(protein_mutation)) {
    protein_mutation <- overlap_patients %>% filter(`Patient ID` == pt) %>% pull(`p.`) %>% first()
  }
  protein_mutation <- ifelse(is.na(protein_mutation), "NA", protein_mutation)
  
  patient_reg_num <- overlap_patients %>% filter(`Patient ID` == pt) %>% pull(`Registry #`) %>% first()
  patient_reg_num <- ifelse(!is.na(patient_reg_num), paste0(" (Registry #", patient_reg_num, ")"), "")
  
  has_infantile_spasms <- df_sz %>% filter(patient_uuid == pt, type == "Infantile spasms") %>% nrow() > 0
  title_suffix <- if (has_infantile_spasms) " - IF" else ""
  
  timeline_title <- paste(pt, patient_reg_num, " (", protein_mutation, ")", title_suffix, sep = "")
  
  pt_data <- seizures_summary_combined %>% filter(patient_uuid == pt)
  
  pt_data_duration <- df_duration %>% filter(patient_uuid == pt) %>%
    mutate(
      start_med_age_months = start_med_age / 30,
      end_med_age_months   = end_med_age / 30,
      first_3_months_end   = pmin(start_med_age_months + 3, end_med_age_months),
      medication_base      = sub(" \\d+$", "", medication)
    )
  
  duration_order <- pt_data_duration %>% group_by(medication) %>% summarise(
    total_duration = sum(end_med_age - start_med_age, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(desc(total_duration))
  
  pt_data <- pt_data %>% left_join(duration_order, by = "medication") %>% 
    arrange(total_duration) %>% mutate(medication = factor(medication, levels = unique(medication)))
  
  pt_data_type <- df_type %>% filter(patient_uuid == pt) %>% mutate(age_months = age_days / 30)
  
  appointment_data <- bind_rows(
    df_med_apts %>% filter(patient_uuid == pt) %>% select(appointment_age_days = medication_age_days_firstDate),
    df_sz_apts %>% filter(patient_uuid == pt) %>% select(appointment_age_days = age_days),
    df_diagnosis_apts %>% filter(patient_uuid == pt) %>% select(appointment_age_days = clinical_diagnosis_age_days_firstDate)
  ) %>% distinct() %>% mutate(appointment_age_months = appointment_age_days / 30)
  
  pt_demographics <- demographics %>% filter(patient_uuid == pt) %>% mutate(most_recent_record_age_months = most_recent_records_age_days / 30)
  
  pt_data_adverse <- adverse_effects %>% filter(patient_uuid == pt) %>% mutate(age_months = adverse_effect_age_days_firstDate / 30)
  
  pt_data_status <- hospitalizations %>% filter(patient_uuid == pt, admission_diagnosis == "Status epilepticus") %>% mutate(age_months = admission_age_days_firstDate / 30)
  
  pt_dev_data <- df_dev %>% filter(patient_uuid == pt) %>% group_by(domain_milestone) %>% arrange(domain_age_days_firstDate) %>%
    mutate(
      age_months = domain_age_days_firstDate / 30,
      status_change = case_when(
        domain_status == "Unable" & lag(domain_status) == "Able" ~ "Loss",
        domain_status == "Able" & lag(domain_status) == "Unable" ~ "Gained",
        TRUE ~ NA_character_
      )
    ) %>% ungroup() %>% filter(!is.na(status_change)) %>%
    mutate(status_change = factor(status_change, levels = c("Gained", "Loss"))) %>%
    group_by(age_months, status_change, domain) %>%
    mutate(n_points = n(), y_offset = row_number() - (n_points + 1) / 2) %>% ungroup()
  
  pt_data_duration <- pt_data_duration %>% group_by(medication_base) %>% mutate(earliest_start = min(start_med_age_months)) %>% ungroup()
  
  med_order <- pt_data_duration %>% distinct(medication_base, earliest_start) %>% arrange(earliest_start) %>% pull(medication_base)
  
  pt_spasm_periods <- df_spasm_periods %>% filter(patient_uuid == pt)
  pt_eeg <- df_eeg %>% filter(patient_uuid == pt)
  pt_hyps <- df_hyps %>% filter(patient_uuid == pt)
  
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

# Clean onset data by reading onset ages and computing onset group
clean_onset_data <- function() {
  onset_data <- read_csv(PATH_ONSET_AGES, show_col_types = FALSE)
  
  onset_data <- onset_data %>%
    group_by(UUID) %>%
    summarise(age_onset = min(Age_onset, na.rm = TRUE), .groups = "drop") %>%
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
  
  seizure_data <- seizure_data %>% mutate(
    focal         = ifelse(seizure_history_type %in% initial_classifier$focal, 1, 0),
    bilateral_tc  = ifelse(seizure_history_type %in% initial_classifier$bilateral_tonic_clonic, 1, 0),
    absence       = ifelse(seizure_history_type %in% initial_classifier$absence, 1, 0),
    infantile     = ifelse(seizure_history_type %in% initial_classifier$infantile_spasms, 1, 0)
  )
  
  initial_seizures <- seizure_data %>% group_by(UUID) %>% summarise(
    focal = ifelse(sum(focal, na.rm = TRUE) > 0, 1, 0),
    bilateral_tc = ifelse(sum(bilateral_tc, na.rm = TRUE) > 0, 1, 0),
    absence = ifelse(sum(absence, na.rm = TRUE) > 0, 1, 0),
    infantile = ifelse(sum(infantile, na.rm = TRUE) > 0, 1, 0),
    .groups = "drop"
  )
  return(initial_seizures)
}

# Clean abnormal EEG data for abnormal reports
clean_abnormal_eeg <- function() {
  classifier <- read_classifier()
  eeg_data <- read_excel(PATH_CITIZEN_DATA, sheet = "diagnostic_procedures") %>%
    filter(str_detect(procedure, "EEG")) %>%
    select(patient_uuid, procedure_findings, procedure_age_days) %>%
    filter(procedure_age_days < AGE_CUTOFF_DAYS) %>%
    mutate(abnormal_eeg = ave(!(procedure_findings %in% classifier$eeg_normal),
                              patient_uuid,
                              FUN = function(x) as.integer(any(x)))) %>%
    rename(UUID = patient_uuid) %>%
    distinct(UUID, abnormal_eeg)
  return(eeg_data)
}

# Clean hospitalization data
clean_hospitalization_data <- function(select_cols = c(1:2)) {
  hosp <- read_hospitalizations()
  hosp <- subset(hosp, select = select_cols)
  
  hosp_classifier <- read_excel(PATH_HOSPITALIZATION_CLASSIFIER)
  hosp <- left_join(hosp, hosp_classifier, by = c("admission_diagnosis" = "Hospitalization Events"))
  hosp <- hosp %>% filter(`Admission Type` %in% c("Emergency", "Incidental"))
  
  return(hosp)
}

# Calculate hospitalization frequencies
calculate_hospitalization_frequencies <- function(hosp_data) {
  freq_df <- hosp_data %>% group_by(`Admission Type`, Subgroup, admission_diagnosis) %>%
    summarise(n = n(), .groups = "drop") %>% ungroup() %>% na.omit()
  
  seizure_freq <- freq_df %>% filter(Subgroup == "Seizure") %>%
    mutate(admission_diagnosis = if_else(admission_diagnosis == "Status epilepticus", "Status epilepticus", "Seizure"))
  
  other_freq <- freq_df %>% filter(Subgroup != "Seizure") %>% mutate(Facet = "Other")
  
  hosp_data_mod <- hosp_data
  hosp_data_mod$admission_diagnosis[hosp_data_mod$admission_diagnosis == "Aspiration pneumonia"] <- "Pneumonia"
  hosp_data_mod$admission_diagnosis[hosp_data_mod$admission_diagnosis == "Acute respiratory failure"] <- "Respiratory failure"
  
  specific_types <- c("Respiratory failure", "Pneumonia")
  
  specific_hosp <- hosp_data_mod %>% filter(`Admission Type` == "Emergency", admission_diagnosis %in% specific_types) %>%
    group_by(`Admission Type`, Subgroup, admission_diagnosis) %>%
    summarise(n_specific = n(), .groups = "drop") %>% ungroup() %>%
    arrange(`Admission Type`, Subgroup, admission_diagnosis) %>% group_by(`Admission Type`, Subgroup) %>%
    mutate(cumulative_n = cumsum(n_specific)) %>% ungroup() %>% mutate(label = paste(admission_diagnosis, n_specific, sep = ": "))
  
  return(list(
    other_freq = other_freq,
    seizure_freq = seizure_freq,
    specific_freq = specific_hosp
  ))
}

# Clean diagnoses data
clean_diagnoses_data <- function() {
  diagnoses <- read_excel(PATH_CITIZEN_DATA, sheet = "clinical_diagnosis_features")
  diagnoses <- subset(diagnoses, select = c(1:2, 9))
  
  PATH_DIAGNOSIS_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "Grouping diagnoses.xlsx")
  diagnosis_classifier <- read_excel(PATH_DIAGNOSIS_CLASSIFIER)
  diagnosis_classifier <- subset(diagnosis_classifier, select = -c(5))
  
  diagnoses <- left_join(diagnoses, diagnosis_classifier, by = c("diagnosis" = "Clinical Diagnoses"))
  diagnoses <- unique(na.omit(subset(diagnoses, select = c(1:2, 6))))
  diagnoses <- diagnoses %>% filter(!System %in% c("Endocrine", "Excretory", "Integumentary"))
  
  sys_pcts <- diagnoses %>% group_by(System) %>% summarise(
    sys_pct = n() / nrow(diagnoses) * 100,
    sys_n = n(),
    .groups = "drop"
  )
  
  diagnosis_pcts <- diagnoses %>% group_by(System, diagnosis) %>% summarise(diagnosis_n = n(), .groups = "drop")
  
  diagnosis_pcts <- left_join(diagnosis_pcts, sys_pcts, by = "System")
  diagnosis_pcts <- diagnosis_pcts %>% mutate(diagnosis_pct = (diagnosis_n / sys_n) * 100) %>% ungroup()
  
  diagnosis_pcts <- subset(diagnosis_pcts, select = c("System", "diagnosis", "diagnosis_pct"))
  
  return(list(
    diagnoses = diagnoses,
    sys_pcts = sys_pcts,
    diagnosis_pcts = diagnosis_pcts
  ))
}