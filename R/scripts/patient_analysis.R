# R/scripts/patient_analysis.R
# Generate individual patient charts

# Import Libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(patchwork)

# Source Files
source(file.path(".", "R", "functions", "config.R"))
source(file.path(".", "R", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "functions", "analysis_functions.R"))
source(file.path(".", "R", "functions", "plotting_functions.R"))

# Data Cleaning and Setup
df_duration <- clean_medication_data()
df_type <- suppressWarnings(clean_seizure_data())
appointment_summary <- compute_appointment_summary()

# Seizure Index Comparisons
seizures_summary_combined <- calculate_seizure_index_comparisons(
  df_type = df_type,
  df_duration = df_duration,
  appointment_summary = appointment_summary
)

# Import Data and Classifier
df_sz <- read_excel(PATH_CITIZEN_DATA, sheet = "seizure_history")
names(df_sz) <- sub('^seizure_history_', '', names(df_sz))
classifier <- read_excel(PATH_CLASSIFIER)
timeline_info <- suppressWarnings(timeline_data(df_sz, classifier))
df_dev <- read_excel(PATH_CITIZEN_DATA, sheet = "development")
demographics <- read_excel(PATH_CITIZEN_DATA, sheet = "demographics")

# Order Patient List
patient_list <- unique(seizures_summary_combined$patient_uuid) %>%
  as_tibble() %>%
  rename(patient_uuid = value) %>%
  left_join(timeline_info$overlap_patients %>% select(`Patient ID`, `Registry #`), 
            by = c("patient_uuid" = "Patient ID")) %>%
  left_join(timeline_info$genetics %>% filter(gene == "SCN8A") %>% select(patient_uuid, variant_protein), 
            by = "patient_uuid") %>%
  left_join(timeline_info$overlap_patients %>% select(`Patient ID`, `p.`), 
            by = c("patient_uuid" = "Patient ID")) %>%
  mutate(
    protein_mutation = coalesce(variant_protein, `p.`),
    registry_status = if_else(!is.na(`Registry #`), 1, 0),
    mutation_number = as.numeric(str_extract(protein_mutation, "\\d+"))
  ) %>%
  arrange(desc(registry_status), mutation_number) %>%
  pull(patient_uuid)

# Generate Patient Charts (Timelines, Heatmaps, & Line Plots)
pdf_file <- "./output/figures/combined_patients_report.pdf"
pdf(pdf_file, width = 16, height = 12)

# Loop through each patient and generate their chart
for (pt in unique(patient_list)) {
  patient_data <- prepare_patient_chart_data(
    pt = pt,
    seizures_summary_combined = seizures_summary_combined,
    df_duration = df_duration,
    df_type = df_type,
    timeline_data = timeline_info,
    df_dev = df_dev,
    df_sz = df_sz,
    demographics = demographics
  )
  combined_plot <- plot_patient_chart(patient_data)
  suppressWarnings(print(combined_plot))
}

dev.off()