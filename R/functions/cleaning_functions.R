# R/functions/cleaning_functions.R
# Functions for common data cleaning tasks

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
  if (grepl(">", x)) return(as.numeric(sub(">", "", x)) + 1)
  return(as.numeric(x))
}
convert_less_than <- function(x) {
  if (grepl("<", x)) return(as.numeric(sub("<", "", x)) - 1)
  return(as.numeric(x))
}
convert_greater_than_equal_to <- function(x) {
  if (grepl("≥", x)) return(as.numeric(sub("≥", "", x)))
  return(as.numeric(x))
}
convert_less_than_equal_to <- function(x) {
  if (grepl("≤", x)) return(as.numeric(sub("≤", "", x)))
  return(as.numeric(x))
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