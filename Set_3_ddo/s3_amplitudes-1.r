---
title: "Trials"
author: "Sven v/d Bergh"
date: "2024-12-22"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(dplyr)

# Filter the data to include only rows with time <= 25 seconds
filtered_for_amplitude <- combined_data %>% filter(time <= 25)

# Initialize a list to store global maximum amplitudes and corresponding SDs
global_amplitudes <- list()

# Loop through trials to find the global maximum amplitude and corresponding SD for each trial
for (trial_number in 1:10) {
  ch1_col <- paste0("mean_Ch1_Ch2_Trial_", trial_number)
  sd_col <- paste0("sd_Ch1_Ch2_Trial_", trial_number)
  
  # Check if the columns exist in the data
  if (ch1_col %in% colnames(filtered_for_amplitude) && sd_col %in% colnames(filtered_for_amplitude)) {
    # Find the row with the global maximum amplitude
    max_row_index <- which.max(filtered_for_amplitude[[ch1_col]])
    
    # Extract the maximum amplitude and the corresponding SD
    max_amplitude <- filtered_for_amplitude[[ch1_col]][max_row_index]
    corresponding_sd <- filtered_for_amplitude[[sd_col]][max_row_index]
    
    # Store the results in a data frame
    global_amplitudes[[trial_number]] <- data.frame(
      Trial = trial_number,
      Max_Amplitude = max_amplitude,
      Corresponding_SD = corresponding_sd
    )
  }
}

# Combine the results into a single data frame
global_amplitudes_df <- do.call(rbind, global_amplitudes)

# View the results
print(global_amplitudes_df)
View(global_amplitudes_df)
```

```{r} 
#To find the phase shift, the sds for angle are not necasserily relevant; we therefore first omit them:
library(dplyr)

# 1. Select the "Time" column and all columns containing "mean"
filtered_data <- combined_data %>%
  select(time, contains("mean"))

# 2. View the filtered data
print(filtered_data)
View(filtered_data)
```

```{r}
#Next, we need to find the local maxima as the system is not always stable, we need to make sure these maxima are positive values
library(dplyr)

# Function to find indices of positive maxima (traditional and flat-topped)
find_positive_local_maxima <- function(column) {
  n <- length(column)
  maxima_indices <- integer(0) # Empty vector to store indices

  for (i in 2:(n - 1)) {
    # Handle missing values: skip comparison if any neighbor is NA
    if (is.na(column[i - 1]) || is.na(column[i]) || is.na(column[i + 1])) {
      next
    }
    
    # Check traditional local maxima
    if (column[i - 1] < column[i] && column[i] > column[i + 1]) {
      maxima_indices <- c(maxima_indices, i)
    }
    # Check flat-topped maxima (2 points)
    else if (i < n - 1 && 
             !is.na(column[i + 2]) && 
             column[i - 1] < column[i] && column[i] == column[i + 1] && column[i] > column[i + 2]) {
      maxima_indices <- c(maxima_indices, i, i + 1)
    }
    # Check flat-topped maxima (3 points)
    else if (i < n - 2 && 
             !is.na(column[i + 2]) && !is.na(column[i + 3]) && 
             column[i - 1] < column[i] && column[i] == column[i + 1] && column[i] == column[i + 2] && column[i] > column[i + 3]) {
      maxima_indices <- c(maxima_indices, i, i + 1, i + 2)
    }
  }

  # Return unique indices where the values are positive
  maxima_indices <- unique(maxima_indices)
  maxima_indices[column[maxima_indices] > 0]
}

# Apply the function to each numeric column except "time" and find the local maxima
positive_local_maxima <- filtered_data %>%
  mutate(across(-time, ~ {
    indices <- find_positive_local_maxima(.)
    # Create a vector with NAs
    result_vector <- rep(NA, length(.))
    # Place the values of the maxima into their corresponding positions
    result_vector[indices] <- .[indices]
    result_vector
  }, .names = "{.col}_modified")) %>%
  # Keep only rows where at least one column (except time) has a non-NA value
  filter(if_any(-time, ~ !is.na(.))) %>%
  # Select only the relevant columns (time and the modified columns)
  select(time, ends_with("_modified"))

# View the filtered results with relevant maxima
print(positive_local_maxima)
View(positive_local_maxima)
```

```{r}
#filter all data for time>25.00

positive_local_maxima <- filtered_data %>%
  mutate(across(-time, ~ {
    indices <- find_positive_local_maxima(.)
    # Create a vector with NAs
    result_vector <- rep(NA, length(.))
    # Place the values of the maxima into their corresponding positions
    result_vector[indices] <- .[indices]
    result_vector
  }, .names = "{.col}_modified")) %>%
  # Keep only rows where at least one column (except time) has a non-NA value
  filter(if_any(-time, ~ !is.na(.))) %>%
  # Select only the relevant columns (time and the modified columns)
  select(time, ends_with("_modified"))

# Now, filter out all rows where `time` > 25.00
filtered_by_time <- positive_local_maxima %>%
  filter(time <= 25.00)

# View the resulting dataframe
print(filtered_by_time)
View(filtered_by_time)
#  
# # Save the data frame for each trial as a CSV file
#   write.csv(filtered_by_time,
#             paste0("C:/Users/smvan/Downloads/filtered_by_time.csv"),
#             row.names = FALSE)
```

```{r}
#compute the average phase shift and corresponding SD

compute_phase_shift <- function(extrema_data, trial_number) {
  # Construct column names for Ch1_Ch2 and Ch3_Ch4
  ch1_col <- paste0("mean_Ch1_Ch2_Trial_", trial_number, "_modified")
  ch3_col <- paste0("mean_Ch3_Ch4_Trial_", trial_number, "_modified")
  
  # If it's trial 10, restrict time evaluation to 10 seconds
  if (trial_number == 10) {
    extrema_data <- extrema_data[extrema_data$time <= 10, ]
  }
  
  # Extract times of local maxima for Ch1_Ch2 and Ch3_Ch4
  ch1_times <- extrema_data$time[!is.na(extrema_data[[ch1_col]])]
  ch3_times <- extrema_data$time[!is.na(extrema_data[[ch3_col]])]
  
  # Check if local maxima exist for both channels
  if (length(ch1_times) == 0) {
    print(paste("No local maxima found in Ch1_Ch2 for trial", trial_number))
  }
  if (length(ch3_times) == 0) {
    print(paste("No local maxima found in Ch3_Ch4 for trial", trial_number))
  }
  
  # If either Ch1_Ch2 or Ch3_Ch4 has no local maxima, return NA
  if (length(ch1_times) == 0 | length(ch3_times) == 0) {
    return(data.frame(
      Trial = trial_number,
      Avg_Phase_Shift = NA,
      Phase_Shift_Error = NA
    ))
  }
  
  # Find the phase shift (time difference) between each local maxima in Ch1_Ch2 and closest in Ch3_Ch4
  phase_differences <- sapply(ch1_times, function(t1) {
    # Find the closest time in Ch3_Ch4
    closest_t3 <- ch3_times[which.min(abs(ch3_times - t1))]
    return(abs(t1 - closest_t3))  # Calculate the absolute time difference
  })
  
  # Compute the average phase shift and its standard deviation
  avg_phase_shift <- mean(phase_differences, na.rm = TRUE)
  phase_shift_error <- sd(phase_differences, na.rm = TRUE)
  
  # Return the results for this trial
  return(data.frame(
    Trial = trial_number,
    Avg_Phase_Shift = avg_phase_shift,
    Phase_Shift_Error = phase_shift_error
  ))
}

# Compute phase shifts for all trials, including trial 10 with the time restriction
phase_shifts <- do.call(rbind, lapply(1:10, function(i) {
  compute_phase_shift(filtered_by_time, i)
}))

# View the results
print(phase_shifts)
View(phase_shifts)
``` 

```{r}
#compute the angular driving frequency

# Set the threshold to 0.1s
threshold <- 0.8

# Initialize a list to store the results
angular_frequencies_modified <- list()

# Loop through trials
for (trial_number in 1:10) {
  # Get the times for Ch3_Ch4 for the current trial
  ch3_col <- paste0("mean_Ch3_Ch4_Trial_", trial_number, "_modified")
  ch3_times <- filtered_by_time$time[!is.na(filtered_by_time[[ch3_col]])]
  
  # Calculate periods for Ch3_Ch4 maxima
  ch3_periods <- diff(ch3_times)
  
  # Apply the threshold to the periods (remove periods smaller than the threshold)
  valid_ch3_periods <- ch3_periods[ch3_periods >= threshold]
  
  # Calculate angular frequency (2*pi / period)
  angular_frequencies <- 2 * pi / valid_ch3_periods
  
  # Calculate the average angular frequency and its SD for the trial
  avg_angular_frequency <- mean(angular_frequencies, na.rm = TRUE)
  angular_frequency_sd <- sd(angular_frequencies, na.rm = TRUE)
  
  # Store results for the trial
  angular_frequencies_modified[[trial_number]] <- data.frame(
    Trial = trial_number,
    Avg_Angular_Frequency = avg_angular_frequency,
    Angular_Frequency_SD = angular_frequency_sd
  )
}

# Combine the results into a data frame
angular_frequencies_df <- do.call(rbind, angular_frequencies_modified)

# View the results
print(angular_frequencies_df)
View(angular_frequencies_df)
```

```{r}
#combining the data frames

# Ensure 'Trial' column is consistent in all data frames
colnames(angular_frequencies_df)[colnames(angular_frequencies_df) == "trial_number"] <- "Trial"
colnames(phase_shifts)[colnames(phase_shifts) == "Trial"] <- "Trial"
colnames(global_amplitudes_df)[colnames(global_amplitudes_df) == "Trial"] <- "Trial"

# Merge the data frames step by step
combined_results <- merge(angular_frequencies_df, phase_shifts, by = "Trial", all = TRUE)
combined_results <- merge(combined_results, global_amplitudes_df, by = "Trial", all = TRUE)

# View the final combined results
print(combined_results)
View(combined_results)
```
```{r}
#add voltages for personal reference

# Define the voltages
voltages <- c(2.001, 2.996, 3.999, 4.995, 5.998, 6.992, 4.500, 4.747, 4.250, 4.507)

# Create a data frame for the voltages
voltage_df <- data.frame(
  Trial = 1:10,
  Voltage = voltages
)

# Merge the voltages into the combined_results data frame
combined_results <- merge(voltage_df, combined_results, by = "Trial", all = TRUE)

# Reorder columns to ensure Voltage appears after Trial
combined_results <- combined_results[, c("Trial", "Voltage", setdiff(names(combined_results), c("Trial", "Voltage")))]

# View the updated combined_results
print(combined_results)
View(combined_results)

 # Save the data frame for each trial as a CSV file
  write.csv(combined_results,
            paste0("C:/Users/smvan/Downloads/s3_graphing_data.csv"),
            row.names = FALSE)
```