---
title: "OS_dndo"
author: "Sven v/d Bergh"
date: "2024-12-16"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Set locale to use comma as decimal separator
Sys.setlocale("LC_NUMERIC", "Dutch")
library(tidyverse)

# Select columns that contain "Time" or "Angle"
selected_data <- set_3_ddo_1_ %>%
  select(matches("Time|Angle"))

# Check the structure of the selected data
str(selected_data)

# Print the filtered data to the console
print(selected_data)

# Assuming time columns have a common pattern in their names, like "Time"
long_data <- selected_data %>%
  gather(key = "Run", value = "Time", matches("Time")) %>%
  select(-Run)

# View the reshaped data
View(long_data)

# Print the reshaped data to the console
print(long_data)
```

```{r}
library(tidyverse)

# Assuming long_data is already created with the relevant columns

# Filter long_data to unique time values
long_data_summary <- long_data %>%
  distinct(Time, .keep_all = TRUE)

# View the summarized data frame
View(long_data_summary)

# Print the summarized data frame to the console
print(long_data_summary)

# Display the first few rows of the summarized data frame
head(long_data_summary)
```

```{r}
# Load the dplyr package
library(dplyr)

# Replace commas with periods and convert to numeric for all columns
long_data_summary <- long_data_summary %>%
  mutate(across(everything(), ~ as.numeric(gsub(",", ".", .))))

data <- long_data_summary

View(data)
print(data)
```

```{r}
# # Iterate through each column
# for (col in colnames(data)) {
#   # Skip non-numeric columns and explicitly exclude "time"
#   if (col != "time" && is.numeric(data[[col]])) {
#     # Find indices where the column is zero
#     zero_indices <- which(data[[col]] == 0)
#     if (length(zero_indices) > 1) {
#       # Keep only the first zero before a non-zero value
#       first_non_zero <- which(data[[col]] != 0)[1]
#       if (!is.na(first_non_zero)) {
#         # Indices of zeros to remove
#         remove_zeros <- zero_indices[zero_indices > zero_indices[1] & zero_indices < first_non_zero]
#         data[remove_zeros, col] <- NA
#       }
#     }
#   }
# }
# 
# # Remove rows with NA if necessary
# data <- na.omit(data)
# 
# View(data)
```

```{r}
library(dplyr)

# Assuming your data frame is named `data`
# Number of runs you want to process (e.g., 30 runs total)
num_runs <- 30

# Create an empty list to store the results
runs_list <- list()

# Loop through runs in sets of 3 (from 1-3, 4-6, 7-9, ..., 28-30)
for (n in 1:(num_runs / 3)) {
  # Calculate the run range for each trial
  run_start <- (n - 1) * 3 + 1
  run_end <- n * 3
  
  # Create a dynamic name for the data frame (Trial #n) stored as a key in the list
  trial_name <- paste0("Trial_", n)
  
  # Select the relevant columns for the current set of 3 runs, making sure to match exact run numbers
  runs_list[[trial_name]] <- data %>%
    select(Time, 
           matches(paste0("Run #", run_start, "$")),  # Match exact "Run #start"
           matches(paste0("Run #", run_start + 1, "$")),  # Match exact "Run #start+1"
           matches(paste0("Run #", run_start + 2, "$")))  # Match exact "Run #start+2"

  # Print the selected data frame for each trial
  cat(paste("Data for", trial_name, ":\n"))
  print(runs_list[[trial_name]])
  View(runs_list[[trial_name]])
}

#
#
#

# Loop through the runs_list data frames and add mean and sd columns for both Ch 1+2 and Ch 3+4
for (trial_name in names(runs_list)) {
  # Add rowwise mean and sd for Ch 1+2 and Ch 3+4 to each data frame in the list
  runs_list[[trial_name]] <- runs_list[[trial_name]] %>%
    rowwise() %>%
    mutate(
      mean_Ch1_Ch2 = mean(c_across(starts_with("Angle, Ch 1+2")), na.rm = TRUE),  # Mean for Ch 1 + 2
      sd_Ch1_Ch2 = sd(c_across(starts_with("Angle, Ch 1+2")), na.rm = TRUE),      # SD for Ch 1 + 2
      mean_Ch3_Ch4 = mean(c_across(starts_with("Angle, Ch 3+4")), na.rm = TRUE),  # Mean for Ch 3 + 4
      sd_Ch3_Ch4 = sd(c_across(starts_with("Angle, Ch 3+4")), na.rm = TRUE)       # SD for Ch 3 + 4
    ) %>%
    ungroup()  # Ungroup after rowwise calculation

  # Print the updated data frame for each trial
  cat(paste("Data with mean and SD for", trial_name, ":\n"))
  print(runs_list[[trial_name]])
  View(runs_list[[trial_name]])
}

# Loop through each trial in the runs_list to save the data frames as CSV
for (trial_name in names(runs_list)) {
  # Save the data frame for each trial as a CSV file
  write.csv(runs_list[[trial_name]], 
            paste0("C:/Users/smvan/Downloads/", trial_name, "_data.csv"), 
            row.names = FALSE)
  
  # Print message after saving each data frame
  cat(paste("Saved", trial_name, "data frame to CSV.\n"))
}
# 
#
# 

library(ggplot2)
library(tidyr)
library(dplyr)

# Loop through each trial data frame in runs_list and plot the data
for (trial_name in names(runs_list)) {
  # Extract the current data frame for the trial
  trial_data <- runs_list[[trial_name]]

  # Convert the data to a long format for plotting
  trial_data_long <- trial_data %>%
    gather(key = "Channel", value = "Mean", mean_Ch1_Ch2, mean_Ch3_Ch4) %>%
    gather(key = "SD_Channel", value = "SD", sd_Ch1_Ch2, sd_Ch3_Ch4) %>%
    mutate(Channel = recode(Channel, "mean_Ch1_Ch2" = "Ch 1+2", "mean_Ch3_Ch4" = "Ch 3+4"),
           SD_Channel = recode(SD_Channel, "sd_Ch1_Ch2" = "Ch 1+2", "sd_Ch3_Ch4" = "Ch 3+4"))
  
  # Plot the means for Ch 1+2 and Ch 3+4 together with their SD as error bars
  p <- ggplot(trial_data_long, aes(x = Time, color = Channel)) +
    # Plot "Ch 3+4" first
    geom_line(data = subset(trial_data_long, Channel == "Ch 3+4"), aes(y = Mean), size = 1) +
    geom_ribbon(data = subset(trial_data_long, Channel == "Ch 3+4"), aes(ymin = Mean - SD, ymax = Mean + SD), 
                fill = "darkcyan", alpha = 0.2) +  # SD for Ch 3+4
    # Plot "Ch 1+2" on top
    geom_line(data = subset(trial_data_long, Channel == "Ch 1+2"), aes(y = Mean), size = 1) +
    geom_ribbon(data = subset(trial_data_long, Channel == "Ch 1+2"), aes(ymin = Mean - SD, ymax = Mean + SD), 
                fill = "salmon", alpha = 0.2) +  # SD for Ch 1+2
    labs(title = paste(trial_name, "- Ch 1+2 and Ch 3+4 Means with SD"), 
         x = "Time", 
         y = "Angle (rad)") +
    theme_minimal() +
    scale_color_manual(values = c("Ch 1+2" = "salmon", "Ch 3+4" = "darkcyan")) # Custom color for channels
  
  # Print the plot for the trial
  print(p)
  
  # Save the plot as a PNG file
  ggsave(paste0("C:/Users/smvan/Downloads/", trial_name, "_plot.png"), 
         plot = p, width = 8, height = 6, dpi = 300)
  
  # Print message after saving each plot
  cat(paste("Saved", trial_name, "plot to PNG.\n"))
}
```