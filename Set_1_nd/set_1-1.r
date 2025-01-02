---
title: "OS_nd"
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
selected_data <- Run_1_10_no_magnet_neat_ %>%
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
# Iterate through each column
for (col in colnames(data)) {
  # Skip non-numeric columns (like "time", if present)
  if (is.numeric(data[[col]])) {
    # Find indices where the column is zero
    zero_indices <- which(data[[col]] == 0)
    if (length(zero_indices) > 1) {
      # Keep only the first zero before a non-zero value
      first_non_zero <- which(data[[col]] != 0)[1]
      if (!is.na(first_non_zero)) {
        # Indices of zeros to remove
        remove_zeros <- zero_indices[zero_indices > zero_indices[1] & zero_indices < first_non_zero]
        data[remove_zeros, col] <- NA
      }
    }
  }
}

# Remove rows with NA if necessary
data <- na.omit(data)
View(data)
```

```{r}
library(dplyr)
library(stringr)

# Calculate row-wise mean and standard deviation for Ch 1+2 (Angle_t) and Ch 3+4 (Angle_b)
data <- data %>%
  rowwise() %>%
  mutate(
    mean_Ch1_Ch2 = mean(c_across(starts_with("Angle, Ch 1+2")), na.rm = TRUE),  # Mean for Ch 1 + 2
    sd_Ch1_Ch2 = sd(c_across(starts_with("Angle, Ch 1+2")), na.rm = TRUE),      # SD for Ch 1 + 2
    mean_Ch3_Ch4 = mean(c_across(starts_with("Angle, Ch 3+4")), na.rm = TRUE),  # Mean for Ch 3 + 4
    sd_Ch3_Ch4 = sd(c_across(starts_with("Angle, Ch 3+4")), na.rm = TRUE)       # SD for Ch 3 + 4
  ) %>%
  ungroup()

View(data)
```

```{r}
# Function to find indices of positive maxima for "mean_Ch1_Ch2"
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

# Find local maxima in the "mean_Ch1_Ch2" column
positive_local_maxima_indices <- find_positive_local_maxima(data$mean_Ch1_Ch2)

# Extract the times and values of the maxima from the original data
ch1_maxima_times <- data$Time[positive_local_maxima_indices]
ch1_maxima_values <- data$mean_Ch1_Ch2[positive_local_maxima_indices]

# Calculate periods for the maxima (difference in times)
ch1_periods <- diff(ch1_maxima_times)

# Apply the threshold to the periods (remove periods smaller than the threshold)
threshold <- 0.1
valid_ch1_periods <- ch1_periods[ch1_periods >= threshold]

# Calculate angular frequency (2*pi / period)
angular_frequencies <- 2 * pi / valid_ch1_periods

# Calculate the average period, angular frequency, and standard deviation
avg_period <- mean(valid_ch1_periods, na.rm = TRUE)
avg_angular_frequency <- mean(angular_frequencies, na.rm = TRUE)
angular_frequency_sd <- sd(angular_frequencies, na.rm = TRUE)

# Store results in a data frame
angular_frequencies_df <- data.frame(
  Avg_Period = avg_period,
  Avg_Angular_Frequency = avg_angular_frequency,
  Angular_Frequency_SD = angular_frequency_sd
)

# View the results
print(angular_frequencies_df)
View(angular_frequencies_df)
```

```{r}
colnames(data) <- trimws(colnames(data))
library(ggplot2)

# Ensure 'time' is numeric
data$Time <- as.numeric(data$Time)

# Check for NA values in the time column
if (any(is.na(data$Time))) {
  warning("There are missing values in the 'time' column.")
}

# Plotting both means (Ch1+2 and Ch3+4) against time with error bars (standard deviation)
ggplot(data) +
  # Plot for Ch 1+2
  geom_line(aes(x = Time, y = mean_Ch1_Ch2, color = "Ch 1+2 Mean"), size = 1) +
  geom_errorbar(aes(x = Time, ymin = mean_Ch1_Ch2 - sd_Ch1_Ch2, ymax = mean_Ch1_Ch2 + sd_Ch1_Ch2, color = "Ch 1+2 Mean"), width = 0.1) +
  
  # Plot for Ch 3+4
  geom_line(aes(x = Time, y = mean_Ch3_Ch4, color = "Ch 3+4 Mean"), size = 1) +
  geom_errorbar(aes(x = Time, ymin = mean_Ch3_Ch4 - sd_Ch3_Ch4, ymax = mean_Ch3_Ch4 + sd_Ch3_Ch4, color = "Ch 3+4 Mean"), width = 0.1) +
  
  # Customize the plot
  labs(title = "Means vs. Time with Standard Deviation as Error Bars",
       x = "Time",
       y = "Mean (rad)",
       color = "Channels") +
  theme_minimal() +
  theme(legend.position = "top")

# Save the plot as a PNG file
ggsave("C:/Users/smvan/Downloads/plot_2.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
```

```{r}
# Save the 'data' table as a CSV file in the current working directory
write.csv(data, "C:/Users/smvan/Downloads/data_s2_osc.csv", row.names = FALSE)
```