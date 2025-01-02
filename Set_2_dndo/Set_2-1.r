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
selected_data <- set_2_dndo %>%
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