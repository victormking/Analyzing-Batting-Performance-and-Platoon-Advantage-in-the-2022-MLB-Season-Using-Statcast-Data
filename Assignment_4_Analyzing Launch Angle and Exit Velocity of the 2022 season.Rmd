---
title: "assignment_4"
output: html_document
date: "2024-09-17"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}

# Load necessary libraries
library(baseballr)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Set the season year and initialize an empty tibble for the data
season <- 2022
statcast_data_mar_apr <- tibble()

# Define date range from March to April 2022
days_mar_apr <- seq(from = as.Date(paste0(season, "-03-01")), to = as.Date(paste0(season, "-04-30")), by = '2 days')

# Loop through each date range and scrape Statcast data
for (i in seq_along(days_mar_apr)[-length(days_mar_apr)]) {
  start_date <- days_mar_apr[i]
  end_date <- days_mar_apr[i + 1]
  print(paste("Scraping data from", start_date, "to", end_date))
  
  # Scrape data for the current period
  current_data <- tryCatch({
    scrape_statcast_savant(start_date = start_date, end_date = end_date, player_type = "batter")
  }, error = function(e) {
    message("Error occurred: ", e)
    return(NULL)
  })
  
  # Filter out invalid data and perform necessary type conversions
  if (!is.null(current_data) && nrow(current_data) > 0) {
    current_data <- current_data %>%
      mutate(
        outs_when_up = as.numeric(outs_when_up),
        game_type = ifelse(is.na(game_type), "", game_type),
        inning = as.integer(inning),
        pitch_number = as.integer(pitch_number),
        break_angle_deprecated = as.logical(break_angle_deprecated),
        break_length_deprecated = as.logical(break_length_deprecated),
        game_year = as.integer(game_year)
      )
    
    # Bind the current chunk of data to the overall dataset
    statcast_data_mar_apr <- bind_rows(statcast_data_mar_apr, current_data)
  }
}

# Create "data" directory if it doesn't exist(I was getting some errors here and so I did it this way to be safe)
output_directory <- "data"
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Save the combined data from March 1, 2022 to April 30, 2022, to a single CSV file
output_filename_mar_apr <- file.path(output_directory, "statcast_2022-03-01_to_2022-04-30.csv")
write_csv(statcast_data_mar_apr, output_filename_mar_apr)
print(paste("Combined data saved to", output_filename_mar_apr))


```

```{r}

# Set the season year and initialize an empty tibble for the data
season <- 2022
statcast_data_may_jun <- tibble()

# Define date range from May to June 2022
days_may_jun <- seq(from = as.Date(paste0(season, "-05-01")), to = as.Date(paste0(season, "-06-30")), by = '2 days')

# Loop through each date range and scrape Statcast data
for (i in seq_along(days_may_jun)[-length(days_may_jun)]) {
  start_date <- days_may_jun[i]
  end_date <- days_may_jun[i + 1]
  print(paste("Scraping data from", start_date, "to", end_date))
  
  # Scrape data for the current period
  current_data <- tryCatch({
    scrape_statcast_savant(start_date = start_date, end_date = end_date, player_type = "batter")
  }, error = function(e) {
    message("Error occurred: ", e)
    return(NULL)
  })
  
  # Filter out invalid data and perform necessary type conversions
  if (!is.null(current_data) && nrow(current_data) > 0) {
    current_data <- current_data %>%
      mutate(
        outs_when_up = as.numeric(outs_when_up),
        game_type = ifelse(is.na(game_type), "", game_type),
        inning = as.integer(inning),
        pitch_number = as.integer(pitch_number),
        break_angle_deprecated = as.logical(break_angle_deprecated),
        break_length_deprecated = as.logical(break_length_deprecated),
        game_year = as.integer(game_year)
      )
    
    # Bind the current chunk of data to the overall dataset
    statcast_data_may_jun <- bind_rows(statcast_data_may_jun, current_data)
  }
}


output_directory <- "data"
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Save the combined data from May 1, 2022 to June 30, 2022, to a single CSV file
output_filename_may_jun <- file.path(output_directory, "statcast_2022-05-01_to_2022-06-30.csv")
write_csv(statcast_data_may_jun, output_filename_may_jun)
print(paste("Combined data saved to", output_filename_may_jun))



```


```{r}

# Read the two CSV files
data_mar_apr <- read_csv("data/statcast_2022-03-01_to_2022-04-30.csv")
data_may_jun <- read_csv("data/statcast_2022-05-01_to_2022-06-30.csv")

# Merge the two datasets
merged_data <- bind_rows(data_mar_apr, data_may_jun)

# Check for NA values in the dataset
summary(merged_data)

# Save the merged data to a new CSV file
output_filename_merged <- "data/statcast_2022-03-01_to_2022-06-30.csv"
write_csv(merged_data, output_filename_merged)
print(paste("Merged data saved to", output_filename_merged))

```

```{r}

# Set the season year and initialize an empty tibble for the data
season <- 2022
statcast_data_jul_aug <- tibble()

# Define date range from July to August 2022
days_jul_aug <- seq(from = as.Date(paste0(season, "-07-01")), to = as.Date(paste0(season, "-08-31")), by = '2 days')

# Loop through each date range and scrape Statcast data
for (i in seq_along(days_jul_aug)[-length(days_jul_aug)]) {
  start_date <- days_jul_aug[i]
  end_date <- days_jul_aug[i + 1]
  print(paste("Scraping data from", start_date, "to", end_date))
  
  # Scrape data for the current period
  current_data <- tryCatch({
    scrape_statcast_savant(start_date = start_date, end_date = end_date, player_type = "batter")
  }, error = function(e) {
    message("Error occurred: ", e)
    return(NULL)
  })
  
  # Filter out invalid data and perform necessary type conversions
  if (!is.null(current_data) && nrow(current_data) > 0) {
    current_data <- current_data %>%
      mutate(
        outs_when_up = as.numeric(outs_when_up),
        game_type = ifelse(is.na(game_type), "", game_type),
        inning = as.integer(inning),
        pitch_number = as.integer(pitch_number),
        break_angle_deprecated = as.logical(break_angle_deprecated),
        break_length_deprecated = as.logical(break_length_deprecated),
        game_year = as.integer(game_year)
      )
    
    # Bind the current chunk of data to the overall dataset
    statcast_data_jul_aug <- bind_rows(statcast_data_jul_aug, current_data)
  }
}

output_directory <- "data"
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Save the combined data from July 1, 2022 to August 31, 2022, to a single CSV file
output_filename_jul_aug <- file.path(output_directory, "statcast_2022-07-01_to_2022-08-31.csv")
write_csv(statcast_data_jul_aug, output_filename_jul_aug)
print(paste("Combined data saved to", output_filename_jul_aug))

```



```{r}

# Read the already merged CSV file (March to June)
data_mar_jun <- read_csv("data/Merged/statcast_2022-03-01_to_2022-06-30.csv")

# Read the new CSV file (July to August)
data_jul_aug <- read_csv("data/statcast_2022-07-01_to_2022-08-31.csv")

# Merge the two datasets
merged_data <- bind_rows(data_mar_jun, data_jul_aug)

# Check for NA values
summary(merged_data)

# Save the merged data to a new CSV file
output_filename_merged <- "data/statcast_2022-03-01_to_2022-08-31.csv"
write_csv(merged_data, output_filename_merged)
print(paste("Merged data saved to", output_filename_merged))


```


```{r}


# Set the season year and initialize an empty tibble for the data
season <- 2022
statcast_data_sep_oct <- tibble()

# Define date range from September to October 2022
days_sep_oct <- seq(from = as.Date(paste0(season, "-09-01")), to = as.Date(paste0(season, "-10-31")), by = '2 days')

# Loop through each date range and scrape Statcast data
for (i in seq_along(days_sep_oct)[-length(days_sep_oct)]) {
  start_date <- days_sep_oct[i]
  end_date <- days_sep_oct[i + 1]
  print(paste("Scraping data from", start_date, "to", end_date))
  
  # Scrape data for the current period
  current_data <- tryCatch({
    scrape_statcast_savant(start_date = start_date, end_date = end_date, player_type = "batter")
  }, error = function(e) {
    message("Error occurred: ", e)
    return(NULL)
  })
  
  # Filter out invalid data and perform necessary type conversions
  if (!is.null(current_data) && nrow(current_data) > 0) {
    current_data <- current_data %>%
      mutate(
        outs_when_up = as.numeric(outs_when_up),
        game_type = ifelse(is.na(game_type), "", game_type),
        inning = as.integer(inning),
        pitch_number = as.integer(pitch_number),
        break_angle_deprecated = as.logical(break_angle_deprecated),
        break_length_deprecated = as.logical(break_length_deprecated),
        game_year = as.integer(game_year)
      )
    
    # Bind the current chunk of data to the overall dataset
    statcast_data_sep_oct <- bind_rows(statcast_data_sep_oct, current_data)
  }
}

output_directory <- "data"
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Save the combined data from September 1, 2022 to October 31, 2022, to a single CSV file
output_filename_sep_oct <- file.path(output_directory, "statcast_2022-09-01_to_2022-10-31.csv")
write_csv(statcast_data_sep_oct, output_filename_sep_oct)
print(paste("Combined data saved to", output_filename_sep_oct))


```

```{r}

# Read the already merged CSV file (March to August)
data_mar_aug <- read_csv("data/Merged/statcast_2022-03-01_to_2022-08-31.csv")

# Read the new CSV file (September to October)
data_sep_oct <- read_csv("data/statcast_2022-09-01_to_2022-10-31.csv")

# Merge the two datasets
merged_data_complete <- bind_rows(data_mar_aug, data_sep_oct)

# Check for NA values
summary(merged_data_complete)

# Save the merged data to a new CSV file
output_filename_complete <- "data/statcast_2022-03-01_to_2022-10-31.csv"
write_csv(merged_data_complete, output_filename_complete)
print(paste("Merged data saved to", output_filename_complete))


```

```{r}

# Set the season year and initialize an empty tibble for the data
season <- 2022
statcast_data_sep_nov <- tibble()

# Define date range from Nov 1 to November 07, 2022
days_sep_nov <- seq(from = as.Date(paste0(season, "-11-01")), to = as.Date(paste0(season, "-11-07")), by = '2 days')

# Loop through each date range and scrape Statcast data
for (i in seq_along(days_sep_nov)[-length(days_sep_nov)]) {
  start_date <- days_sep_nov[i]
  end_date <- days_sep_nov[i + 1]
  print(paste("Scraping data from", start_date, "to", end_date))
  
  # Scrape data for the current period
  current_data <- tryCatch({
    scrape_statcast_savant(start_date = start_date, end_date = end_date, player_type = "batter")
  }, error = function(e) {
    message("Error occurred: ", e)
    return(NULL)
  })
  
  # Filter out invalid data and perform necessary type conversions
  if (!is.null(current_data) && nrow(current_data) > 0) {
    current_data <- current_data %>%
      mutate(
        outs_when_up = as.numeric(outs_when_up),
        game_type = ifelse(is.na(game_type), "", game_type),
        inning = as.integer(inning),
        pitch_number = as.integer(pitch_number),
        break_angle_deprecated = as.logical(break_angle_deprecated),
        break_length_deprecated = as.logical(break_length_deprecated),
        game_year = as.integer(game_year)
      )
    
    # Bind the current chunk of data to the overall dataset
    statcast_data_sep_nov <- bind_rows(statcast_data_sep_nov, current_data)
  }
}

output_directory <- "data"
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Save the combined data from September 1, 2022 to November 15, 2022, to a single CSV file
output_filename_sep_nov <- file.path(output_directory, "statcast_2022-11-01_to_2022-11-07.csv")
write_csv(statcast_data_sep_nov, output_filename_sep_nov)
print(paste("Combined data saved to", output_filename_sep_nov))


```


```{r}

# Read the already merged CSV file (March to August)
data_mar_aug <- read_csv("data/statcast_2022-03-01_to_2022-10-31.csv")

# Read the new CSV file (September to November)
data_sep_nov <- read_csv("data/statcast_2022-11-01_to_2022-11-07.csv")

# Merge the two datasets
merged_data_complete <- bind_rows(data_mar_aug, data_sep_nov)

# Check for NA values 
summary(merged_data_complete)

# Save the merged data to a new CSV file
output_filename_complete <- "data/statcast_2022_complete.csv"
write_csv(merged_data_complete, output_filename_complete)
print(paste("Merged data saved to", output_filename_complete))



```

```{r}

# second Part of Question #1 

# This code reads in the Statcast dataset, filters it based on specific conditions, 
# checks the filtered data, and saves it to a new CSV file. 
# The conditions used in filtering are that it excludes spring training games 
# and only keeps rows where the pitch was hit into play. 
# Finally, it saves the result and confirms the action with a print statement.

# Read the complete 2022 Statcast data
statcast2022 <- read_csv("data/statcast_2022_complete.csv")

# Filter out spring training games and only include pitches hit into play
filtered_statcast2022 <- statcast2022 %>%
  filter(game_type != "S", type == "X")

# Check the filtered data
summary(filtered_statcast2022)

# Save the filtered data to a new CSV file
output_filename_filtered <- "data/statcast2022_filtered.csv"
write_csv(filtered_statcast2022, output_filename_filtered)
print(paste("Filtered data saved to", output_filename_filtered))


```



```{r}

# Read the filtered Statcast data for 2022
statcast2022_filtered <- read_csv("data/statcast2022_filtered.csv")

# Define guidelines for ground balls, line drives, and fly balls
guidelines <- tibble(
  launch_angle = c(10, 25, 50),
  launch_speed = 40,
  label = c("Ground balls", "Line drives", "Flyballs")
)


# Explanation: Batting Average (BA) represents the simple probability of a ball in play resulting in a hit, 
# without regard to the quality or run value of the hit. Higher BA values are typically linked to 
# well-hit balls (line drives) that are more likely to become hits, while ground balls and fly balls tend 
# to result in lower BA values.
# BA does not differentiate between hit types (e.g., singles, doubles, home runs), treating all hits equally.


# Create scatter plot of predicted Batting Average (BA)
ev_plot <- statcast2022_filtered %>%
  sample_n(nrow(.) / 2) %>%  # Subsample for performance
  ggplot(aes(x = launch_speed, y = launch_angle, color = estimated_ba_using_speedangle)) +
  geom_point(alpha = 0.05) +  # Adjust alpha for point transparency
  geom_hline(data = guidelines, aes(yintercept = launch_angle), 
             color = "black", linetype = 2) +  # Add guidelines for hit types
  geom_text(data = guidelines, 
            aes(label = label, y = launch_angle - 4),
            color = "black", hjust = "left") +  # Label guidelines
  scale_color_gradient2("BA", low = "blue", mid = "white", high = "red", midpoint = 0.5) +  # Color scale for BA
  scale_x_continuous("Exit velocity (mph)", limits = c(40, 120)) +  # X-axis settings
  scale_y_continuous("Launch angle (degrees)", breaks = seq(-75, 75, 25)) +  # Y-axis settings
  theme_dark()  # Dark theme for the plot

# Print BA plot
print(ev_plot)


```




```{r}


# Explanation: Weighted On-Base Average (wOBA) is a more advanced metric than BA, assigning 
# different weights to different outcomes (e.g., singles, doubles, home runs) based on their run value. 
# This means that balls hit harder and at optimal launch angles are more likely to result in higher wOBA values, 
# reflecting their potential to contribute more to scoring runs. Unlike BA, wOBA differentiates between hit quality, 
# making it a better predictor of offensive production.
# The color scale reflects the likelihood of higher wOBA for well-hit balls (line drives and fly balls), 
# while ground balls tend to have lower wOBA values.


# Create a scatter plot visualizing the relationship between launch angle, exit velocity, and wOBA
woba_plot <- statcast2022_filtered %>%
  sample_n(nrow(.) / 2) %>%
  ggplot(aes(x = launch_speed, y = launch_angle, color = estimated_woba_using_speedangle)) +
  geom_point(alpha = 0.05) +
  geom_hline(data = guidelines, aes(yintercept = launch_angle), color = "black", linetype = 2) +
  geom_text(data = guidelines, aes(label = label, y = launch_angle - 4), color = "black", hjust = "left") +
  scale_color_gradient2("wOBA", low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  scale_x_continuous("Exit velocity (mph)", limits = c(40, 120)) +
  scale_y_continuous("Launch angle (degrees)", breaks = seq(-75, 75, 25)) +
  theme_dark()

# Display wOBA plot
woba_plot


```





```{r}


# Explanation: In the following matrix of plots, we explore how the pitcher/hitter handedness combinations 
# (left or right) affect both BA and wOBA. This platoon advantage can significantly impact performance; 
# for example, a left-handed hitter might perform better against right-handed pitchers due to better 
# pitch visibility. The plots are faceted by pitcher (`p_throws`) and batter (`stand`) handedness, 
# allowing us to see how the exit velocity and launch angle interact across different handedness combinations.
# Higher BA or wOBA values for certain platoon combinations could suggest a performance advantage based on handedness.


# Create a matrix of plots using pitcher/hitter handedness for BA
ba_plot_matrix <- statcast2022_filtered %>%
  sample_n(nrow(.) / 2) %>%  # Subsample for performance
  ggplot(aes(x = launch_speed, y = launch_angle, color = estimated_ba_using_speedangle)) +
  geom_point(alpha = 0.05) +
  geom_hline(data = guidelines, aes(yintercept = launch_angle), color = "black", linetype = 2) +
  geom_text(data = guidelines, aes(label = label, y = launch_angle - 4), color = "black", hjust = "left") +
  scale_color_gradient2("BA", low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  scale_x_continuous("Exit velocity (mph)", limits = c(40, 120)) +
  scale_y_continuous("Launch angle (degrees)", breaks = seq(-75, 75, 25)) +
  theme_dark() +
  facet_grid(p_throws ~ stand)  # Facet by pitcher and batter handedness

# Display the BA plot matrix
print(ba_plot_matrix)



```





```{r}

# Explanation: In this matrix of plots, we explore how the pitcher/hitter handedness combinations 
# (left or right) affect wOBA. This platoon advantage can significantly influence offensive production; 
# for example, a left-handed hitter might have better success against a right-handed pitcher due to better 
# pitch visibility. The matrix is faceted by both pitcher handedness (`p_throws`) and batter handedness (`stand`), 
# which allows us to analyze how the interaction between exit velocity and launch angle varies across different 
# handedness combinations. Higher wOBA values for certain combinations can indicate a platoon advantage that 
# can inform strategic decisions for both teams.


# Create a matrix of plots using pitcher/hitter handedness for wOBA
woba_plot_matrix <- statcast2022_filtered %>%
  sample_n(nrow(.) / 2) %>%  # Subsample for performance, adjust if needed
  ggplot(aes(x = launch_speed, y = launch_angle, color = estimated_woba_using_speedangle)) +
  geom_point(alpha = 0.05) +
  geom_hline(data = guidelines, aes(yintercept = launch_angle), color = "black", linetype = 2) +
  geom_text(data = guidelines, aes(label = label, y = launch_angle - 4), color = "black", hjust = "left") +
  scale_color_gradient2("wOBA", low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  scale_x_continuous("Exit velocity (mph)", limits = c(40, 120)) +
  scale_y_continuous("Launch angle (degrees)", breaks = seq(-75, 75, 25)) +
  theme_dark() +
  facet_grid(p_throws ~ stand)  # Facet by pitcher and batter handedness

# Display the wOBA plot matrix
print(woba_plot_matrix)


```





```{r}



```







```{r}



```







```{r}



```








```{r}



```

