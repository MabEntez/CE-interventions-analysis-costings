rm(list = ls())
library(rstudioapi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
library(reshape2)


#### Data Prep ####
setwd(dirname(getActiveDocumentContext()$path))
inter1 <- read.csv("intervention_int.csv")
inter2 <- read.csv("intervention_int1.csv")
inter2$RunId <- inter2$RunId + max(inter1$RunId) + 1
inter3 <- read.csv("intervention_int2.csv")
inter3$RunId <- inter3$RunId + max(inter2$RunId) + 1
inter4 <- read.csv("intervention_int3.csv")
inter4$RunId <- inter4$RunId + max(inter3$RunId) + 1
inter5 <- read.csv("intervention_int4.csv")
inter5$RunId <- inter5$RunId + max(inter4$RunId) + 1

inter <- rbind(inter1, inter2, inter3, inter4, inter5)


inter <- inter[, -c(12,13)]
#keptid <- read.csv("KeptIDs.csv")
#keptid <- keptid[,2]
inter$Step <- as.integer(round((inter$Step + 10)/24))
spatial_dis <- read.csv("Spatial prev distribution.csv")
spatial_dis <- spatial_dis[,-1]
fitting <- inter[inter$Step >= 69 & inter$Step <= 79 & inter$sheep_vaccine_cov_alt == 0.8 & inter$dog_deworming_cov_alt == 0.65,]

fitting <- fitting %>%
  group_by(RunId) %>%
  summarise(sheep_prev = mean(sheep_prev, na.rm = TRUE), dog_prev = mean(dog_prev, na.rm = TRUE))


runID_per_farm <- list()

for (i in 1:nrow(spatial_dis)) {
  in_range <- (fitting$sheep_prev >= (spatial_dis$mean[i] - (spatial_dis$sd[i]))) & (fitting$sheep_prev <= (spatial_dis$mean[i] + (spatial_dis$sd[i])))
  runID_per_farm[[i]] <- fitting$RunId[in_range]
}


result_list <- lapply(seq_along(runID_per_farm), function(idx) {
  samples <- sample(runID_per_farm[[idx]], 100)
  incremented_samples <- unlist(sapply(samples, function(x) c(x, x + 1, x + 2, x + 3)))
  
  # Return list with index and the incremented samples
  list(index = idx, values = incremented_samples)
})

# Create a nested dataframe based on your lapply result
df <- data.frame(idx = seq_along(runID_per_farm))

# Expand your result_list to a dataframe
kept_ID <- df %>%
  rowwise() %>%
  mutate(samples = list(sample(runID_per_farm[[idx]], 100))) %>%
  unnest(samples) %>%
  mutate(values = map(samples, ~tibble(value = .x + 0:3, 
                                       order = 1:4))) %>%
  unnest(values) %>%
  select(-samples)  # remove the samples column

save(kept_ID, file = "KeptID.RData")
load(file = "KeptID.RData")
  
inter$groups <- with(inter, interaction(sheep_vaccine_cov_alt, dog_deworming_cov_alt))
inter_list <- split(inter, inter$groups)
new_names <- list("int1", "int2", "int3", "int4")
inter_list <- setNames(inter_list, new_names)
list2env(inter_list, envir = .GlobalEnv)

#### Quick plot ####
int1_sheep <- int1[, c(1,2,13)]
int1_sheep <- int1_sheep[int1_sheep$RunId %in% kept_ID$ value, ]
result <- merge(int1_sheep, kept_ID, by.x = "RunId", by.y = "value", all.y = TRUE)
int1_sheep <- result[, c(names(int1_sheep), "idx")]


int1_df <- int1_sheep[int1_sheep$idx == 25,] %>%
  group_by(Step) %>%
  summarise(
    base_mean = mean(sheep_prev, na.rm = TRUE),
    base_lower = quantile(sheep_prev, 0.05, na.rm = TRUE),
    base_upper = quantile(sheep_prev, 0.95, na.rm = TRUE)
  )

start_x <- 2013
end_x <- 2015

base <- ggplot(data = int1_df, aes(x = Step + 1940)) +
  geom_line(aes(y = base_mean, color = "Current intervention\nmedian prevalence"), size = 1) +
  geom_ribbon(aes(y = base_mean, ymin = base_lower, ymax = base_upper), fill = "black", alpha = 0.2) +
  coord_cartesian(xlim = c(1980, NA)) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(1980, max(int1_df$Step + 1940, na.rm = TRUE), by = 10)) +
  xlab("Year") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For Current Intervention Parameters") +
  theme_bw() +
  scale_color_manual(values = c("Current intervention\nmedian prevalence" = "black", name = "Legend")) +
  guides(fill = "none") +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "outside") +
  geom_segment(aes(x = 2014, xend = 2014, y = spatial_dis$mean[1] - 0.015, yend = spatial_dis$mean[1] + 0.015), color = "red", linetype = "solid", size = 1) + # Vertical red line for 2009
  geom_segment(aes(x = start_x, xend = end_x, y = spatial_dis$mean[1], yend = spatial_dis$mean[1]), color = "red", linetype = "solid", size = 1) + # Horizontal red line for mean
  
  # Ribbon for mean ? sd
  geom_ribbon(data = data.frame(x = c(2009, 2019), 
                                ymin = spatial_dis$mean[1] - spatial_dis$sd[1], 
                                ymax = spatial_dis$mean[1] + spatial_dis$sd[1]),
              aes(x = x, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.2)

print(base)


#### Intervention 1 ####
int1_sheep <- int1[, c(1,2,13)]
int1_sheep <- int1_sheep[int1_sheep$RunId %in% kept_ID$ value, ]
result <- merge(int1_sheep, kept_ID, by.x = "RunId", by.y = "value", all.y = TRUE)
int1_sheep <- result[, c(names(int1_sheep), "idx")]


int1_df <- int1_sheep %>%
  group_by(Step) %>%
  summarise(
    base_mean = mean(sheep_prev, na.rm = TRUE),
    base_lower = quantile(sheep_prev, 0.05, na.rm = TRUE),
    base_upper = quantile(sheep_prev, 0.95, na.rm = TRUE)
  )

#### Intervention 2 ####

int2_sheep <- int2[, c(1,2,13)]
int2_sheep <- int2_sheep[int2_sheep$RunId %in% kept_ID$ value, ]
result <- merge(int2_sheep, kept_ID, by.x = "RunId", by.y = "value", all.y = TRUE)
int2_sheep <- result[, c(names(int2_sheep), "idx")]

int2_df <- int2_sheep %>%
  group_by(Step) %>%
  summarise(
    mean = mean(sheep_prev, na.rm = TRUE),
    lower = quantile(sheep_prev, 0.05, na.rm = TRUE),
    upper = quantile(sheep_prev, 0.95, na.rm = TRUE)
  )

#### Intervention 3 ####

int3_sheep <- int3[, c(1,2,13)]
int3_sheep <- int3_sheep[int3_sheep$RunId %in% kept_ID$ value, ]
result <- merge(int3_sheep, kept_ID, by.x = "RunId", by.y = "value", all.y = TRUE)
int3_sheep <- result[, c(names(int3_sheep), "idx")]

int3_df <- int3_sheep %>%
  group_by(Step) %>%
  summarise(
    mean = mean(sheep_prev, na.rm = TRUE),
    lower = quantile(sheep_prev, 0.05, na.rm = TRUE),
    upper = quantile(sheep_prev, 0.95, na.rm = TRUE)
  )

#### Intervention 4 ####

int4_sheep <- int4[, c(1,2,13)]
int4_sheep <- int4_sheep[int4_sheep$RunId %in% kept_ID$ value, ]
result <- merge(int4_sheep, kept_ID, by.x = "RunId", by.y = "value", all.y = TRUE)
int4_sheep <- result[, c(names(int4_sheep), "idx")]

int4_df <- int4_sheep %>%
  group_by(Step) %>%
  summarise(
    mean = mean(sheep_prev, na.rm = TRUE),
    lower = quantile(sheep_prev, 0.05, na.rm = TRUE),
    upper = quantile(sheep_prev, 0.95, na.rm = TRUE)
  )

#### Basic Plot Sheep ####
library(patchwork)

#### ggplot Sheep ####
library(scales)
library(cowplot)
library(gridExtra)

int2_df = cbind(int1_df[,2:4], int2_df)
int3_df = cbind(int1_df[,2:4], int3_df)
int4_df = cbind(int1_df[,2:4], int4_df)



#### Plotting Postintervention change ####
vac <- ggplot(data = int2_df, aes(x = Step + 1940)) +
  geom_line(aes(y = base_mean, color = "Current intervention\nmedian prevalence"), size = 1) +
  geom_ribbon(aes(y = base_mean, ymin = base_lower, ymax = base_upper), fill = "black", alpha = 0.2) +
  geom_line(aes(y = mean, color = "Intensified intervention\nmedian prevalence"), size = 1) +
  geom_ribbon(aes(y = mean, ymin = lower, ymax = upper), fill = "#e40404", alpha = 0.2) +
  coord_cartesian(xlim = c(2020, NA), ylim = c(0, 0.4)) +  # Adjust y-axis here
  scale_y_continuous(labels = percent_format()) +
  xlab("Year") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For Increased\nVaccine Coverage") +
  theme_bw() +
  scale_color_manual(values = c("Current intervention\nmedian prevalence" = "black",
                                "Intensified intervention\nmedian prevalence" = "#e40404"),
                     name = "Legend") +
  guides(fill = "none")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.position = "none")



dworm <- ggplot(data = int3_df, aes(x = Step + 1940)) +
  geom_line(aes(y = base_mean, color = "Current intervention\nmedian prevalence"), size = 1) +
  geom_ribbon(aes(y = base_mean, ymin = base_lower, ymax = base_upper), fill = "black", alpha = 0.2) +
  geom_line(aes(y = mean, color = "Intensified intervention\nmedian prevalence"), size = 1) +
  geom_ribbon(aes(y = mean, ymin = lower, ymax = upper), fill = "#ee7b00", alpha = 0.2) +
  coord_cartesian(xlim = c(2020, NA), ylim = c(0, 0.4)) +
  scale_y_continuous(labels = percent_format()) +
  xlab("Year") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For Increased\nDeworming Coverage") +
  theme_bw() +
  scale_color_manual(values = c("Current intervention\nmedian prevalence" = "black",
                                "Intensified intervention\nmedian prevalence" = "#ee7b00"),
                     name = "Legend") +
  guides(fill = "none")+
  theme(legend.text = element_text(size = 12))+
  theme(legend.position = "none")



vacdworm <- ggplot(data = int4_df, aes(x = Step + 1940)) +
  geom_line(aes(y = base_mean, color = "Current intervention\nmedian prevalence"), size = 1) +
  geom_ribbon(aes(y = base_mean, ymin = base_lower, ymax = base_upper), fill = "black", alpha = 0.2) +
  geom_line(aes(y = mean, color = "Intensified intervention\nmedian prevalence"), size = 1) +
  geom_ribbon(aes(y = mean, ymin = lower, ymax = upper), fill = "#257600", alpha = 0.2) +
  coord_cartesian(xlim = c(2020, NA), ylim = c(0, 0.4)) +
  scale_y_continuous(labels = percent_format()) +
  xlab("Year") +
  ylab("CE prevalence in sheep") +
  ggtitle("Median Sheep Prevalence For Increased\nDeworming & Vaccination Coverage") +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Current intervention\nmedian prevalence" = "black",
      "Intensified intervention\nmedian prevalence" = "#257600"
    ),name = "Legend") +
  guides(fill = "none")+
  theme(legend.text = element_text(size = 12))

# Generate a plot that includes all the legend elements
legend_plot <- ggplot() +
  geom_point(aes(x = 1, y = 1, color = "Current intervention"), data = data.frame(x = 1, y = 1)) +
  geom_point(aes(x = 1, y = 1, color = "Intensified Vaccination"), data = data.frame(x = 1, y = 1)) +
  geom_point(aes(x = 1, y = 1, color = "Intensified Deworming"), data = data.frame(x = 1, y = 1)) +
  geom_point(aes(x = 1, y = 1, color = "Intensified Vaccination & Deworming"), data = data.frame(x = 1, y = 1)) +
  scale_color_manual(
    values = c(
      "Current intervention" = "black",
      "Intensified Vaccination" = "#e40404",
      "Intensified Deworming" = "#ee7b00",
      "Intensified Vaccination & Deworming" = "#257600"  
    ), 
    breaks = c("Current intervention",
               "Intensified Vaccination", 
               "Intensified Deworming", 
               "Intensified Vaccination & Deworming"),  # Add this line
    labels = c("Current intervention", 
               "Increased Vaccination\nCoverage", 
               "Increased Deworming\nCoverage", 
               "Increased Deworming &\nVaccination Coverage"),  
    name = "Legend"
  ) +
  theme_void() +
  theme(legend.direction = "horizontal", legend.position = "bottom")


# Extract the legend
legend_grob <- cowplot::get_legend(legend_plot)

# Combine plots horizontally without their individual legends
combined_plot <- plot_grid(
  vac + theme(legend.position = "none"), 
  dworm + theme(legend.position = "none"), 
  vacdworm + theme(legend.position = "none"), 
  ncol = 3, 
  align = "h"
)

# Combine the plots with the legend
final_plot <- grid.arrange(combined_plot, legend_grob, nrow = 2, heights = c(10, 1))

# Display the final plot
print(final_plot)

#### Elimination percentage graph ####

set.seed(123)  # For reproducibility

int1_sheep_random <- int1_sheep %>%
  select(idx, RunId) %>%
  distinct() %>%
  group_by(idx) %>%
  mutate(rep = row_number()) %>%
  ungroup() %>%
  arrange(rep, idx) %>%
  left_join(int1_sheep, by = c("idx", "RunId"))

int2_sheep_random <- int2_sheep %>%
  select(idx, RunId) %>%
  distinct() %>%
  group_by(idx) %>%
  mutate(rep = row_number()) %>%
  ungroup() %>%
  arrange(rep, idx) %>%
  left_join(int2_sheep, by = c("idx", "RunId"))

int3_sheep_random <- int3_sheep %>%
  select(idx, RunId) %>%
  distinct() %>%
  group_by(idx) %>%
  mutate(rep = row_number()) %>%
  ungroup() %>%
  arrange(rep, idx) %>%
  left_join(int3_sheep, by = c("idx", "RunId"))

int4_sheep_random <- int4_sheep %>%
  select(idx, RunId) %>%
  distinct() %>%
  group_by(idx) %>%
  mutate(rep = row_number()) %>%
  ungroup() %>%
  arrange(rep, idx) %>%
  left_join(int4_sheep, by = c("idx", "RunId"))


proportion_zero1 <- int1_sheep_random %>%
  group_by(rep, Step) %>%
  summarize(
    total = n(),
    zero_sheep_prev = sum(sheep_prev == 0),
    proportion_zero = zero_sheep_prev / total
  )

proportion_zero2 <- int2_sheep_random %>%
  group_by(rep, Step) %>%
  summarize(
    total = n(),
    zero_sheep_prev = sum(sheep_prev == 0),
    proportion_zero = zero_sheep_prev / total
  )

proportion_zero3 <- int3_sheep_random %>%
  group_by(rep, Step) %>%
  summarize(
    total = n(),
    zero_sheep_prev = sum(sheep_prev == 0),
    proportion_zero = zero_sheep_prev / total
  )

proportion_zero4 <- int4_sheep_random %>%
  group_by(rep, Step) %>%
  summarize(
    total = n(),
    zero_sheep_prev = sum(sheep_prev == 0),
    proportion_zero = zero_sheep_prev / total
  )

calculate_summary <- function(df) {
  df %>%
    group_by(Step) %>%
    summarize(
      mean_prop = mean(proportion_zero),
      lower_95 = quantile(proportion_zero, 0.025),
      upper_95 = quantile(proportion_zero, 0.975)
    )
}

proportion_zero1 <- proportion_zero1 %>% filter(!is.na(proportion_zero))
proportion_zero2 <- proportion_zero2 %>% filter(!is.na(proportion_zero))
proportion_zero3 <- proportion_zero3 %>% filter(!is.na(proportion_zero))
proportion_zero4 <- proportion_zero4 %>% filter(!is.na(proportion_zero))

summary1 <- calculate_summary(proportion_zero1)
summary2 <- calculate_summary(proportion_zero2)
summary3 <- calculate_summary(proportion_zero3)
summary4 <- calculate_summary(proportion_zero4)

library(ggplot2)
library(scales)

p1 <- ggplot() +
  geom_ribbon(data = summary1, aes(x = Step + 1940, ymin = lower_95, ymax = upper_95), fill = "#003f5c", alpha = 0.3) +
  geom_line(data = summary1, aes(x = Step + 1940, y = mean_prop, color = "Current intervention"), size = 1.3) +
  
  geom_ribbon(data = summary2, aes(x = Step + 1940, ymin = lower_95, ymax = upper_95), fill = "#7a5195", alpha = 0.3) +
  geom_line(data = summary2, aes(x = Step + 1940, y = mean_prop, color = "Intensified Vaccination"), size = 1.3) +
  
  geom_ribbon(data = summary3, aes(x = Step + 1940, ymin = lower_95, ymax = upper_95), fill = "#ef5675", alpha = 0.3) +
  geom_line(data = summary3, aes(x = Step + 1940, y = mean_prop, color = "Intensified Deworming"), size = 1.3) +
  
  geom_ribbon(data = summary4, aes(x = Step + 1940, ymin = lower_95, ymax = upper_95), fill = "#ffa600", alpha = 0.3) +
  geom_line(data = summary4, aes(x = Step + 1940, y = mean_prop, color = "Intensified Vaccination & Deworming"), size = 1.3) +
  
  coord_cartesian(xlim = c(2019, NA)) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(
    values = c(
      "Current intervention" = "#003f5c",
      "Intensified Vaccination" = "#7a5195",
      "Intensified Deworming" = "#ef5675",
      "Intensified Vaccination & Deworming" = "#ffa600"
    ),
    name = "Legend"
  ) +
  xlab("Year") +
  ylab("Percentage of simulations") +
  ggtitle("Percentage of simulations achieving 0% prevalence") +
  theme_bw()

print(p1)

#### New Elimination percentage graph ####




library(dplyr)
setwd(dirname(getActiveDocumentContext()$path))
inter1 <- read.csv("intervention_int.csv")
inter2 <- read.csv("intervention_int1.csv")
inter2$RunId <- inter2$RunId + max(inter1$RunId) + 1
inter3 <- read.csv("intervention_int2.csv")
inter3$RunId <- inter3$RunId + max(inter2$RunId) + 1
inter4 <- read.csv("intervention_int3.csv")
inter4$RunId <- inter4$RunId + max(inter3$RunId) + 1
inter5 <- read.csv("intervention_int4.csv")
inter5$RunId <- inter5$RunId + max(inter4$RunId) + 1

inter <- rbind(inter1, inter2, inter3, inter4, inter5)
inter$Step <- as.integer(round((inter$Step + 10)/24))

pop <- read.csv("Population.csv")
colnames(pop)[names(pop) == "index"] <- "idx"

inter$groups <- with(inter, interaction(sheep_vaccine_cov_alt, dog_deworming_cov_alt))
inter_list <- split(inter, inter$groups)
new_names <- list("int1", "int2", "int3", "int4")
inter_list <- setNames(inter_list, new_names)
list2env(inter_list, envir = .GlobalEnv)

process_dose <- function(df, kept_ID){
  dose_df <- df[, c(1,2,12,13)]
  dose_df <- dose_df[dose_df$RunId %in% kept_ID$value, ]
  result <- merge(dose_df, kept_ID, by.x = "RunId", by.y = "value", all.y = TRUE)
  return(result[, c(names(dose_df), "idx")])
}

# Processing each intervention dataset
int1_dose <- process_dose(int1, kept_ID)
int2_dose <- process_dose(int2, kept_ID)
int3_dose <- process_dose(int3, kept_ID)
int4_dose <- process_dose(int4, kept_ID)

calculate_dose_summary_adj <- function(dose_df, current_step) {
  
  # Multiply dosages with Scaling from pop based on idx
  dose_df <- merge(dose_df, pop, by = "idx", all.x = TRUE)
  dose_df$Vaccine.Dosage.Count <- dose_df$Vaccine.Dosage.Count * dose_df$Scaling
  dose_df$Deworm.Dosage.Count <- dose_df$Deworm.Dosage.Count 
  
  # Filter for current step and its preceding step
  dose_df <- dose_df[dose_df$Step %in% c(83, current_step), ]
  
  # Calculate mean and quantiles for each Step and idx
  summary_df <- dose_df %>%
    group_by(Step, idx) %>%
    summarise(
      Vaccine_Mean = mean(Vaccine.Dosage.Count, na.rm = TRUE),
      Vaccine_Upper = quantile(Vaccine.Dosage.Count, 0.975, na.rm = TRUE),
      Vaccine_Lower = quantile(Vaccine.Dosage.Count, 0.025, na.rm = TRUE),
      Deworm_Mean = mean(Deworm.Dosage.Count, na.rm = TRUE),
      Deworm_Upper = quantile(Deworm.Dosage.Count, 0.975, na.rm = TRUE),
      Deworm_Lower = quantile(Deworm.Dosage.Count, 0.025, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Pivot the summary data for difference calculation
  wide_df <- summary_df %>%
    pivot_wider(names_from = Step, values_from = c(Vaccine_Mean, Vaccine_Upper, Vaccine_Lower, Deworm_Mean, Deworm_Upper, Deworm_Lower))
  
  # Dynamic column names for Vaccine and Deworm calculations
  columns <- c("Vaccine_Mean", "Vaccine_Upper", "Vaccine_Lower", "Deworm_Mean", "Deworm_Upper", "Deworm_Lower")
  
  for (col in columns) {
    curr_col_name <- paste0(col, "_", current_step)
    prev_col_name <- paste0(col, "_83")
    diff_col_name <- paste0(col, "_Diff")
    
    if (curr_col_name %in% names(wide_df) && prev_col_name %in% names(wide_df)) {
      wide_df[[diff_col_name]] <- wide_df[[curr_col_name]] - wide_df[[prev_col_name]]
    }
  }
  
  # Calculate the total average of the differences
  total_diff <- wide_df %>%
    summarise(
      Sum_Vaccine_Mean_Diff = sum(Vaccine_Mean_Diff, na.rm = TRUE),
      Sum_Vaccine_Upper_Diff = sum(Vaccine_Upper_Diff, na.rm = TRUE),
      Sum_Vaccine_Lower_Diff = sum(Vaccine_Lower_Diff, na.rm = TRUE),
      Sum_Deworm_Mean_Diff = sum(Deworm_Mean_Diff, na.rm = TRUE),
      Sum_Deworm_Upper_Diff = sum(Deworm_Upper_Diff, na.rm = TRUE),
      Sum_Deworm_Lower_Diff = sum(Deworm_Lower_Diff, na.rm = TRUE)
    )
  
  return(list(Summary = summary_df, Differences = wide_df, TotalDifferences = total_diff))
}


steps <- c(90, 100, 110, 120, 130, 140)

results_list <- lapply(steps, function(step) {
  list(
    int1 = calculate_dose_summary_adj(int1_dose, step),
    int2 = calculate_dose_summary_adj(int2_dose, step),
    int3 = calculate_dose_summary_adj(int3_dose, step),
    int4 = calculate_dose_summary_adj(int4_dose, step)
  )
})

# Combine the averages 
all_combined_averages <- mapply(function(res, step) {
  # Extract TotalDifferences for each dataset
  int1_diff <- res$int1$TotalDifferences
  int2_diff <- res$int2$TotalDifferences
  int3_diff <- res$int3$TotalDifferences
  int4_diff <- res$int4$TotalDifferences
  
  # Calculate combined values
  int1_diff$Combined_Mean_Int1 <- (int1_diff[,1] * 0.12) + (int1_diff[,4] * 0.43)
  int1_diff$Combined_Upper_Int1 <- (int1_diff[,2] * 0.12) + (int1_diff[,5] * 0.43)
  int1_diff$Combined_Lower_Int1 <- (int1_diff[,3] * 0.12) + (int1_diff[,6] * 0.43)
  
  int2_diff$Combined_Mean_Int2 <- (int2_diff[,1] * 0.12) + (int2_diff[,4] * 0.43)
  int2_diff$Combined_Upper_Int2 <- (int2_diff[,2] * 0.12) + (int2_diff[,5] * 0.43)
  int2_diff$Combined_Lower_Int2 <- (int2_diff[,3] * 0.12) + (int2_diff[,6] * 0.43)
  
  int3_diff$Combined_Mean_Int3 <- (int3_diff[,1] * 0.12) + (int3_diff[,4] * 0.43)
  int3_diff$Combined_Upper_Int3 <- (int3_diff[,2] * 0.12) + (int3_diff[,5] * 0.43)
  int3_diff$Combined_Lower_Int3 <- (int3_diff[,3] * 0.12) + (int3_diff[,6] * 0.43)
  
  int4_diff$Combined_Mean_Int4 <- (int4_diff[,1] * 0.12) + (int4_diff[,4] * 0.43)
  int4_diff$Combined_Upper_Int4 <- (int4_diff[,2] * 0.12) + (int4_diff[,5] * 0.43)
  int4_diff$Combined_Lower_Int4 <- (int4_diff[,3] * 0.12) + (int4_diff[,6] * 0.43)
  
  int1_diff$Step <- step
  int2_diff$Step <- step
  int3_diff$Step <- step
  int4_diff$Step <- step
  
  # Join the datasets on 'Step' to form one combined dataset
  combined <- bind_rows(
    select(int1_diff, Step, starts_with("Combined_Mean_Int1"), starts_with("Combined_Upper_Int1"), starts_with("Combined_Lower_Int1")),
    select(int2_diff, Step, starts_with("Combined_Mean_Int2"), starts_with("Combined_Upper_Int2"), starts_with("Combined_Lower_Int2")),
    select(int3_diff, Step, starts_with("Combined_Mean_Int3"), starts_with("Combined_Upper_Int3"), starts_with("Combined_Lower_Int3")),
    select(int4_diff, Step, starts_with("Combined_Mean_Int4"), starts_with("Combined_Upper_Int4"), starts_with("Combined_Lower_Int4"))
  )
  
  return(combined)
}, results_list, steps, SIMPLIFY = FALSE)

# Convert the results list to a dataframe
all_combined_averages_df <- bind_rows(all_combined_averages)


consolidated_df <- all_combined_averages_df %>%
  group_by(Step) %>%
  summarise(across(starts_with("Combined"), ~na.omit(.x)[1]), .groups = "drop")

colnames(consolidated_df) <- c("Step", "cost_mean_int1", "cost_upper_int1", "cost_lower_int1",
                               "cost_mean_int2", "cost_upper_int2", "cost_lower_int2",
                               "cost_mean_int3", "cost_upper_int3", "cost_lower_int3",
                               "cost_mean_int4", "cost_upper_int4", "cost_lower_int4")
                               
final_df <- consolidated_df %>%
  left_join(select(summary1, Step, mean_prop1 = mean_prop, lower_951 = lower_95, upper_951 = upper_95), by = "Step") %>%
  left_join(select(summary2, Step, mean_prop2 = mean_prop, lower_952 = lower_95, upper_952 = upper_95), by = "Step") %>%
  left_join(select(summary3, Step, mean_prop3 = mean_prop, lower_953 = lower_95, upper_953 = upper_95), by = "Step") %>%
  left_join(select(summary4, Step, mean_prop4 = mean_prop, lower_954 = lower_95, upper_954 = upper_95), by = "Step")

final_df$cost_mean_int1 <- final_df$cost_mean_int1$Sum_Vaccine_Mean_Diff
final_df$cost_upper_int1 <- final_df$cost_upper_int1$Sum_Vaccine_Upper_Diff
final_df$cost_lower_int1 <- final_df$cost_lower_int1$Sum_Vaccine_Lower_Diff

final_df$cost_mean_int2 <- final_df$cost_mean_int2$Sum_Vaccine_Mean_Diff
final_df$cost_upper_int2 <- final_df$cost_upper_int2$Sum_Vaccine_Upper_Diff
final_df$cost_lower_int2 <- final_df$cost_lower_int2$Sum_Vaccine_Lower_Diff

final_df$cost_mean_int3 <- final_df$cost_mean_int3$Sum_Vaccine_Mean_Diff
final_df$cost_upper_int3 <- final_df$cost_upper_int3$Sum_Vaccine_Upper_Diff
final_df$cost_lower_int3 <- final_df$cost_lower_int3$Sum_Vaccine_Lower_Diff

final_df$cost_mean_int4 <- final_df$cost_mean_int4$Sum_Vaccine_Mean_Diff
final_df$cost_upper_int4 <- final_df$cost_upper_int4$Sum_Vaccine_Upper_Diff
final_df$cost_lower_int4 <- final_df$cost_lower_int4$Sum_Vaccine_Lower_Diff

# Create a long dataframe for costs
df_costs <- final_df %>%
  select(Step, starts_with("cost_mean"), starts_with("cost_lower"), starts_with("cost_upper")) %>%
  pivot_longer(-Step, names_to = "variable", values_to = "value_cost")


df_costs$intervention <- ifelse(grepl("cost_mean", df_costs$variable),
                                gsub("cost_mean", "", df_costs$variable),
                                ifelse(grepl("cost_lower", df_costs$variable),
                                       gsub("cost_lower", "", df_costs$variable),
                                       gsub("cost_upper", "", df_costs$variable)))


df_costs$type <- ifelse(grepl("cost_mean", df_costs$variable), "cost_mean",
                        ifelse(grepl("cost_lower", df_costs$variable), "cost_lower", "cost_upper"))

df_costs$intervention <- gsub("_int1", "1", df_costs$intervention)
df_costs$intervention <- gsub("_int2", "2", df_costs$intervention)
df_costs$intervention <- gsub("_int3", "3", df_costs$intervention)
df_costs$intervention <- gsub("_int4", "4", df_costs$intervention)

# Create a long dataframe for proportions
df_props <- final_df %>%
  select(Step, starts_with("mean_prop"), starts_with("lower_95"), starts_with("upper_95")) %>%
  pivot_longer(-Step, names_to = "variable", values_to = "value_prop")

df_props$intervention <- ifelse(grepl("mean_prop", df_props$variable),
                                gsub("mean_prop", "", df_props$variable),
                                ifelse(grepl("lower_95", df_props$variable),
                                       gsub("lower_95", "", df_props$variable),
                                       gsub("upper_95", "", df_props$variable)))

df_props$type <- ifelse(grepl("mean_prop", df_props$variable), "mean",
                        ifelse(grepl("lower_95", df_props$variable), "lower", "upper"))

df_merged <- df_costs %>%
  left_join(df_props, by = c("Step", "intervention"))

df_ribbon <- df_merged %>%
  group_by(Step, intervention) %>%
  summarize(
    mean = mean(value_prop[type.y == "mean"], na.rm = TRUE),
    lower = mean(value_prop[type.y == "lower"], na.rm = TRUE),
    upper = mean(value_prop[type.y == "upper"], na.rm = TRUE),
    cost_mean = mean(value_cost[type.x == "cost_mean"], na.rm = TRUE),
    cost_lower = mean(value_cost[type.x == "cost_lower"], na.rm = TRUE),
    cost_upper = mean(value_cost[type.x == "cost_upper"], na.rm = TRUE),
  ) %>%
  ungroup()

df_ribbon[,3:5] <- df_ribbon[,3:5] * 100

# Plot
p2 <- ggplot(df_ribbon, aes(x = cost_mean, y = mean, color = intervention)) + 
  theme_bw() +
  
  # Scatter plot of mean values
  geom_line(size = 1.3) +  # Change size to match the second plot
  
  # Error bars for costs
  geom_errorbarh(aes(xmin = cost_lower, xmax = cost_upper), height = 0.1, size = 1) + 
  
  # Error bars for proportions
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) +  # Adjust width as needed to match the second plot
  
  # Custom aesthetics
  scale_color_manual(
    values = c(
      "1" = "#003f5c",
      "2" = "#7a5195",
      "3" = "#ef5675",
      "4" = "#ffa600"
    ),  # Match colors to the second plot
    name = "Intervetnions"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), name = "Percentage of simulations") +
  labs(x = "Mean Cost", title = "Mean Proportion vs. Mean Cost for Interventions") +
  theme(legend.position = "none")+
  xlab("Cost for intervention doses (USD)") +
  ylab("Percentage of simulations") +
  ggtitle("Percentage of simulations achieving 0% prevalence against\nintervention costs") +
  theme_bw()

print(p2)

combined_plot <- plot_grid(
  p1 + theme(legend.position = "none"), 
  p2 + theme(legend.position = "none"),
  ncol = 2, 
  align = "h"
)


legend_grob <- cowplot::get_legend(p1 + theme(legend.direction = "horizontal",  # Set legend orientation to horizontal
                                              legend.box = "horizontal"  # Arrange legend items in a horizontal box
                                              ))
# Combine the plots with the legend
final_plot <- grid.arrange(combined_plot, legend_grob, nrow = 2, heights = c(10, 1))

# Display the final plot
print(final_plot)

reshaped_df <- df_ribbon %>%
  mutate(cost_combined = sprintf("%.2f (%.2f - %.2f)", 
                                 round(cost_mean, 2), 
                                 round(cost_lower, 2), 
                                 round(cost_upper, 2))) %>%
  select(Step, intervention, cost_combined) %>%
  pivot_wider(names_from = intervention, values_from = cost_combined, names_prefix = "Intervention_")

reshaped_df$Step <- reshaped_df$Step + 1940

print(reshaped_df)

write.csv(reshaped_df, "Int_costs.csv", row.names = FALSE)
