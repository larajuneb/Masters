# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Set output directory (modify this to your desired location)
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/salt_stress/"

# Read the data
data <- read.csv("/home/larajuneb/Masters/Code/Masters/data/salt_stress/ETR - Individual readings.csv", header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
colnames(data)[3] <- "StimBlue+"

# Reshape data into long format
data_long <- data %>%
  pivot_longer(cols = c("Control", "StimBlue+", "StimBlue+ + NaCl", "NaCl"), names_to = "Treatment", values_to = "Value")
data_long
# Remove rows where Value is empty
data_long <- na.omit(data_long)
data_long
# Convert Week to a numeric factor to ensure correct ordering
data_long$Week <- factor(data_long$Week, levels = sort(unique(as.numeric(data_long$Week))), ordered = TRUE)

# Perform Shapiro-Wilk test for each Week and Treatment group
shapiro_results <- data_long %>%
  group_by(Week, Treatment) %>%
  summarise(
    p_value = shapiro.test(Value)$p.value,
    normality = ifelse(p_value > 0.05, "Yes", "No")
  )

# Print results
print(shapiro_results)

# Perform right-tailed t-test for each week
RT_Students_t_test_results <- data_long %>%
  group_by(Week) %>%
  summarise(
    t_test = list(t.test(
      Value[Treatment == "StimBlue+"], 
      Value[Treatment == "Control"], 
      alternative = "greater", 
      var.equal = TRUE
    )),
    .groups = "drop"
  ) %>%
  mutate(
    t_statistic = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value)
  )

# Display results
print(RT_Students_t_test_results)

# Perform right-tailed t-test for each week
LT_Students_t_test_results <- data_long %>%
  group_by(Week) %>%
  summarise(
    t_test = list(t.test(
      Value[Treatment == "StimBlue+"], 
      Value[Treatment == "Control"], 
      alternative = "less", 
      var.equal = TRUE
    )),
    .groups = "drop"
  ) %>%
  mutate(
    t_statistic = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value)
  )

# Display results
print(LT_Students_t_test_results)

# Perform right-tailed t-test for each week
RT_Welchs_t_test_results <- data_long %>%
  group_by(Week) %>%
  summarise(
    t_test = list(t.test(
      Value[Treatment == "StimBlue+"], 
      Value[Treatment == "Control"], 
      alternative = "greater", 
      var.equal = FALSE
    )),
    .groups = "drop"
  ) %>%
  mutate(
    t_statistic = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value)
  )

# Display results
print(RT_Welchs_t_test_results)

# Perform right-tailed t-test for each week
LT_Welchs_t_test_results <- data_long %>%
  group_by(Week) %>%
  summarise(
    t_test = list(t.test(
      Value[Treatment == "StimBlue+"], 
      Value[Treatment == "Control"], 
      alternative = "less", 
      var.equal = FALSE
    )),
    .groups = "drop"
  ) %>%
  mutate(
    t_statistic = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value)
  )

# Display results
print(LT_Welchs_t_test_results)

# Create the scatter plot (without jitter)
p2 <- ggplot(data_long, aes(x = Week, y = Value, color = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Weekly Electron Transport Rate (ETR) per Treatment",
       x = "Week",
       y = "ETR (umol+1m-2s-1)",
       color = "Treatment") +
  theme_minimal() +
  geom_vline(xintercept = c(1, 5, 9), linetype = "dashed", color = "grey40") +
  coord_cartesian(clip = "off") +
  annotate("text", x = 1, y = 20, label = "T1", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 5, y = 20, label = "T2", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 9, y = 20, label = "T3", vjust = 2, hjust = -0.5, angle = 0, size = 3)


# Print and save the scatter plot (without jitter)
print(p2)
ggsave(filename = paste0(output_dir, "ETR_individualReadings_scatter_plot_exact.png"), plot = p2, width = 8, height = 6, dpi = 300)

# Create the scatter plot with jitter
p3 <- ggplot(data_long, aes(x = Week, y = Value, color = Treatment)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  labs(title = "Weekly Electron Transport Rate (ETR) per Treatment",
       x = "Week",
       y = "ETR (umol+1m-2s-1)",
       color = "Treatment") +
  theme_minimal() +
  geom_vline(xintercept = c(1, 5, 9), linetype = "dashed", color = "grey40") +
  coord_cartesian(clip = "off") +
  annotate("text", x = 1, y = 20, label = "T1", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 5, y = 20, label = "T2", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 9, y = 20, label = "T3", vjust = 2, hjust = -0.5, angle = 0, size = 3)


# Print and save the scatter plot (with jitter)
print(p3)
ggsave(filename = paste0(output_dir, "ETR_individualReadings_scatter_plot_jittered.png"), plot = p3, width = 8, height = 6, dpi = 300)

# ==== NEW SECTION: Significance star annotations ====

# Set output directory (modify this to your desired location)
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/significance_marked/"

# Reshape Shapiro results into wide format
shapiro_wide <- shapiro_results %>%
  select(-p_value) %>%  # Remove p-value column
  pivot_wider(names_from = Treatment, values_from = normality) %>%
  rename(Normal_Control = Control, Normal_StimBlue = `StimBlue+`) %>%
  drop_na()  # Remove any rows with NA values

sig_annotations <- RT_Students_t_test_results %>%
  left_join(shapiro_wide, by = "Week") %>%
  filter(Normal_Control == "Yes", Normal_StimBlue == "Yes") %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  filter(significance != "") %>%
  select(Week, significance)

# y-position for placing stars above max values
data_max <- data_long %>%
  group_by(Week) %>%
  summarise(max_y = max(Value), .groups = "drop")

# Compute dynamic offset: 5% of the y-axis range
y_range <- max(data_long$Value, na.rm = TRUE) - min(data_long$Value, na.rm = TRUE)

sig_annotations <- sig_annotations %>%
  left_join(data_max, by = "Week") %>%
  mutate(y_pos = max_y + 0.05 * y_range)  # adjust spacing if needed

# Create the scatter plot (without jitter)
p2_sig <- ggplot(data_long, aes(x = Week, y = Value, color = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Weekly Electron Transport Rate (ETR) per Treatment",
       x = "Week",
       y = "ETR (umol+1m-2s-1)",
       color = "Treatment") +
  theme_minimal() +
  geom_text(data = sig_annotations,
            aes(x = Week, y = y_pos, label = significance),
            inherit.aes = FALSE,
            size = 6,
            vjust = 0) +
  geom_vline(xintercept = c(1, 5, 9), linetype = "dashed", color = "grey40") +
  coord_cartesian(clip = "off") +
  annotate("text", x = 1, y = 20, label = "T1", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 5, y = 20, label = "T2", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 9, y = 20, label = "T3", vjust = 2, hjust = -0.5, angle = 0, size = 3)


# Print and save the scatter plot (without jitter)
print(p2_sig)
ggsave(filename = paste0(output_dir, "ETR_individualReadings_scatter_plot_exact.png"), plot = p2_sig, width = 8, height = 6, dpi = 300)

# Create the scatter plot with jitter
p3_sig <- ggplot(data_long, aes(x = Week, y = Value, color = Treatment)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  labs(title = "Weekly Electron Transport Rate (ETR) per Treatment",
       x = "Week",
       y = "ETR (umol+1m-2s-1)",
       color = "Treatment") +
  theme_minimal() +
  geom_text(data = sig_annotations,
            aes(x = Week, y = y_pos, label = significance),
            inherit.aes = FALSE,
            size = 6,
            vjust = 0) +
  geom_vline(xintercept = c(1, 5, 9), linetype = "dashed", color = "grey40") + 
  coord_cartesian(clip = "off") +
  annotate("text", x = 1, y = 20, label = "T1", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 5, y = 20, label = "T2", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 9, y = 20, label = "T3", vjust = 2, hjust = -0.5, angle = 0, size = 3)

# Print and save the scatter plot (with jitter)
print(p3_sig)
ggsave(filename = paste0(output_dir, "ETR_individualReadings_scatter_plot_jittered.png"), plot = p3_sig, width = 8, height = 6, dpi = 300)

############################# RANKS
ranked_weeks <- RT_Students_t_test_results %>%
  arrange(`p_value`) %>%
  mutate(Rank = row_number()) %>%
  select(Week, `p_value`, Rank)
print(ranked_weeks)

output_row <- c(DataType = "PhiPS2 - Indiv.", setNames(as.list(ranked_weeks$Week), as.character(ranked_weeks$Rank)))
output_df <- as.data.frame(t(output_row), stringsAsFactors = FALSE)

rankings_file <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/week_rankings.csv"

# Load existing data (if any)
if (file.exists(rankings_file)) {
  rankings <- read.csv(rankings_file, check.names = FALSE, stringsAsFactors = FALSE)
} else {
  rankings <- NULL
}

data_type <- "ETR - Indiv."

# Ensure ranked_weeks is ordered by Rank
ranked_weeks <- ranked_weeks[order(ranked_weeks$Rank), ]
week_values <- as.character(ranked_weeks$Week)  # Ensure it's a character vector

# Create named row: DataType + Weeks 1–13
output_row <- c(DataType = data_type, setNames(week_values, as.character(1:13)))
output_df <- as.data.frame(t(output_row), stringsAsFactors = FALSE)

# Check if DataType already exists
if (is.null(rankings) || !(data_type %in% rankings$DataType)) {
  if (!is.null(rankings)) {
    # Ensure columns match and convert all to character
    rankings[] <- lapply(rankings, as.character)
    output_df[] <- lapply(output_df, as.character)
    updated_data <- rbind(rankings, output_df)
  } else {
    updated_data <- output_df
  }
  
  # Write to file
  write.csv(updated_data, rankings_file, row.names = FALSE, quote = TRUE)
  message("Row added.")
} else {
  message("DataType already exists. Row not added.")
}

########################################### HEATMAP
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/significance_marked/pvalue_heatmaps/"

# Plotting
pvalue_heatmap <- ggplot(RT_Students_t_test_results, aes(x = Week, y = 1, fill = p_value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = sprintf("%.3f", p_value)), color = "black", size = 4) +
  scale_fill_gradientn(colors = heat.colors(100), limits = c(0, 1), name = "p-value") +
  coord_fixed(ratio = 1) +  # This makes each tile a square
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  ggtitle("Right-tailed Student's t-test p-values by Week for ETR (individual readings)")

# Print the plot
print(pvalue_heatmap)
ggsave(filename = paste0(output_dir, "ETR_individual_readings.png"), plot = pvalue_heatmap, width = 8, height = 6, dpi = 300)