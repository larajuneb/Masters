#plots for comparing p-values for all cases in salt trial

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

setwd("C:/Users/User/OneDrive - Stellenbosch University/Desktop/Masters/")

# Set output directory (modify this to your desired location)
output_dir <- "data_visualization/agronomic/plots/salt_stress/compare_cases/"

# Read the data
data <- read.csv("data_visualization/agronomic/test_results/salt_stress/all_incl_dead/ChangeInLeafNumber_left_tailed_results.csv", header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)

# Define treatment group order
group_order <- c("StimBlue+", "Control", "StimBlue+ + NaCl", "NaCl")

# Filter to keep only one direction of each comparison and exclude normality failures
data_filtered <- data %>%
  mutate(Group1 = factor(Group1, levels = group_order),
         Group2 = factor(Group2, levels = group_order),
         normality_failed = as.logical(normality_failed)) %>%
  filter(as.integer(Group1) < as.integer(Group2)) %>%           # only one direction
  filter(!normality_failed) %>%                                  # exclude non-normal
  mutate(comparison = paste(as.character(Group1), "vs", as.character(Group2)))

data_filtered$Week <- factor(data_filtered$Week, levels = 1:13)

# Line plot
p_line <- ggplot(data_filtered, aes(x = Week, y = p_value, color = comparison, group = comparison)) +
  geom_line(size = 1) +
  geom_point(data = data_filtered %>% filter(normality_failed), aes(x = Week, y = p_value), 
             shape = 21, fill = "red", color = "black", size = 3, stroke = 1.5) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "grey30") +
  ylim(0, 1) +
  labs(title = "P-values over Time by Group Comparison",
       subtitle = "Red points indicate normality test failed",
       y = "p-value",
       x = "Week") +
  theme_minimal() +
  theme(legend.position = "right")

print(p_line)

# Save the line plot
ggsave("p_values_line_plot.png", plot = p_line, width = 10, height = 6, dpi = 300)


# Dot plot
p_dot <- ggplot(data_filtered, aes(x = Week, y = p_value, color = comparison)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = data_filtered %>% filter(normality_failed), aes(x = Week, y = p_value), 
             shape = 8, color = "red", size = 3, stroke = 1.5) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "grey30") +
  ylim(0, 1) +
  labs(title = "P-values over Time by Group Comparison",
       subtitle = "Stars indicate normality test failed",
       y = "p-value",
       x = "Week") +
  theme_minimal() +
  theme(legend.position = "right")

print(p_dot)

# Save the dot plot
ggsave("p_values_dot_plot.png", plot = p_dot, width = 10, height = 6, dpi = 300)