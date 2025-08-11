# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("C:/Users/User/OneDrive - Stellenbosch University/Desktop/Masters/")

data_type <- ""
# Set output directory (modify this to your desired location)
output_dir <- "data_visualization/agronomic/plots/salt_stress/compare_cases/"

# Read the data
data <- read.csv("data/salt_stress/compare_cases/LeafNumber_RT_SB_Ctrl.csv", header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
rownames(data) <- NULL
data
# Reshape data into long format
data_long <- data %>%
  pivot_longer(cols = c("Standard", "Excl outliers"), names_to = "Case", values_to = "Value")
data_long
# Remove rows where Value is empty
data_long <- data_long[!is.na(data_long$Value) & data_long$Value != "", ]

# Convert Week to a numeric factor to ensure correct ordering
data_long$Week <- factor(data_long$Week, levels = sort(unique(as.numeric(data_long$Week))), ordered = TRUE)

# Reset row numbering
rownames(data_long) <- NULL
data_long$Value <- as.numeric(data_long$Value)
data_long

p1 <- ggplot(data_long, aes(x = Week, y = Value, color = Case, group = Case)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(title = "p-values for right-tailed Student's t-tests (StimBlue+ u > Control u) for Leaf Number Over Time Comparing Standard vs Excl Outliers",
       x = "Week",
       y = "Leaf Number",
       color = "Treatment") +
  theme_minimal() +
  geom_vline(xintercept = c(1, 5, 9), linetype = "dashed", color = "grey40") +
  coord_cartesian(clip = "off") +
  annotate("text", x = 1, y = -0.1, label = "T1", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 5, y = -0.1, label = "T2", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 9, y = -0.1, label = "T3", vjust = 2, hjust = -0.5, angle = 0, size = 3)

# Print the line plot
print(p1)

# Save the line plot as a PNG file
ggsave(filename = paste0(output_dir, "LeafNumbers_line_plot.png"), plot = p1, width = 8, height = 6, dpi = 300)


# Create the scatter plot (without jitter)
p2 <- ggplot(data_long, aes(x = Week, y = Value, color = Case)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "p-values for right-tailed Student's t-tests (StimBlue+ u > Control u) for Leaf Number Over Time Comparing Standard vs Excl Outliers",
       x = "Week",
       y = "p-value",
       color = "Case") +
  theme_minimal() +
  geom_vline(xintercept = c(1, 5, 9), linetype = "dashed", color = "grey40") +
  coord_cartesian(clip = "off") +
  annotate("text", x = 1, y = -0.1, label = "T1", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 5, y = -0.1, label = "T2", vjust = 2, hjust = -0.5, angle = 0, size = 3) +
  annotate("text", x = 9, y = -0.1, label = "T3", vjust = 2, hjust = -0.5, angle = 0, size = 3)

# Print and save the scatter plot (without jitter)
print(p2)
ggsave(filename = paste0(output_dir, "LeafNumbers_scatter_plot_exact.png"), plot = p2, width = 8, height = 6, dpi = 300)
