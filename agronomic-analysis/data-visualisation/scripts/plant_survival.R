# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Set output directory (modify this to your desired location)
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/salt_stress/"

# Read the data from CSV
data <- read.csv("/home/larajuneb/Masters/Code/Masters/data/salt_stress/plant_survival.csv", header=TRUE, stringsAsFactors=FALSE, check.names = FALSE)

# Optional: Check the column names
colnames(data)

# Reshape data to long format
data_long <- pivot_longer(data, 
                          cols = -Week, 
                          names_to = "Treatment", 
                          values_to = "Survival")

# Plot 1: Line chart
p1 <- ggplot(data_long, aes(x = Week, y = Survival, color = Treatment)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Plant Survival Over Time by Treatment",
       x = "Week",
       y = "Number of Surviving Plants") +
  theme_minimal()

 ggsave(filename = paste0(output_dir, "Plant_survival_line_plot.png"), plot = p1, width = 8, height = 6, dpi= 300)

# Plot 2: Bar plot (stacked)
p2 <- ggplot(data_long, aes(x = factor(Week), y = Survival, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Plant Survival by Week and Treatment",
       x = "Week",
       y = "Number of Surviving Plants") +
  theme_minimal()

ggsave(filename = paste0(output_dir, "Plant_survival_bar_plot.png"), plot = p2, width = 8, height = 6, dpi= 300)
