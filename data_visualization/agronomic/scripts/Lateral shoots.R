# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Set output directory (modify this to your desired location)
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/"

# Read the data
data <- read.csv("/home/larajuneb/Masters/Code/Masters/data/spreadsheets/Lateral Shoots.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(data)[2] <- "StimBlue+"
# Reshape data into long format
data_long <- data %>%
  pivot_longer(cols = c("Control", "StimBlue+"), names_to = "Treatment", values_to = "Value")
data_long
# Remove rows where Value is empty
data_long <- na.omit(data_long)

# Reset row numbering
rownames(data_long) <- NULL
data_long$Value <- as.numeric(data_long$Value)
data_long

# Convert Treatment column to a factor (ensures categorical x-axis)
data_long$Treatment <- as.factor(data_long$Treatment)

# Create the scatter plot (without jitter)
p1 <- ggplot(data_long, aes(x = Treatment, y = Value, color = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Effect of StimBlue+ on Lateral Shoots",
       x = "Treatment",
       y = "Number of Lateral Shoots",
       color = "Treatment") +
  theme_minimal()

# Print the bar plot
print(p1)

# Save the bar plot as a PNG file
ggsave(filename = paste0(output_dir, "LateralShoots_scatter_plot_exact.png"), plot = p1, width = 8, height = 6, dpi = 300)