# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Set output directory (modify this to your desired location)
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/"

# Read the data
data <- read.csv("/home/larajuneb/Masters/Code/Masters/data/spreadsheets/Leaf Numbers.csv", header=FALSE, stringsAsFactors=FALSE)
data <- separate(data, col = V1, into = c("Week", "Control", "StimBlue+"), sep = ",")
data <- data[-1, ]
rownames(data) <- NULL
data
# Reshape data into long format
data_long <- data %>%
  pivot_longer(cols = c("Control", "StimBlue+"), names_to = "Treatment", values_to = "Value")
data_long
# Remove rows where Value is empty
data_long <- data_long[data_long$Value != "", ]

# Convert Week to a numeric factor to ensure correct ordering
data_long$Week <- factor(data_long$Week, levels = sort(unique(as.numeric(data_long$Week))), ordered = TRUE)


# Reset row numbering
rownames(data_long) <- NULL
data_long$Value <- as.numeric(data_long$Value)
data_long

# Create a bar plot
p_bar <- ggplot(data_long, aes(x = Week, y = Value, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weekly Leaf Gain per Treatment",
       x = "Week",
       y = "Leaves Gained",
       fill = "Treatment") +
  theme_minimal()

# Print the bar plot
print(p_bar)

# Save the bar plot as a PNG file
ggsave(filename = paste0(output_dir, "ChangeInLeafNumbers_bar_plot.png"), plot = p_bar, width = 8, height = 6, dpi = 300)
