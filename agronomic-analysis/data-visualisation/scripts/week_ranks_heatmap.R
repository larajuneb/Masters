# Load necessary libraries
library(ggplot2)
library(reshape2)
library(RColorBrewer)

# Read the CSV file with the updated path
rankings_file <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/week_rankings.csv"
data <- read.csv(rankings_file, stringsAsFactors = FALSE)

# Reshape the data from wide to long format for plotting
long_data <- melt(data, id.vars = "DataType", variable.name = "Week", value.name = "Rank")

# Convert Rank to numeric (handling NAs)
long_data$Rank <- as.numeric(as.character(long_data$Rank))

# Define the color scale (13 distinct colors, mapped to the range 1-13)
color_scale <- scale_fill_gradientn(colors = heat.colors(13), limits = c(1, 13))

# Handle NA as grey (or white)
long_data$Rank[is.na(long_data$Rank)] <- 0  # Assign 0 to NAs, so we can map it to a grey color

# Keep the row order from the CSV
long_data$DataType <- factor(long_data$DataType, levels = unique(long_data$DataType))

# Define the plot
heatmap_plot <- ggplot(long_data, aes(x = Week, y = DataType, fill = Rank)) +
  geom_tile() +
  geom_text(aes(label = ifelse(Rank == 0, "NA", as.character(Rank))), color = "black", size = 5) +  # Add week number inside each tile
  color_scale +  # Apply the color scale
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Week Rankings Heatmap")

# Print the plot
print(heatmap_plot)

# Set output directory (modify this to your desired location)
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/significance_marked/"

ggsave(filename = paste0(output_dir, "week_rankings_heatmap.png"), plot = heatmap_plot, width = 8, height = 6, dpi = 300)
