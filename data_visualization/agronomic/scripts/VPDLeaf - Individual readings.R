# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Set output directory (modify this to your desired location)
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/"

# Read the data
data <- read.csv("/home/larajuneb/Masters/Code/Masters/data/spreadsheets/VPDLeaf - Individual readings.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(data)[3] <- "StimBlue+"

# Reshape data into long format
data_long <- data %>%
  pivot_longer(cols = c("Control", "StimBlue+"), names_to = "Treatment", values_to = "Value")
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
      Value[Treatment == "Control"], 
      Value[Treatment == "StimBlue+"], 
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
      Value[Treatment == "Control"], 
      Value[Treatment == "StimBlue+"], 
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
      Value[Treatment == "Control"], 
      Value[Treatment == "StimBlue+"], 
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
      Value[Treatment == "Control"], 
      Value[Treatment == "StimBlue+"], 
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
  labs(title = "Weekly Leaf Vapor Pressure Deficit (VPDLeaf) per Treatment",
       x = "Week",
       y = "VPDLeaf (kPa)",
       color = "Treatment") +
  theme_minimal()

# Print and save the scatter plot (without jitter)
print(p2)
ggsave(filename = paste0(output_dir, "VPDLeaf_individualReadings_scatter_plot_exact.png"), plot = p2, width = 8, height = 6, dpi = 300)

# Create the scatter plot with jitter
p3 <- ggplot(data_long, aes(x = Week, y = Value, color = Treatment)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  labs(title = "Weekly Leaf Vapor Pressure Deficit (VPDLeaf) per Treatment",
       x = "Week",
       y = "VPDLeaf (kPa)",
       color = "Treatment") +
  theme_minimal()

# Print and save the scatter plot (with jitter)
print(p3)
ggsave(filename = paste0(output_dir, "VPDLeaf_individualReadings_scatter_plot_jittered.png"), plot = p3, width = 8, height = 6, dpi = 300)

