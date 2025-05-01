# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Set output directory (modify this to your desired location)
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/"

# Read the data
data <- read.csv("/home/larajuneb/Masters/Code/Masters/data/spreadsheets/Dry Weight - Shoots.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(data)[2] <- "StimBlue+"
data
# Reshape data into long format
data_long <- data %>%
  pivot_longer(cols = c("Control", "StimBlue+"), names_to = "Treatment", values_to = "Value")
data_long
# Remove rows where Value is empty
data_long <- na.omit(data_long)
data_long

# Reset row numbering
rownames(data_long) <- NULL
data_long$Value <- as.numeric(data_long$Value)
data_long

# Convert Treatment column to a factor (ensures categorical x-axis)
data_long$Treatment <- as.factor(data_long$Treatment)
data_long

# Perform Shapiro-Wilk test for each Treatment group
shapiro_results <- data_long %>%
  group_by(Treatment) %>%
  summarise(
    p_value = shapiro.test(Value)$p.value,
    normality = ifelse(p_value > 0.05, "Yes", "No")
  )

# Print results
print(shapiro_results)

# Perform right-tailed t-test between Control and StimBlue+ (assuming equal variance)
RT_Students_t_test_results <- data_long %>%
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

# Perform left-tailed t-test between Control and StimBlue+ (assuming equal variance)
LT_Students_t_test_results <- data_long %>%
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

# Perform right-tailed Welch's t-test (unequal variance) between Control and StimBlue+
RT_Welchs_t_test_results <- data_long %>%
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

# Perform left-tailed Welch's t-test (unequal variance) between Control and StimBlue+
LT_Welchs_t_test_results <- data_long %>%
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
p1 <- ggplot(data_long, aes(x = Treatment, y = Value, color = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Effect of StimBlue+ on Shoot Dry Weight",
       x = "Treatment",
       y = "Shoot Dry Weight (g)",
       color = "Treatment") +
  theme_minimal()

# Print the scatter plot
print(p1)

# Save the scatter plot as a PNG file
ggsave(filename = paste0(output_dir, "DryWeight-Shoots_scatter_plot_exact.png"), plot = p1, width = 8, height = 6, dpi = 300)

print(data_long)
# Create the scatter plot with jitter
p2 <- ggplot(data_long, aes(x = Treatment, y = Value, color = Treatment)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  labs(title = "Effect of StimBlue+ on Shoot Dry Weight",
       x = "Treatment",
       y = "Shoot Dry Weight (g)",
       color = "Treatment") +
  theme_minimal()

print(data_long)

# Print the scatter plot
print(p2)

# Save the scatter plot as a PNG file
ggsave(filename = paste0(output_dir, "DryWeight-Shoots_scatter_plot_jitter.png"), plot = p2, width = 8, height = 6, dpi = 300)



# Read the data
data <- read.csv("/home/larajuneb/Masters/Code/Masters/data/spreadsheets/Dry Weight - Roots.csv", header=TRUE, stringsAsFactors=FALSE)
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

# Perform Shapiro-Wilk test for each Treatment group
shapiro_results <- data_long %>%
  group_by(Treatment) %>%
  summarise(
    p_value = shapiro.test(Value)$p.value,
    normality = ifelse(p_value > 0.05, "Yes", "No")
  )

# Print results
print(shapiro_results)

# Perform right-tailed t-test between Control and StimBlue+ (assuming equal variance)
RT_Students_t_test_results <- data_long %>%
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

# Perform left-tailed t-test between Control and StimBlue+ (assuming equal variance)
LT_Students_t_test_results <- data_long %>%
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

# Perform right-tailed Welch's t-test (unequal variance) between Control and StimBlue+
RT_Welchs_t_test_results <- data_long %>%
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

# Perform left-tailed Welch's t-test (unequal variance) between Control and StimBlue+
LT_Welchs_t_test_results <- data_long %>%
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
p1 <- ggplot(data_long, aes(x = Treatment, y = Value, color = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Effect of StimBlue+ on Root Dry Weight",
       x = "Treatment",
       y = "Root Dry Weight (g)",
       color = "Treatment") +
  theme_minimal()

# Print the bar plot
print(p1)

# Save the bar plot as a PNG file
ggsave(filename = paste0(output_dir, "DryWeight-Roots_scatter_plot_exact.png"), plot = p1, width = 8, height = 6, dpi = 300)

