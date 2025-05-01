# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Set output directory (modify this to your desired location)
output_dir <- "/home/larajuneb/Masters/Code/Masters/data_visualization/agronomic/plots/"

# Read the data
data <- read.csv("/home/larajuneb/Masters/Code/Masters/data/spreadsheets/Change in Shoot Length.csv", header=FALSE, stringsAsFactors=FALSE)
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

# Create a bar plot
p_bar <- ggplot(data_long, aes(x = Week, y = Value, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weekly Change in Shoot Length per Treatment",
       x = "Week",
       y = "Change in Shoot Length (mm)",
       fill = "Treatment") +
  theme_minimal()

# Print the bar plot
print(p_bar)

# Save the bar plot as a PNG file
ggsave(filename = paste0(output_dir, "ChangeInShootLength_bar_plot.png"), plot = p_bar, width = 8, height = 6, dpi = 300)

# Create the scatter plot (without jitter)
p2 <- ggplot(data_long, aes(x = Week, y = Value, color = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Weekly Change in Shoot Length per Treatment",
       x = "Week",
       y = "Change in Shoot Length (mm)",
       color = "Treatment") +
  theme_minimal()

# Print and save the scatter plot (without jitter)
print(p2)
ggsave(filename = paste0(output_dir, "ChangeInShootLength_scatter_plot_exact.png"), plot = p2, width = 8, height = 6, dpi = 300)

# Create the scatter plot with jitter
p3 <- ggplot(data_long, aes(x = Week, y = Value, color = Treatment)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  labs(title = "Weekly Change in Shoot Length per Treatment",
       x = "Week",
       y = "Change in Shoot Length (mm)",
       color = "Treatment") +
  theme_minimal()

# Print and save the scatter plot (with jitter)
print(p3)
ggsave(filename = paste0(output_dir, "ChangeInShootLength_scatter_plot_jittered.png"), plot = p3, width = 8, height = 6, dpi = 300)

shapiro_wide <- shapiro_results %>%
  select(-p_value) %>%  # Remove p-value column
  pivot_wider(names_from = Treatment, values_from = normality) %>%
  rename(Normal_Control = Control, Normal_StimBlue = `StimBlue+`) %>%
  drop_na()  # Remove any rows with NA values

ranked_weeks <- RT_Students_t_test_results %>%
  arrange(`p_value`) %>%
  mutate(Rank = row_number()) %>%
  select(Week, `p_value`, Rank)
print(ranked_weeks)
