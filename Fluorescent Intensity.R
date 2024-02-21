# Load necessary libraries
library(ggplot2)  # For data visualization
library(car)      # For Levene's test
library(ggpubr)   # For adding p-values manually to ggplot2 plots

# Figure S1B: Control vs Calhm2 MO Analysis
############################################################

# Creating dataset for Control MO
control_mo_data <- data.frame(
  Value = c(160.65, 139.95, 141.73, 110.21, 141.96, 120.56, 116.40, 142.88, 
            134.67, 157.51, 132.80, 125.03, 146.03, 142.67, 155.78, 147.51, 150.95, 
            110.39, 108.17, 155.02, 101.54, 94.55, 116.57, 157.87, 133.30, 125.95, 
            133.51, 113.51, 125.54, 122.90, 142.56, 115.24, 162.19, 152.94, 116.93, 
            161.95, 113.78, 108.59, 170.30, 140.93, 141.16, 133.45, 134.57, 136.18, 
            127.07, 120.11, 152.29, 145.50, 136.77, 119.52, 143.52, 155.61),
  Group = rep(c("Group 1", "Group 2", "Group 3"), times = c(17, 18, 17)),
  Type = "Control MO"
)

# Creating dataset for Mutant
mutant_data <- data.frame(
  Value = c(95.31, 90.53, 91.73, 61.17, 116.83, 66.99, 104.86, 67.06, 120.14, 
            57.09, 76.78, 54.12, 55.78, 40.16, 60.33, 56.21, 47.97, 70.43, 40.18, 
            83.28, 107.08, 88.44, 83.35, 78.15, 82.11, 69.12, 104.06, 66.02, 77.31, 
            65.49, 72.63, 63.85, 64.18, 72.95, 44.48, 71.53, 67.97, 61.09, 91.30, 
            94.82, 113.68, 111.96, 116.59, 89.11, 89.15, 65.58, 78.51, 102.69, 
            71.43, 107.91, 97.51, 56.28, 62.79, 105.89, 81.89),
  Group = rep(c("Group 1", "Group 2", "Group 3"), times = c(20, 18, 17)),
  Type = "Mutant"
)

# Combining both datasets
combined_data <- rbind(control_mo_data, mutant_data)

# Statistical Analysis
############################################################

# Function to calculate mean and standard deviation for plotting
mean_sd <- function(x) {
  return(data.frame(y = mean(x), ymin = mean(x) - sd(x), ymax = mean(x) + sd(x)))
}

# Shapiro-Wilk normality test for Control MO and Mutant groups
shapiro_test_control_mo <- shapiro.test(combined_data$Value[combined_data$Type == "Control MO"])
shapiro_test_mutant <- shapiro.test(combined_data$Value[combined_data$Type == "Mutant"])

# Printing Shapiro-Wilk test results
print(shapiro_test_control_mo)
print(shapiro_test_mutant)

# Levene's test for equal variances between Control MO and Mutant groups
levene_test_full <- leveneTest(Value ~ Type, data = combined_data)
print(levene_test_full) # Printing Levene's test result

# Two-sample t-test for Control MO vs Mutant (assuming equal variances)
t_test_result_standard <- t.test(Value ~ Type, data = combined_data, var.equal = TRUE)
print(t_test_result_standard) # Printing t-test result

# Data Visualization
############################################################

# Preparing a data frame for manual p-value annotation
stat.test <- data.frame(
  group1 = "Control MO",
  group2 = "Mutant",
  p.value = 2.2e-16,  # Example p-value
  y.position = 180  # Adjust based on plot's scale
)

# Creating the plot
p <- ggplot(combined_data, aes(x = Type, y = Value, color = Group)) +
  geom_jitter(width = 0.2, size = 3) +  # Adding jitter to points for better visibility
  scale_color_manual(values = c("blue", "magenta", "#00FF00")) +  # Custom color scale
  stat_summary(fun.data = mean_sd, geom = "errorbar", width = 0.2, color = "black") +  # Error bars for SD
  stat_summary(fun = mean, geom = "crossbar", width = 0.2, 
               aes(ymin = after_stat(y), ymax = after_stat(y)), color = "black") +  # Mean bars
  labs(y = "Fluorescent Intensity", x = "") +  # Axis labels
  theme_minimal() +  # Minimal theme for clean look
  theme(legend.position = "none",  # Custom theme settings for aesthetics
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 10),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200))  # Y-axis scale adjustment

# Adding manual p-value annotation to the plot
p <- p + stat_pvalue_manual(stat.test, label = "p.value", tip.length = 0.02)

# Displaying the plot
print(p)

# Optionally, saving the plot to a file
ggsave("fluo_intensity.png", plot = p, width = 10, height = 6, dpi = 300)
         