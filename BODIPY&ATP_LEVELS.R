# Load necessary libraries
library(car) # For Levene's test
library(dunn.test) # For Dunn's post hoc test
library(ggplot2) # For plotting
library(ggpubr) # For stat_pvalue_manual function
library(dplyr) # For data manipulation

# Make dataframe
control_data <- c(
  30.95, 23.92, 26.62, 18.35, 19.93, 6.67, 17.94, 17.10, 9.02, 46.40,
  33.47, 20.79, 30.40, 42.89, 48.47, 40.39, 41.47, 40.19, 50.75, 16.01,
  68.18, 55.32, 61.26, 55.27, 63.90, 84.63, 73.39, 41.49, 102.43, 151.54
)

calhm2_ko_data <- c(
  71.11, 83.29, 176.01, 79.86, 45.79, 28.67, 93.40, 144.09, 77.76, 182.61,
  74.91, 42.59, 106.06, 132.80, 32.27, 55.88, 95.81, 128.56, 123.74, 89.58,
  136.97, 138.14, 94.09, 145.04, 80.63, 65.04, 91.78, 128.00
)

control_fccp_data <- c(
  41.80, 82.26, 46.26, 49.17, 66.98, 66.95, 18.20, 13.62, 60.20, 29.02,
  147.20, 56.92, 88.68, 126.68, 46.90, 29.96, 36.71, 66.27, 35.44, 29.38,
  62.02, 54.77, 57.10, 69.19, 24.27, 31.90, 65.04, 43.72, 39.76, 34.54
)

# Determine the maximum length among all vectors
max_length <- max(length(control_data), length(calhm2_ko_data), length(control_fccp_data))

# Pad shorter vectors with NA values to make them the same length
control_data <- c(control_data, rep(NA, max_length - length(control_data)))
calhm2_ko_data <- c(calhm2_ko_data, rep(NA, max_length - length(calhm2_ko_data)))
control_fccp_data <- c(control_fccp_data, rep(NA, max_length - length(control_fccp_data)))

# Create a DataFrame
combined_df <- data.frame(
  Control = control_data,
  Calhm2_KO = calhm2_ko_data,
  Control_FCCP = control_fccp_data
)

# Check for normality
shapiro_test_control <- shapiro.test(combined_df$Control)
shapiro_test_calhm2_ko <- shapiro.test(combined_df$Calhm2_KO)
shapiro_test_control_fccp <- shapiro.test(combined_df$Control_FCCP)

# Print Shapiro-Wilk test results
print(shapiro_test_control)
print(shapiro_test_calhm2_ko)
print(shapiro_test_control_fccp)

# Reshape data for Levene's test
reshaped_data <- stack(combined_df)

# Perform Levene's test
levene_test <- leveneTest(values ~ ind, data = reshaped_data)

# Print Levene's test result
print(levene_test)

# Perform Kruskal-Wallis test
kruskal_test <- kruskal.test(values ~ ind, data = reshaped_data)

# Print Kruskal-Wallis test result
print(kruskal_test)

# Perform Dunn's test for multiple comparisons
dunn_test_result <- dunn.test(reshaped_data$values, reshaped_data$ind, method="bonferroni")

# Print Dunn's test result
print(dunn_test_result)

group_stats <- reshaped_data %>%
  group_by(ind) %>%
  summarise(
    mean = mean(values, na.rm = TRUE),
    sd = sd(values, na.rm = TRUE)
  )
max_value <- max(reshaped_data$values, na.rm = TRUE)
y_position <- max_value * 1.05  # Adjust as needed

pvalue_data <- data.frame(
  group1 = "Control",
  group2 = "Calhm2_KO",
  p.value = formatC(dunn_test_result$P.adjusted[1], format = "e", digits = 2),
  y.position = max_value * 1.05
)

my_colors <- c("blue", "#FF0000", "green3")

ggplot(reshaped_data, aes(x = ind, y = values, color = ind)) +
  geom_jitter(width = 0.2, alpha = 0.5) +  # Jittered data points with custom colors
  geom_errorbar(data = group_stats, aes(y = mean, ymin = mean - sd, ymax = mean + sd), width = 0.2) +  # Error bars for standard deviation
  geom_point(data = group_stats, aes(y = mean), shape = 95, size = 8, color = "black") +  # Mean points
  stat_pvalue_manual(pvalue_data, label = "p.value", tip.length = 0.02) +  # Add p-value annotation
  theme_minimal() +  # Minimal theme for the plot
  scale_color_manual(values = my_colors) +  # Apply custom color palette
  theme(
    legend.position = "none",  # Remove the legend
    axis.line = element_line(color = "black"),  # Add borders to the axes
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 12, face = "bold")  # Customize y-axis title
  ) +
  ylab("Fluorescent Intensity/Area")  # Label the y-axis

# ATP Content Analysis
# Create the dataframe
data <- data.frame(
  Control = c(88.5, 107.0, 104.0),
  Calhm2_KD = c(54, 49, 47)
)

# View the dataframe
print(data)

# Shapiro-Wilk test for normality
shapiro_test_control <- shapiro.test(data$Control)
shapiro_test_calhm2_kd <- shapiro.test(data$Calhm2_KD)

print(shapiro_test_control)
print(shapiro_test_calhm2_kd)

# Reshape the data to long format for Levene's test
data_long <- melt(data)
colnames(data_long) <- c("Group", "Value")

# Perform Levene's test
levene_test <- leveneTest(Value ~ Group, data_long)
print(levene_test)

# Perform an independent samples t-test assuming equal variances
t_test_result <- t.test(data$Control, data$Calhm2_KD, var.equal = TRUE)

# Print the result
print(t_test_result)

# Reshape the data to long format for plotting
data_long <- reshape2::melt(data)
colnames(data_long) <- c("Group", "Value")

# Create the bar chart with the updated y-axis label
p <- ggplot(data_long, aes(x = Group, y = Value)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue", width = 0.5) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  theme_minimal() +
  ylab("Luminescence (%control)") +  # Updated y-axis label
  xlab("Group") +
  ggtitle("Comparison of Control and Calhm2 KD")

# Calculate position for the p-value annotation
y_position <- max(data_long$Value) + 5
x_position <- mean(as.numeric(data_long$Group))

# Add the p-value and line
p <- p + geom_text(aes(x = x_position, y = y_position, label = paste("p =", formatC(0.001221, format = "e", digits = 2))), vjust = 0) +
  geom_segment(aes(x = 1.25, y = y_position - 2, xend = 1.75, yend = y_position - 2))

# Print the plot
print(p)
