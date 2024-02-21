# Function to create data frame and add columns
create_data_frame <- function(mito_BFP, TMRM, group_name) {
  data_frame <- data.frame(Mito_BFP = mito_BFP, TMRM = TMRM)
  data_frame$Ratio <- data_frame$TMRM / data_frame$Mito_BFP
  data_frame$Group <- rep(group_name, nrow(data_frame))
  return(data_frame)
}

# Control group data
mito_BFP_control <- c(46.994, 40.621, 45.327, 70.910, 68.510, 31.711, 33.953, 42.847, 55.987)
TMRM_control <- c(129.581, 100.855, 128.825, 78.118, 74.997, 46.977, 58.729, 101.298, 91.983)
data_control <- create_data_frame(mito_BFP_control, TMRM_control, "Control")

# Calhm2 KD group data
mito_BFP_kd <- c(20.247, 23.367, 31.574, 69.132, 41.175, 31.914, 35.186, 43.041, 34.295, 43.039)
TMRM_kd <- c(13.931, 15.868, 17.837, 34.529, 27.622, 30.846, 16.022, 22.392, 26.922, 32.098)
data_kd <- create_data_frame(mito_BFP_kd, TMRM_kd, "Calhm2 KD")

# Combine the dataframes
combined_data <- rbind(data_control, data_kd)

# Statistical Analysis
# Shapiro-Wilk Normality Test for Ratio
shapiro_test_control_ratio <- shapiro.test(combined_data$Ratio[combined_data$Group == "Control"])
shapiro_test_kd_ratio <- shapiro.test(combined_data$Ratio[combined_data$Group == "Calhm2 KD"])

# Levene's Test for Homogeneity of Variances for Ratio
levene_test_ratio <- leveneTest(Ratio ~ Group, data = combined_data)

# Welch's t-test
welchs_test <- t.test(Ratio ~ Group, data = combined_data, var.equal = FALSE)

# Mann-Whitney U test
mann_whitney_test <- wilcox.test(Ratio ~ Group, data = combined_data, exact = FALSE)

# Print test results
print(shapiro_test_control_ratio)
print(shapiro_test_kd_ratio)
print(levene_test_ratio)
print(welchs_test)
print(mann_whitney_test)

# Data summarization and plotting
group_summary <- combined_data %>%
  group_by(Group) %>%
  summarise(
    Mean = mean(Ratio),
    LowerCI = Mean - qt(0.975, df=n()-1) * sd(Ratio) / sqrt(n()),
    UpperCI = Mean + qt(0.975, df=n()-1) * sd(Ratio) / sqrt(n())
  ) %>%
  mutate(Group = factor(Group, levels = c("Control", "Calhm2 KD")))

# Create the bar plot with updated y-axis label and no legend
p <- ggplot(group_summary, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("Control" = "skyblue", "Calhm2 KD" = "skyblue")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.9)) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  ) +
  labs(x = "Group", y = "Fluorescent Intensity\nBasal TMRE / Mito-BFP")

# Add annotations for p-value and comparison line
p_value_y_position <- 2.85
line_y_position <- 2.75
p <- p + annotate("text", x = 1.5, y = p_value_y_position, label = paste("p =", formatC(0.0004027, format = "e", digits = 2)))
p <- p + geom_segment(aes(x = 1, xend = 2, y = line_y_position, yend = line_y_position), color = "black")

# Print the plot
print(p)
