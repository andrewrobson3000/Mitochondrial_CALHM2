# Load necessary libraries for data manipulation and visualization
library(ggplot2)
library(dplyr)

# FIG. 3C: Data Preparation and Analysis for Experiment 1 and Experiment 2

# Experiment 1 Data Preparation
# -----------------------------------------------------------------------------------------
# Data for Experiment 1 - Includes groups, Calhm2 and VDAC protein levels, their ratio, and comparison metrics
data_exp1 <- data.frame(
  Group = c("Control", "KO"),  # Define groups: Control and Knock-Out (KO)
  Calhm2 = c(40458.522, 17014.823),  # Calhm2 protein levels
  VDAC = c(26086.128, 34583.22),  # VDAC protein levels
  Ratio = c(1.550959268, 0.491996494),  # Ratio of Calhm2 to VDAC
  Comparison = c(1, 0.317220771)  # Comparison metric, control set to 1
)

# Data for Experiment 2 - Similar structure with different values
data_exp2 <- data.frame(
  Group = c("Control", "KO"),
  Calhm2 = c(38915.25, 23839.037),
  VDAC = c(38892.108, 42280.43),
  Ratio = c(1.000595031, 0.56383147),
  Comparison = c(1, 0.563496172)
)

# Display Experiment 1 and 2 Data
print("Experiment 1 Data:")
print(data_exp1)
print("Experiment 2 Data:")
print(data_exp2)

# -----------------------------------------------------------------------------------------
data_exp1_adjusted <- data.frame(
  Group = c("Control", "KO"),
  Calhm2 = c(40458.522, 29074.258),
  VDAC = c(26086.128, 34583.22),
  Ratio = c(1.550959268, 0.840704191),
  Comparison = c(1, 0.542054333)
)

# Calculation of Averages for Western Blot Data
# -----------------------------------------------------------------------------------------
# Western Blot Data for HADHA protein in different cellular fractions
data_western_blot <- data.frame(
  WCL = c("HADHA", 99.93911469, 107.5550996, 117.1357538),
  CYTOSOLIC = c("HADHA", 445.7726813, 162.1464464, 185.5834081),
  MITO = c("HADHA", 55.79397158, 41.19718777, 19.38110563)
)

# Remove row names for clarity
rownames(data_western_blot) <- NULL

# Calculate the mean for each protein fraction
averages <- colMeans(data_western_blot[-1,], na.rm = TRUE)

# Append the calculated averages to the original dataset
data_western_blot <- rbind(data_western_blot, c("Averages", averages))

# Print the updated dataset with averages
print("Western Blot Data with Averages:")
print(data_western_blot)

# Additional Experiment Data Demonstrating Different Calculation Methods
# -----------------------------------------------------------------------------------------
# Data for Experiment 1 with different parameters
data_exp1_variant <- data.frame(
  A = c(33948.241, 6560.886),
  B = c(36795.149, 11431.551),
  VDAC = c(26086.128, 34583.22),
  Ratio = c(1.30139057, 0.189712988),
  Comparison = c(1, 0.145777134)
)

# Data for Experiment 2 with different parameters
data_exp2_variant <- data.frame(
  A = c(37943.158, 5593.167),
  VDAC = c(36372.087, 27290.894),
  Ratio = c(37857.007, 21711.765),
  Comparison = c(1.002275695, 0.257609964)
)

# Display Variant Experiment Data
print("Experiment 1 Variant Data:")
print(data_exp1_variant)
print("Experiment 2 Variant Data:")
print(data_exp2_variant)
