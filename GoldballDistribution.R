# Importing required libraries
library(ggplot2) # For data visualization

## Goldball Distribution
##########################################################################

# Make Dataframe
data <- data.frame(
  Location = c("Outer Mito. Membrane", "Inner Boundary", "Inner Cristae Membrane", "Mito. Matrix"),
  Value = c(4.95, 24.79, 61.16, 9.09)
)

# Create the bar plot with specific colors for each bar and no legend
ggplot(data, aes(x = Location, y = Value, fill = Location)) +  # Specify x, y, and fill aesthetics
  geom_bar(stat = "identity") +  # Plot bars with the specified colors
  labs(y = "Immunogold Distribution (%)", x = "Mitochondrial Location") +  # Add labels to the axes
  theme_minimal() +  # Set a minimal theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        legend.position = "none") +  # Remove the legend
  scale_fill_manual(values = c("Outer Mito. Membrane" = "blue",  # Set specific colors for each bar
                               "Inner Boundary" = "red", 
                               "Inner Cristae Membrane" = "green", 
                               "Mito. Matrix" = "purple"))
# Optionally, saving the plot to a file
ggsave("goldball_dist.png", plot = p, width = 10, height = 6, dpi = 300)
