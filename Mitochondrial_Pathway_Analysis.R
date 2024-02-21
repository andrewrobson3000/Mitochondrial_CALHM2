# Load necessary libraries
library(ggplot2)  # For data visualization

# IPA Analysis: Identifying Mitochondrial-Related Pathways
#####################################################################

# Creating vectors for each column: diseases/functions and their corresponding p-values
diseases_and_functions <- c(
  "Mitochondrial trifunctional protein deficiency",
  "inflammation of hippocampus",
  "beta-oxidation of fatty acid",
  "Leigh syndrome",
  "replication of Moloney murine leukemia",
  "dilated cardiomyopathy type 1T",
  "accumulation of suberic acid",
  "depletion of L-arginine",
  "accumulation of C10:1 dicarboxylic acid",
  "activation of 5,6-dichloro-1-beta-D-ribofuranosylbenzimidazole",
  "dysfunction of cerebellum",
  "autosomal recessive spastic ataxia type",
  "hyperargininemia",
  "dark cell degeneration of purkinje cells",
  "hydrolysis of L-arginine",
  "accumulation of sebacic acid",
  "early infantile epileptic encephalopathy",
  "formation of inner nuclear membrane",
  "early myoclonic encephalopathy",
  "homeostasis of L-arginine",
  "accumulation of myristic acid",
  "spinocerebellar ataxia type 28",
  "French-Canadian type Leigh syndrome",
  "deficiency of long-chain 3-hydroxyacyl-CoA dehydrogenase",
  "accumulation of C14:1 fatty acid",
  "mitochondrial respiratory chain deficiency",
  "mitochondrial myopathy",
  "organization of mitochondria",
  "transport of glutamine family amino acid",
  "abnormal morphology of hepatocytes"
)

p_values <- c(
  2.47E-07, 4.21E-05, 4.66E-05, 1.72E-04, 5.14E-04, 5.14E-04, 5.14E-04,
  5.14E-04, 5.14E-04, 5.14E-04, 5.14E-04, 5.14E-04, 5.14E-04, 5.14E-04,
  5.14E-04, 5.14E-04, 5.14E-04, 5.14E-04, 5.14E-04, 5.14E-04, 5.14E-04,
  5.14E-04, 5.14E-04, 5.14E-04, 5.14E-04, 7.63E-04, 8.02E-04, 8.81E-04,
  9.23E-04, 9.86E-04
)

# Combining vectors into a dataframe
dataframe <- data.frame(
  DiseasesAndFunctions = diseases_and_functions, 
  PValue = p_values,
  stringsAsFactors = FALSE  # Avoid converting strings to factors
)

# Calculate -log10(p-value) for better visualization of significance
dataframe$LogPValue <- -log10(dataframe$PValue)

# Initial Plotting: Horizontal Bar Chart of Significance Levels
#####################################################################
ggplot(dataframe, aes(x = reorder(DiseasesAndFunctions, -LogPValue), y = LogPValue)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Makes the bar chart horizontal
  xlab("Diseases and Functions") +
  ylab("-log10(p-value)") +
  theme_minimal()  # Uses a minimal theme for a cleaner look

# Filtering Data for Mitochondrial-Related Pathways
#####################################################################
# Selecting entries related to mitochondrial pathways based on prior analysis
mito_related_terms <- c(
  "Mitochondrial trifunctional protein deficiency",
  "inflammation of hippocampus",
  "beta-oxidation of fatty acid",
  "Leigh syndrome",
  "dilated cardiomyopathy type 1T",
  "accumulation of suberic acid",
  "depletion of L-arginine",
  "accumulation of C10:1 dicarboxylic acid",
  "dark cell degeneration of purkinje cells",
  "hydrolysis of L-arginine",
  "accumulation of sebacic acid",
  "early infantile epileptic encephalopathy",
  "spinocerebellar ataxia type 28",
  "French-Canadian type Leigh syndrome",
  "deficiency of long-chain 3-hydroxyacyl-CoA dehydrogenase",
  "accumulation of C14:1 fatty acid",
  "mitochondrial respiratory chain deficiency",
  "mitochondrial myopathy",
  "organization of mitochondria",
  "transport of glutamine family amino acid"
)

# Filtering the original dataframe for entries that are mitochondrial-related
mito_linked_dataframe <- dataframe[dataframe$DiseasesAndFunctions %in% mito_related_terms, ]

# Final Visualization: Enhanced Horizontal Bar Chart
#####################################################################
# Creating a more detailed horizontal bar chart with added aesthetics for mitochondrial-related pathways
ggplot(mito_linked_dataframe, aes(x = reorder(DiseasesAndFunctions, -LogPValue), y = LogPValue, fill = LogPValue)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flips the axes to make the chart horizontal
  scale_fill_gradient(low = "lavender", high = "darkmagenta") +  # Adds a color gradient based on significance
  xlab("Diseases and Functions") +
  ylab(expression(-log[10](p-value))) +  # Uses mathematical notation for y-label
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +  # Significance threshold line
  annotate("text", x = Inf, y = -log10(0.05), label = " Significance threshold (p < 0.05)", 
           hjust = 1.1, vjust = -0.5, color = "red", size = 3) +
  theme_minimal() +  # Minimal theme for a clean appearance
  theme(legend.position = "none") +  # Removes the legend to clean up the plot
  ggtitle("Mitochondrial Pathways Analysis")  # Adds a title to the plot

# Note: The significance threshold line is drawn at -log10(0.05) to visually indicate which findings are considered statistically significant.

# Optionally, save the enhanced plot to a file
ggsave("mitochondrial_pathways_analysis_enhanced.png", width = 10, height = 8, dpi = 300)
