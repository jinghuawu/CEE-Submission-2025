# Load required R packages
library(readxl)  # For reading Excel files
library(writexl)  # For writing Excel files
library(Rtsne)  # For performing t-SNE (t-Distributed Stochastic Neighbor Embedding)
library(ggplot2)  # For creating plots

# Set working directory
setwd(dir = 'G:/RData')  # Set the directory to read/write files
getwd()  # Get the current working directory

# Load the data from an Excel file
data <- read_excel("CEE_imputed_results_modified.xlsx")  # Read the Excel file
# Subset data by removing rows where 'Group' is "Fertile" and 'ZrHf' is greater than 25
data <- subset(data, !(Group == "Fertile" & ZrHf > 25))

# Perform preliminary analysis of the data
str(data)  # Display the structure of the dataset
dim(data)  # Display the dimensions (rows and columns) of the dataset
View(data)  # View the data in a tabular format

# Select features (exclude the first two columns which are labels)
features <- data[, c(-1, -2)]  # Remove the first two columns (labels)

# Train the t-SNE model
set.seed(2025)  # Set the random seed for reproducibility (can be any number)
tsne <- Rtsne(as.matrix(features), check_duplicates = FALSE, pca = TRUE, 
              perplexity = 50,  # Perplexity should be less than the number of rows divided by 3
              theta = 0.0,      # Weight between speed and accuracy in computation (0 to 1, closer to 0 is more accurate)
              dims = 2)         # Reduce to 2 dimensions (for visualization)

# Convert the t-SNE results to a data frame
result <- as.data.frame(tsne$Y)  # Extract the 2D results from the t-SNE output
# Add the 'Group' information to the result data frame
result$Group <- data$Group

# Check the number of unique categories in 'Group' (should be 9)
length(unique(result$Group))
# Define a color palette with 9 colors (one for each category)
mainPalette <- rainbow(9)

# Plot the t-SNE results
ggplot(result, aes(x = V1, y = V2, color = Group)) +
  geom_point(shape = 'circle', size = 1) +  # Plot the points with circles of size 1
  labs(title = "t-SNE plotting",  # Add title to the plot
       x = "tSNE_DIM1",           # Label for the x-axis
       y = "tSNE_DIM2") +         # Label for the y-axis
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  stat_ellipse(aes(color = Group),  # Add ellipses to show the spread of data
               level = 0.95, linetype = 1, show.legend = FALSE) +  # 95% confidence level ellipse
  stat_ellipse(aes(color = Group),  # Add another ellipse with a different confidence level
               level = 0.68, linetype = 1, show.legend = FALSE)   # 68% confidence level ellipse

# Export the result to an Excel file
write_xlsx(result, "G:/RData/tSNE.xlsx")  # Save the result data to an Excel file