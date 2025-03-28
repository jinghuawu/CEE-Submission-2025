# Load necessary libraries
library(readxl)   # For reading Excel files
library(writexl)  # For writing Excel files
library(mice)     # For multiple imputation
library(dplyr)    # For data manipulation

# Set working directory
setwd(dir = 'G:/RData')  # Set the directory where your data is located
getwd()  # Check the current working directory

# Read and inspect the data
mydata <- read_excel("CEE.xlsx")  # Read the Excel file into a data frame
summary(mydata)  # Display a summary of the data

# Perform multiple imputation, 5 times, using Random Forest (rf) method
set.seed(2025)  # Set seed for reproducibility
imp <- mice(mydata, m = 5, meth = 'rf')  # Apply multiple imputation

# Retrieve the 5 imputed datasets
imputed_data_list <- lapply(1:5, function(i) complete(imp, action = i))

# Calculate the average of numeric columns across all imputations
average_numeric_data <- Reduce("+", lapply(imputed_data_list, function(df) df[, sapply(df, is.numeric)])) / 5

# Combine the non-numeric columns with the average numeric columns
average_imputed_data <- imputed_data_list[[1]]  # Use the first imputation as the base
average_imputed_data[, sapply(average_imputed_data, is.numeric)] <- average_numeric_data

# Display a summary of the average imputed data
summary(average_imputed_data)

# Save the 5 imputed datasets and the average imputed dataset to an Excel file
write_xlsx(
  list(
    Average = average_imputed_data,  # Save the average imputed data
    Imputation1 = imputed_data_list[[1]],
    Imputation2 = imputed_data_list[[2]],
    Imputation3 = imputed_data_list[[3]],
    Imputation4 = imputed_data_list[[4]],
    Imputation5 = imputed_data_list[[5]]
  ),
  "CEE_imputed_results.xlsx"  # Save the output to a file
)

###### Compute Element Ratios ######
df <- read_excel("CEE_imputed_results.xlsx")  # Load the imputed results

# Use mutate to create new columns for various element ratios
df <- df %>%
  mutate(
    ZrHf = Zr / Hf,  # Zr/Hf ratio
    NbTa = Nb / Ta,  # Nb/Ta ratio
    RbSr = Rb / Sr,  # Rb/Sr ratio
    VSc = V / Sc,    # V/Sc ratio
    ThU = Th / U,    # Th/U ratio
    GaAl = Ga / (Al * 0.5293),  # Ga/Al ratio, with correction factor for Al
    ACNK = (Al / 101.96) / ((Ca / 56.08) + (Na / 61.98) + (K / 94.20)),  # ACNK index
    TE13 = ((((Ce / 0.612) / ((La / 0.237)^(2/3) * (Nd / 0.467)^(1/3))) * ((Pr / 0.095) / ((La / 0.237)^(1/3) * (Nd / 0.467)^(2/3))))^0.5) *
      ((((Tb / 0.0374) / ((Gd / 0.2055)^(2/3) * (Ho / 0.0566)^(1/3))) * ((Dy / 0.2540) / ((Gd / 0.2055)^(1/3) * (Ho / 0.0566)^(2/3))))^0.5),  # TE13 calculation
    LREE = La + Ce + Pr + Nd + Sm + Eu + Gd,  # Light Rare Earth Elements (LREE)
    HREE = Tb + Dy + Ho + Er + Tm + Yb + Lu,  # Heavy Rare Earth Elements (HREE)
    REE = La + Ce + Pr + Nd + Sm + Eu + Gd + Tb + Dy + Ho + Er + Tm + Yb + Lu,  # Total Rare Earth Elements (REE)
    LRHR = (La + Ce + Pr + Nd + Sm + Eu + Gd) / (Tb + Dy + Ho + Er + Tm + Yb + Lu),  # LREE/HREE ratio
    LaYbN = (La / 0.237) / (Yb / 0.17),  # La/Yb normalized ratio
    d_Ce = (Ce / 0.612) / (((La / 0.237) * (Pr / 0.095))^0.5),  # Ce/Ce* normalized
    d_Eu = (Eu / 0.058) / (((Sm / 0.153) * (Gd / 0.2055))^0.5)  # Eu/Eu* normalized
  )

# Print the resulting data frame
print(df)

# Save the modified data frame back to an Excel file
write_xlsx(df, "CEE_imputed_results_modified.xlsx")  # Save the modified data to a new file