#Run the following files first: "Final import, recode, weight, subset.R" THEN "Final Spearman Wrangling": run dataset spearman.data.completecases 
#spearman correlation

sum(complete.cases(spearman.data.completecases$variables[, c("URXOP1_crt", "URXOP2_crt", "URXOP3_crt", "URXOP4_crt", "URXOP5_crt", "URXOP6_crt")]))
#12446/12966 complete cases for all 6 exposures


##all cycles: Plot Spearman correlation on the log-transformed data

# Compute Spearman correlation matrix
#create this variable
#get index for creatine-adjusted exposures
vars <- c("URXOP1_crt", "URXOP2_crt", "URXOP3_crt", "URXOP4_crt", "URXOP5_crt", "URXOP6_crt")

# Find the column indexes for these variables in the dataset
col_indexes <- which(names(spearman.data.completecases$variables) %in% vars)

# Print the indexes
print(col_indexes)
log_vars <- log(spearman.data.completecases$variables[, col_indexes])

# Rename columns
new_names <- c("DMP", "DEP", "DMTP", "DETP", "DMTDP", "DETDP")
colnames(log_vars) <- paste0("log ", new_names)

# Compute Spearman correlation matrix
cor_mat <- cor(log_vars, method = "spearman", use = "complete.obs")

# Plot the correlation heatmap
ggcorrplot(cor_mat, 
           title = "Spearman Correlations of Log-Transformed Creatinine-adjusted DAPs",
           lab = TRUE, 
           lab_size = 3, 
           colors = c("#6D9EC1", "white", "#E46726"),
           show.legend = TRUE)

