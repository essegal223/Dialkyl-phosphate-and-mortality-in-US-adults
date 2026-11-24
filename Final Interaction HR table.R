#Run the following files first: "Final import, recode, weight, subset.R" THEN "Final_Create creatinine adjusted quart.R" THEN "Final Crtadj all interactions clean.R"
#Note: these stored models correspond to those ran in "Final Crtadj all interactions clean.R" using the desired exposure dataset variable

#purpose of file: create HR and CI table to then use to create plot

#What's completed in this file:
#quartile associations in wide format
#Quaritle associations in long format to be used in "Interaction plot code.rmd".


library(dplyr)
library(tidyr)
library(purrr)


# Named model list
models <- list(
  All_cause_bc = birth.int.fit2.m,
  All_cause_smk = smk.int.fit2.m,
  CVD_bc = birth.int.fit2.cvd,
  CVD_smk = smk.int.fit2.cvd,
  Cancer_bc = birth.int.fit2.c,
  Cancer_smk = smk.int.fit2.cancer
)

# Define target interaction terms
bc_terms <- c(
  "approx.birthcohort_baseline1910-1950:as.factor(quartile)2",
  "approx.birthcohort_baseline1910-1950:as.factor(quartile)3",
  "approx.birthcohort_baseline1910-1950:as.factor(quartile)4",
  "approx.birthcohort_baseline1951-2000:as.factor(quartile)2",
  "approx.birthcohort_baseline1951-2000:as.factor(quartile)3",
  "approx.birthcohort_baseline1951-2000:as.factor(quartile)4"
)

smk_terms <- c(
  "smk_labelnon smoker:as.factor(quartile)2",
  "smk_labelnon smoker:as.factor(quartile)3",
  "smk_labelnon smoker:as.factor(quartile)4",
  "smk_labelpast smoker:as.factor(quartile)2",
  "smk_labelpast smoker:as.factor(quartile)3",
  "smk_labelpast smoker:as.factor(quartile)4",
  "smk_labelcurrent smoker:as.factor(quartile)2",
  "smk_labelcurrent smoker:as.factor(quartile)3",
  "smk_labelcurrent smoker:as.factor(quartile)4",
  "smk_labelMissing:as.factor(quartile)2",
  "smk_labelMissing:as.factor(quartile)3",
  "smk_labelMissing:as.factor(quartile)4"
)

# Function to extract and format interaction terms only
extract_interactions <- function(model, model_name) {
  ci <- confint(model)
  results <- exp(cbind(HR = coef(model), ci))
  results_df <- as.data.frame(results)
  results_df <- round(results_df, 2)
  results_df$`HR (95% CI)` <- paste0(results_df$HR, " (", results_df$`2.5 %`, ", ", results_df$`97.5 %`, ")")
  
  # Choose terms based on model type
  if (grepl("_bc$", model_name)) {
    keep_terms <- bc_terms
  } else if (grepl("_smk$", model_name)) {
    keep_terms <- smk_terms
  } else {
    return(NULL)
  }
  
  results_df <- results_df[rownames(results_df) %in% keep_terms, , drop = FALSE]
  results_df$Term <- rownames(results_df)
  results_df <- results_df[, c("Term", "HR (95% CI)")]
  colnames(results_df)[2] <- model_name
  return(results_df)
}

#extract_interactions(models$All_cause_bc, "All_cause_bc")


# Extract results and merge
results_list <- imap(models, extract_interactions)
combined_long <- reduce(results_list, full_join, by = "Term")

# Pivot longer and add columns for Quartile and Stratification (run this code 3 times to create DMP, DMTP, and DETP tables)
final_long_DETP <- pivot_longer(combined_long, cols = -Term, names_to = "Model", values_to = "HR_CI") %>%
  filter(!is.na(HR_CI)) %>%
  mutate(
    Quartile = str_extract(Term, "quartile\\)([2-4])") %>% str_remove("quartile\\)") %>% as.integer(),
    Stratification = case_when(
      str_detect(Term, "birthcohort") ~ str_extract(Term, "1910-1950|1951-2000"),
      str_detect(Term, "smk_label") ~ str_extract(Term, "non smoker|past smoker|current smoker|Missing"),
      TRUE ~ NA_character_
    ),
    Variable = case_when(
      str_detect(Term, "birthcohort") ~ "Birth Cohort",
      str_detect(Term, "smk_label") ~ "Smoking Status",
      TRUE ~ NA_character_
    ), 
    Metabolite="DETP"
  )%>%
  group_by(Model)

View(final_long_DETP)

#combine metabolites into one long table

final_long_all_metabolites=bind_rows(final_long_DMP, final_long_DMTP, final_long_DETP)
final_long_all_metabolites=final_long_all_metabolites |> select(HR_CI, Quartile, Stratification, Variable, Metabolite)
View(final_long_all_metabolites)

           
    
  
write.csv(final_long_all_metabolites, "Coauthor edits/Submission to Environmental Insights Journal/Tracking revisions/Code for revisions/Interaction Table_long.csv")

#convert into wide format
# Define custom model order
model_order <- c(
  "All_cause_bc", "All_cause_smk",
  "CVD_bc", "CVD_smk",
  "Cancer_bc", "Cancer_smk"
)

# Reorder and pivot to wide format
final_wide_DETP <- final_long_DETP  %>%
  ungroup() %>%
  mutate(
    Model = factor(Model, levels = model_order),
    Quartile = paste0("Q", Quartile)
  ) %>%
  dplyr::select(Model, Variable, Stratification, Quartile, HR_CI) %>%
  pivot_wider(
    names_from = Quartile,
    values_from = HR_CI
  ) %>%
  arrange(Model)

print(final_wide_DETP)
