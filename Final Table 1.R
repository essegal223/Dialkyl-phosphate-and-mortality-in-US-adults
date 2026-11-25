#Run the following files first: "Final import, recode, weight, subset.R" THEN (for DAP variable statistics) "Final_Create creatinine adjusted quart.R"

#Purpose: Baseline characteristics of participants
library(survey)
#drop individuals with at least on DAP measurement
subset.elig.18.followed.nonzeroweight.creatinine.atleast1ex <- subset(
  subset.elig.18.followed.nonzeroweight.creatinine,
  !is.na(URXOP1) | !is.na(URXOP2) | !is.na(URXOP3) |
    !is.na(URXOP4) | !is.na(URXOP5) | !is.na(URXOP6)
)
table1=subset.elig.18.followed.nonzeroweight.creatinine.atleast1ex
dim(table1) 

# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design){
  # Get mean, stderr, and unweighted sample size
  c <- svyby(varformula, byformula, design, unwtd.count ) 
  p <- svyby(varformula, byformula, design, svymean ) 
  outSum <- left_join(select(c,-se), p) 
  outSum
  
}

###come back to this function, not sure how svymean work for categories##
getCI<-function(varformula, byformula, design){
  p <- svyby(varformula, byformula, design, svymean ) 
  CI<-confint(p)
  CI
}
#total pop
#continuous
# Run svymean() for each variable
mean1 <- svymean(~URXOP1_crt, analysis_URXOP1_crt.imputed.cont)
mean2 <- svymean(~URXOP2_crt, analysis_URXOP2_crt.imputed.cont)
mean3 <- svymean(~URXOP3_crt, analysis_URXOP3_crt.imputed.cont)
mean4 <- svymean(~URXOP4_crt, analysis_URXOP4_crt.imputed.cont)
mean5 <- svymean(~URXOP5_crt, analysis_URXOP5_crt.imputed.cont)
mean6 <- svymean(~URXOP6_crt, analysis_URXOP6_crt.imputed.cont)

# Combine into a table
result_table <- data.frame(
  variable = c("URXOP1_crt", "URXOP2_crt", "URXOP3_crt", "URXOP4_crt", "URXOP5_crt", "URXOP6_crt"),
  mean_se = c(
    sprintf("%.2f (%.2f)", coef(mean1), SE(mean1)),
    sprintf("%.2f (%.2f)", coef(mean2), SE(mean2)),
    sprintf("%.2f (%.2f)", coef(mean3), SE(mean3)),
    sprintf("%.2f (%.2f)", coef(mean4), SE(mean4)),
    sprintf("%.2f (%.2f)", coef(mean5), SE(mean5)),
    sprintf("%.2f (%.2f)", coef(mean6), SE(mean6))
  )
)
result_table
#test
svymean(~URXOP5_crt, analysis_URXOP5_crt.imputed.cont)

#average of continuous variables with no missing values
svymean(~RIDAGEYR, table1)
svymean(~permth_exm, table1)
svymean(~permth_exm+URXUCR, table1) #to get both at once

#among individuals with measured BMI, average BMI is:
svymean(~BMXBMI, BMI_table1) 

#proportion who died
svymean(~as.factor(mortstat), table1)
sum(table1$variables$mortstat==1)

#categorical 
# List of variables
vars <- c("sex", "race", "edu", "pir", "mvp_label", "smk_label", 
          "bmicat", "alcg_label", "diabetes_baseline", "hypertension", 
          "cholesterol_cat", "SDDSRVYR")

# Initialize empty list to store results
mean_list <- list()

# Loop over variables, calculate svymean, and print results
for (v in vars) {
  formula <- as.formula(paste0("~", v))
  result <- svymean(formula, table1)
  
  # Save result
  mean_list[[v]] <- result
  
  # Print result during loop
  cat("\nVariable:", v, "\n")
  print(result)
}


result_table <- bind_rows(
  lapply(names(mean_list), function(v) {
    est <- coef(mean_list[[v]])
    data.frame(
      variable = v,
      level = names(est),
      mean = round(est, 2),
      stringsAsFactors = FALSE
    )
  })
)
result_table %>% filter(variable!="bmicat") %>% select(mean) 
#test (these give same output because each level is a binary variable)
svymean(~pir, table1)
prop.table(svytable(~pir, table1))


#by mortality status
#number and percent of people with mortstat==0
sum(table1$variables$mortstat==0)

#number of people with mortstat==1
sum(table1$variables$mortstat==1)

#DAPs
getSummary(~as.factor(quartile), ~mortstat,analysis_URXOP1_crt.imputed.cont)
#mean DAP and se by mortality status
getSummary(~URXOP1_crt, ~mortstat,analysis_URXOP1_crt.imputed.cont)
getSummary(~URXOP2_crt, ~mortstat,analysis_URXOP2_crt.imputed.cont)
getSummary(~URXOP3_crt, ~mortstat,analysis_URXOP3_crt.imputed.cont)
getSummary(~URXOP4_crt, ~mortstat,analysis_URXOP4_crt.imputed.cont)
getSummary(~URXOP5_crt, ~mortstat,analysis_URXOP5_crt.imputed.cont)
getSummary(~URXOP6_crt, ~mortstat,analysis_URXOP6_crt.imputed.cont)

#mean DAP and se by age
analysis_URXOP2_crt.imputed.cont$variables=analysis_URXOP2_crt.imputed.cont$variables %>% 
  mutate(above50=case_when(
    RIDAGEYR>51~1,
    .default = 0
  ))
getSummary(~as.factor(quartile), ~above50,analysis_URXOP1_crt.imputed.cont)
getSummary(~as.factor(quartile), ~above50,analysis_URXOP2_crt.imputed.cont)
getSummary(~as.factor(quartile), ~above50,analysis_URXOP3_crt.imputed.cont)
getSummary(~as.factor(quartile), ~above50,analysis_URXOP4_crt.imputed.cont)
getSummary(~as.factor(quartile), ~above50,analysis_URXOP5_crt.imputed.cont)

getSummary(~above50, ~as.factor(quartile),analysis_URXOP1_crt.imputed.cont)
getSummary(~above50, ~as.factor(quartile),analysis_URXOP2_crt.imputed.cont)
getSummary(~above50, ~as.factor(quartile),analysis_URXOP3_crt.imputed.cont)
getSummary(~above50, ~as.factor(quartile),analysis_URXOP4_crt.imputed.cont)
getSummary(~above50, ~as.factor(quartile),analysis_URXOP5_crt.imputed.cont)
getSummary(~above50, ~as.factor(quartile),analysis_URXOP6_crt.imputed.cont)
#continuous (use getSummary means and SE by mortality status)
getSummary(~RIDAGEYR,~mortstat,table1)

getSummary(~URXUCR,~mortstat,table1)

getSummary(~permth_exm,~mortstat,table1)




#BMI continuous
BMI_table1 <- subset(table1, !is.na(BMXBMI))
getSummary(~BMXBMI,~mortstat,BMI_table1)


#categorical variables by mortality status
#columns add to 1, so this would be P(sex | mortstat)
getSummary(~sex,~mortstat,table1)
prop.table(svytable(~sex + mortstat, table1), margin = 2)


prop.table(svytable(~race + mortstat, table1), margin = 2)


prop.table(svytable(~edu + mortstat, table1), margin = 2)


prop.table(svytable(~pir + mortstat, table1), margin = 2)
getSummary(~pir, ~mortstat, table1)


prop.table(svytable(~mvp_label + mortstat, table1), margin = 2)


prop.table(svytable(~smk_label + mortstat, table1), margin = 2)


prop.table(svytable(~bmicat + mortstat, table1), margin = 2)


prop.table(svytable(~alcg_label + mortstat, table1), margin = 2)


prop.table(svytable(~diabetes_baseline+ mortstat, table1), margin = 2)


prop.table(svytable(~hypertension+ mortstat, table1), margin = 2)


prop.table(svytable(~cholesterol_cat+ mortstat, table1), margin = 2)


prop.table(svytable(~SDDSRVYR+ mortstat, table1), margin = 2)


#99-00 are 10 years younger than other populations
table1$variables %>%
  group_by(SDDSRVYR) %>%
  summarise(mean = mean(RIDAGEYR, na.rm = TRUE))



library(survey)
library(dplyr)
# Function to extract formatted table 1 info with rounding
extract_table1_formatted <- function(var, design, outcome = "mortstat") {
  tab <- prop.table(svytable(as.formula(paste0("~", var, "+", outcome)), design), margin = 2)
  chi <- svychisq(as.formula(paste0("~", var, "+", outcome)), design)
  pval <- format.pval(chi$p.value, digits = 3, eps = 0.001)
  
  df <- as.data.frame.matrix(tab)
  df$Level <- rownames(df)
  df <- df %>%
    rename(Alive = `0`, Death = `1`) %>%
    mutate(across(c(Alive, Death), ~round(.x, 2))) %>%
    select(Level, Alive, Death)
  
  # Add the variable name as a header row with p-value
  header <- data.frame(
    Level = var,
    Alive = NA,
    Death = NA,
    p_value = pval,
    stringsAsFactors = FALSE
  )
  
  # Add p_value column to df with empty values for levels
  df$p_value <- ""
  
  # Combine header and data
  out <- bind_rows(header, df)
  return(out)
}


# Function for continuous variables
extract_cont_summary <- function(var, label, design, outcome = "mortstat") {
  means <- svyby(as.formula(paste0("~", var)), as.formula(paste0("~", outcome)), design, svymean, na.rm = TRUE)
  model <- svyglm(as.formula(paste0(var, "~", outcome)), design = design)
  pval <- format.pval(summary(model)$coefficients[2, 4], digits = 3, eps = 0.001)
  
  df <- data.frame(
    Level = label,
    Alive = round(means[means[[outcome]] == 0, 2], 2),
    Death = round(means[means[[outcome]] == 1, 2], 2),
    p_value = pval,
    stringsAsFactors = FALSE
  )
  return(df)
}

# Variables
cat_vars <- c("sex", "race", "edu", "pir", "mvp_label", "smk_label", 
              "bmicat", "alcg_label", "diabetes_baseline", "hypertension", 
              "cholesterol_cat", "SDDSRVYR")

# Get table for categorical variables
table1_cats <- lapply(cat_vars, extract_table1_formatted, design = table1) %>%
  bind_rows()

# Continuous variable summaries
cont_vars <- list(
  list(var = "RIDAGEYR", label = "Age (years)"),
  list(var = "URXUCR", label = "Creatinine concentration"),
  list(var = "permth_exm", label = "Follow-up (months)")
)

table1_conts <- lapply(cont_vars, function(x) extract_cont_summary(x$var, x$label, design = table1)) %>%
  bind_rows()

# Combine full Table 1
table1_final <- bind_rows(table1_conts, table1_cats)

# View result
View(table1_final)

