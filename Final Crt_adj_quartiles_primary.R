#Run the following files first: "Final import, recode, weight, subset.R" THEN "Final_Create creatinine adjusted quart.R"

#Purpose of this file: To conduct creatinine adjusted DAP quartile versus mortality analysis

#What is completed in this file#
#create CVD- and cancer specific mortality outcomes
#create Cox models with all-cause, cvd, cancer mortality as outcomes
  #first 3 models are adjusted for lifestyle behaviors and demographics
  #last 3 models are adjusted for above + comorbidities
  #total models: 6


#Notes
#run find and replace for URXOP# to run different exposure dataset with quartile  and other variables
#to get estimates for each exposure
#URXOP# corresponding DAP are found in "Final import, recode, weight, subset.R"


dim(analysis_URXOP6_crt.imputed.cont) 
#note there is no URXOP6 variable so you don't have to worry about this error
#with crt_adjusted exposure: 
#DAP 1: 12798
#DAP 2: 12801 
#DAP3: 12817 
#DAP4: 12755 
#DAP5: 12802 
#DAP6: 12690

#create specific mortality outcome variables

#wrangling
table(analysis_URXOP6_crt.imputed.cont$variables$ucod_leading, useNA="always")
#death due to CVD vs all other people; create CVD_death variable
analysis_URXOP6_crt.imputed.cont$variables  <- analysis_URXOP6_crt.imputed.cont$variables %>%
  mutate(CVD_death = case_when(
    ucod_leading==1~1,
    TRUE ~ 0
  ))

table(analysis_URXOP6_crt.imputed.cont$variables$CVD_death, useNA = "always")

CVD_dead_by_quartile <- analysis_URXOP6_crt.imputed.cont$variables %>%  
  group_by(quartile) %>%                     # Group by quartile
  summarise(
    deaths = sum(CVD_death == 1),  # Count deaths where mortstat == 1
    total  = n() # Count total rows
  )

CVD_dead_by_quartile

#death due to cancer vs all other people; create cancer_death variable
analysis_URXOP6_crt.imputed.cont$variables  <- analysis_URXOP6_crt.imputed.cont$variables %>%
  mutate(cancer_death = case_when(
    ucod_leading==2~1,
    TRUE ~ 0
  ))


cancer_dead_by_quartile <- analysis_URXOP6_crt.imputed.cont$variables %>%  
  group_by(quartile) %>%                     # Group by quartile
  summarise(
    deaths = sum(cancer_death == 1),  # Count deaths where mortstat == 1
    total  = n() # Count total rows
  )

cancer_dead_by_quartile
table(analysis_URXOP6_crt.imputed.cont$variables$cancer_death, useNA = "always")
sum(is.na(analysis_URXOP6_crt.imputed.cont$variables$cancer_death))

#total cases for each outcome
sum(analysis_URXOP6_crt.imputed.cont$variables$mortstat == 1)
sum(analysis_URXOP6_crt.imputed.cont$variables$CVD_death == 1)
sum(analysis_URXOP6_crt.imputed.cont$variables$cancer_death == 1)

#fully adjusted
model_URXOP6_quartile_adj <- svycoxph(
  formula = Surv(permth_exm, mortstat) ~ as.factor(quartile)+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label,  
  design = analysis_URXOP6_crt.imputed.cont
)


summary(model_URXOP6_quartile_adj)


#CVD model

CVD_model_URXOP6_quartile_adj <- svycoxph(
  formula = Surv(permth_exm, CVD_death) ~ as.factor(quartile)+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label, 
  design = analysis_URXOP6_crt.imputed.cont
)

summary(CVD_model_URXOP6_quartile_adj)


#cancer model
cancer_model_URXOP6_quartile_adj <- svycoxph(
  formula = Surv(permth_exm, cancer_death) ~ as.factor(quartile)+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label,
  design = analysis_URXOP6_crt.imputed.cont
)

summary(cancer_model_URXOP6_quartile_adj)



#quartile as predictor end for minimal covariate adjustment


#Model 2 (+adjustments for diabetes, total cholesterol, hypertension)
model2_URXOP6_quartile_adj <- svycoxph(
  formula = Surv(permth_exm, mortstat) ~ as.factor(quartile)+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label+diabetes_baseline+cholesterol_cat+hypertension,
  design = analysis_URXOP6_crt.imputed.cont
)


summary(model2_URXOP6_quartile_adj)

CVD_model2_URXOP6_quartile_adj <- svycoxph(
  formula = Surv(permth_exm, CVD_death) ~ as.factor(quartile)+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label+diabetes_baseline+cholesterol_cat+hypertension,
  design = analysis_URXOP6_crt.imputed.cont
)

summary(CVD_model2_URXOP6_quartile_adj)

cancer_model2_URXOP6_quartile_adj <- svycoxph(
  formula = Surv(permth_exm, cancer_death) ~ as.factor(quartile)+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label+diabetes_baseline+cholesterol_cat+hypertension,
  design = analysis_URXOP6_crt.imputed.cont
)

summary(cancer_model2_URXOP6_quartile_adj)
