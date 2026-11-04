#Run the following files first: "Final import, recode, weight, subset.R" THEN "Final_Create creatinine adjusted quart.R" 
#p for trend values correspond with HRs and CIs in "Final Crt_adj_quartiles_primary.R"

#Purpose of this file: To compute primary analysis p for trend values

#What is completed in this file#
#Fit each model in "Final Crt_adj_quartiles_primary.R" with quartile as continuous variable
#model adjusted for demographics and lifestyle factors
#model2 adjusted for demographics and lifestyle factors+diabetes+high cholesterol+hypertension
#total models: 6
model_URXOP6_quartile_adj_pval <- svycoxph(
  formula = Surv(permth_exm, mortstat) ~ quartile+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label,  
  design = analysis_URXOP6_crt.imputed.cont
)


summary(model_URXOP6_quartile_adj_pval)


model2_URXOP6_quartile_adj_pval <- svycoxph(
  formula = Surv(permth_exm, mortstat) ~ quartile+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label+diabetes_baseline+cholesterol_cat+hypertension,
  design = analysis_URXOP6_crt.imputed.cont
)


summary(model2_URXOP6_quartile_adj_pval)


CVD_model_URXOP6_quartile_adj_pval <- svycoxph(
  formula = Surv(permth_exm, CVD_death) ~ quartile+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label, 
  design = analysis_URXOP6_crt.imputed.cont
)

summary(CVD_model_URXOP6_quartile_adj_pval)

CVD_model2_URXOP6_quartile_adj_pval <- svycoxph(
  formula = Surv(permth_exm, CVD_death) ~ quartile+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label+diabetes_baseline+cholesterol_cat+hypertension,
  design = analysis_URXOP6_crt.imputed.cont
)

summary(CVD_model2_URXOP6_quartile_adj_pval)

cancer_model_URXOP6_quartile_adj_pval <- svycoxph(
  formula = Surv(permth_exm, cancer_death) ~ quartile+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label,
  design = analysis_URXOP6_crt.imputed.cont
)

summary(cancer_model_URXOP6_quartile_adj_pval)

cancer_model2_URXOP6_quartile_adj_pval <- svycoxph(
  formula = Surv(permth_exm, cancer_death) ~ quartile+ RIDAGEYR+sex+URXUCR + race+edu+pir+mvp_label+bmicat+smk_label+SDDSRVYR+alcg_label+diabetes_baseline+cholesterol_cat+hypertension,
  design = analysis_URXOP6_crt.imputed.cont
)

summary(cancer_model2_URXOP6_quartile_adj_pval)
