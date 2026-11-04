#Run the following files first: "Final import, recode, weight, subset.R" THEN "Final_Create creatinine adjusted quart.R" 
#p-heterogeneity values correspond with HRs and CIs for smoking interaction models in "Final Crtadj all interactions clean.R" file

#Purpose of this file: 
#compute P-heterogeneity values by jointly testing the interaction terms between past and current smokers (nonsmokers as reference) and ordinal DAP quartile variable using Wald Chi-squared tests
#total models: 3
#control F URXOP# and replace for next exposure to get estimates for each exposure
#all cause
smk.pval.fit3.m <- svycoxph(
  formula = Surv(permth_exm, mortstat) ~ quartile+smk_label+quartile*smk_label+ RIDAGEYR+URXUCR+sex + race+edu+pir+mvp_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)


summary(smk.pval.fit3.m)

#joint test for current and past smokers
library(aod)
# Get the coefficient names
coef_names <- names(coef(smk.pval.fit3.m))

# Find the indices of the terms you're interested in
terms_idx <- which(coef_names %in% c("quartile:smk_labelpast smoker", "quartile:smk_labelcurrent smoker"))

# Run the Wald test (Chi squared test, because no df reported)
wald.test(b = coef(smk.pval.fit3.m), Sigma = vcov(smk.pval.fit3.m), Terms = terms_idx)


#CVD
smk.pval.fit3.cvd <- svycoxph(
  formula = Surv(permth_exm, CVD_death) ~ quartile+smk_label+quartile*smk_label+ RIDAGEYR+URXUCR+sex + race+edu+pir+mvp_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)

summary(smk.pval.fit3.cvd)

# Get the coefficient names
coef_names <- names(coef(smk.pval.fit3.cvd))

# Find the indices of the terms you're interested in
terms_idx <- which(coef_names %in% c("quartile:smk_labelpast smoker", "quartile:smk_labelcurrent smoker"))

# Run the Wald test
wald.test(b = coef(smk.pval.fit3.cvd), Sigma = vcov(smk.pval.fit3.cvd), Terms = terms_idx)


smk.pval.fit3.cancer <- svycoxph(
  formula = Surv(permth_exm, cancer_death) ~ quartile+smk_label+quartile*smk_label+ RIDAGEYR+URXUCR+sex + race+edu+pir+mvp_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)


summary(smk.pval.fit3.cancer)

# Run the Wald test
wald.test(b = coef(smk.pval.fit3.cancer), Sigma = vcov(smk.pval.fit3.cancer), Terms = c(33, 34))
