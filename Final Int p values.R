#Run the following files first: "Final import, recode, weight, subset.R" THEN "Final_Create creatinine adjusted quart.R" 
#p-heterogeneity values correspond with HRs and CIs for birth cohort interaction models in "Final Crtadj all interactions clean.R" file

#purpose of this file: compute p values for the interaction term between ordinal quartile variable and birth cohort (birth cohort 1910-1950 as reference)
#also, compute p values for the interaction term between ordinal quartile variable and past smoking versus nonsmoking, and ordinal quartile variable and current smoking versus nonsmoking

#control F URXOP# and replace for next exposure to get estimates for each exposure

#birth cohort p values
birth.pval.fit3.m <- svycoxph(
  formula = Surv(permth_exm, mortstat) ~ quartile*approx.birthcohort_baseline+RIDAGEYR+URXUCR+ sex + race+edu+pir+mvp_label+smk_label+bmicat+SDDSRVYR+alcg_label,    
  design = analysis_URXOP1_crt.imputed.cont
)

summary(birth.pval.fit3.m)

birth.pval.fit3.cvd <- svycoxph(
  formula = Surv(permth_exm, CVD_death) ~ quartile*approx.birthcohort_baseline+RIDAGEYR+URXUCR+ sex + race+edu+pir+mvp_label+smk_label+bmicat+SDDSRVYR+alcg_label,    
  design = analysis_URXOP1_crt.imputed.cont
)

summary(birth.pval.fit3.cvd)

birth.pval.fit3.c <- svycoxph(
  formula = Surv(permth_exm, cancer_death) ~ quartile*approx.birthcohort_baseline+RIDAGEYR+URXUCR+ sex + race+edu+pir+mvp_label+smk_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)

summary(birth.pval.fit3.c)

#smoking interaction joint (global) p values on "Smk int P values R file"


#smoking int p-trend (separate p values for interaction between each level of smoking (reference is nonsmoking) and ordinal quartile)
smk.pval.fit3.m <- svycoxph(
  formula = Surv(permth_exm, mortstat) ~ quartile+smk_label+quartile*smk_label+ RIDAGEYR+URXUCR+sex + race+edu+pir+mvp_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)


summary(smk.pval.fit3.m)

smk.pval.fit3.cvd <- svycoxph(
  formula = Surv(permth_exm, CVD_death) ~ quartile*smk_label+ RIDAGEYR+URXUCR+sex + race+edu+pir+mvp_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)

summary(smk.pval.fit3.cvd)


smk.pval.fit3.cancer <- svycoxph(
  formula = Surv(permth_exm, cancer_death) ~ quartile*smk_label+ RIDAGEYR+URXUCR+sex + race+edu+pir+mvp_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)


summary(smk.pval.fit3.cancer)

