#Run the following files first: "Final import, recode, weight, subset.R" THEN "Final_Create creatinine adjusted quart.R" 

#Purpose of this file: 
#Interaction analysis: 
#To conduct creatinine adjusted DAP quartile versus mortality by smoking status and birthcohort
#and compute HR point estimates and 95% CIs 
#Note that Plot in paper only shows results for running analysis for DMP, DMTP, and DETP

#after running code, control F exposure variable and replace with next exposure number and rerun 
#for each exposure to get estimates for each exposure

#What is completed in this file#
#create birth cohort variable (2 categories: birth cohort 1910-1950 or birth cohort 1951-2000)
#for each mortality outcome, there are 2 models: one for birth cohort interaction with quartile and one for smoking interaction with quartile
#total models: 6

analysis_URXOP1_crt.imputed.cont$variables <- analysis_URXOP1_crt.imputed.cont$variables %>%
  mutate(
    approx.birthcohort_baseline = case_when(
      approx.birthyr_baseline >= 1910 & approx.birthyr_baseline <= 1950 ~ "1910-1950",
      approx.birthyr_baseline >= 1951 & approx.birthyr_baseline <= 2000 ~ "1951-2000",
      TRUE ~ "NA"     
    )%>% 
      factor(levels = c("1910-1950", "1951-2000" ) # Setting "1910-1950" as the reference group
      )
  )

table(analysis_URXOP1_crt.imputed.cont$variables$approx.birthcohort_baseline)
#all-cause birth cohort interaction

birth.int.fit2.m <- svycoxph(
  formula = Surv(permth_exm, mortstat) ~ approx.birthcohort_baseline+approx.birthcohort_baseline:as.factor(quartile)+RIDAGEYR+URXUCR+ sex + race+edu+pir+mvp_label+smk_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)

summary(birth.int.fit2.m)

#all-cause smoking interaction
smk.int.fit2.m <- svycoxph(
  formula = Surv(permth_exm, mortstat) ~ smk_label+as.factor(quartile):smk_label+ RIDAGEYR+URXUCR+sex + race+edu+pir+mvp_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)


summary(smk.int.fit2.m)


#CVD birth cohort interaction

birth.int.fit2.cvd <- svycoxph(
  formula = Surv(permth_exm, CVD_death) ~ approx.birthcohort_baseline+approx.birthcohort_baseline:as.factor(quartile)+RIDAGEYR+URXUCR+ sex + race+edu+pir+mvp_label+smk_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)

summary(birth.int.fit2.cvd)

#CVD smoking interaction

smk.int.fit2.cvd <- svycoxph(
  formula = Surv(permth_exm, CVD_death) ~ smk_label+as.factor(quartile):smk_label+ RIDAGEYR+URXUCR+sex + race+edu+pir+mvp_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)


summary(smk.int.fit2.cvd)



#cancer birth cohort interaction
birth.int.fit2.c <- svycoxph(
  formula = Surv(permth_exm, cancer_death) ~ approx.birthcohort_baseline+approx.birthcohort_baseline:as.factor(quartile)+RIDAGEYR+URXUCR+ sex + race+edu+pir+mvp_label+smk_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)

summary(birth.int.fit2.c)

#cancer smoking interaction

smk.int.fit2.cancer <- svycoxph(
  formula = Surv(permth_exm, cancer_death) ~ smk_label+as.factor(quartile):smk_label+ RIDAGEYR+URXUCR+sex + race+edu+pir+mvp_label+bmicat+SDDSRVYR+alcg_label,  
  design = analysis_URXOP1_crt.imputed.cont
)


summary(smk.int.fit2.cancer)
