
#Run the following files first: "Final import, recode, weight, subset.R"

#Purpose of this file: Create dataset with adjusted exposures to use for Spearman correlation computation

#what is completed in this file#
#create dataset with participants who have all exposures
#create creatinine adjusted 6 DAP variables

#Spearman
spearman.data.completecases <- subset(subset.elig.18.followed.nonzeroweight.creatinine, 
                        complete.cases(URXOP1, URXOP2, URXOP3, URXOP4, URXOP5, URXOP6))
dim(spearman.data.completecases) #12446 by 910
# Transform the data
spearman.data.completecases$variables<- spearman.data.completecases$variables%>% 
  mutate(logC = log(URXUCR)) #log creatinine

# Fit the regression model
model <- lm(logC ~ RIDAGEYR+ sex+race+bmicat+SDDSRVYR, data = spearman.data.completecases$variables)

# Add predicted log creatinine values to the dataset
spearman.data.completecases$variables$plogC <- predict(model, spearman.data.completecases$variables) #predicted log creatinine
head(spearman.data.completecases$variables %>% select(SEQN, plogC))

# Compute new variables
spearman.data.completecases$variables <- spearman.data.completecases$variables %>%
  mutate(
    pC = exp(plogC), #predicted creatinine
    Cratio = URXUCR / pC, #observed creatinine divided by predicted creatinine
    URXOP1_crt = URXOP1 / Cratio, #covariate-adjusted URXOP1
    URXOP2_crt = URXOP2 / Cratio, #covariate-adjusted URXOP2
    URXOP3_crt = URXOP3 / Cratio, #covariate-adjusted URXOP3
    URXOP4_crt = URXOP4 / Cratio, #covariate-adjusted URXOP4
    URXOP5_crt = URXOP5 / Cratio, #covariate-adjusted URXOP5
    URXOP6_crt = URXOP6 / Cratio  #covariate-adjusted URXOP6
    
  )

# View the updated dataset
head(spearman.data.completecases$variables %>% select(SEQN, URXOP1_crt))
#compare
head(spearman.data.completecases$variables %>% select(SEQN, URXOP1))


dim(spearman.data.completecases) #12446 by 922
