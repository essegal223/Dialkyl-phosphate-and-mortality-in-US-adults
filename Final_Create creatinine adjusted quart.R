
#Run the following files first: "Final import, recode, weight, subset.R"

#Purpose of this file: To adjust exposures for mortality datasets

#what is completed in this file#
#create each DAP dataset
#create creatinine adjusted DAP variable for each dataset
#create the cycle-specific quartiles
#after running code with first DAP, control F exposure variable and replace with next exposure number and rerun 
  #until all 6 exposure datasets and variables (ie quartile variable) created
  #DAP and corresponding code names are listed in "Final import, recode, weight, subset.R"
#dimensions for each DAP dataset are reported


#drop 
analysis_URXOP4_crt<- subset(subset.elig.18.followed.nonzeroweight.creatinine, !is.na(URXOP4))
dim(analysis_URXOP4_crt) #DAP 1: 12798

#create creatinine adjusted exposure (O'Brien's method)
# take the log of creatinine variable
analysis_URXOP4_crt$variables<- analysis_URXOP4_crt$variables %>% 
  mutate(logC = log(URXUCR))

# Fit the regression model
#check percent missing on bmicat: about 1%, has missing category
summary(subset.elig.18.followed.nonzeroweight.creatinine$variables$bmicat)
model <- lm(logC ~ RIDAGEYR+ sex+race+bmicat+SDDSRVYR, data = analysis_URXOP4_crt$variables)
summary(model)
# Add predicted values variable to the dataset
analysis_URXOP4_crt$variables$plogC <- predict(model, data = analysis_URXOP4_crt$variables)

# Compute new variables
analysis_URXOP4_crt$variables <- analysis_URXOP4_crt$variables %>%
  mutate(
    pC = exp(plogC),
    Cratio = URXUCR / pC,
    URXOP4_crt = URXOP4 / Cratio
  )

# View the updated dataset
head(analysis_URXOP4_crt$variables %>% select(SEQN, URXOP4_crt))


max(analysis_URXOP4_crt$variables$URXOP4_crt)
analysis_URXOP4_crt$variables %>% 
  group_by(SDDSRVYR) %>% 
  summarize(max_URXOP4_crt = max(URXOP4_crt, na.rm = TRUE))

#create the cycle-specific quartiles
analysis_URXOP4_crt$variables <- analysis_URXOP4_crt$variables %>%
  group_by(SDDSRVYR) %>%
  mutate(
    quartile = as.numeric(
      cut(
        URXOP4_crt,
        breaks = quantile(URXOP4_crt, probs = seq(0, 1, 0.25), na.rm = TRUE),
        include.lowest = TRUE,
        labels = FALSE
      )
    )
  ) %>%
  ungroup()

table(analysis_URXOP4_crt$variables$quartile)

quartile_check <- analysis_URXOP4_crt$variables %>%
  group_by(SDDSRVYR, quartile) %>%  # Group by cycle & quartile
  summarise(
    count = n(), 
    min_URXOP4_crt = min(URXOP4_crt, na.rm = TRUE),
    max_URXOP4_crt = max(URXOP4_crt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(SDDSRVYR) %>%                                # Regroup to calculate percentages
  mutate(percentage = round(100 * count / sum(count), 2)) %>%  # Calculate % within each cycle
  arrange(SDDSRVYR,quartile)   # Sort for easier comparison

quartile_check

#variables on other files use analysis_URXOP4_crt.imputed.cont, thus we create this dataset so we don't have to change everything
analysis_URXOP4_crt.imputed.cont<- analysis_URXOP4_crt
analysis_URXOP4_crt.imputed.cont$variables <- analysis_URXOP4_crt.imputed.cont$variables %>%
  dplyr::select(-URXOP4)  # Drop the URXOP4 column to ensure no mix up in analysis
dim(analysis_URXOP4_crt.imputed.cont) 
#DAP1 12798 by 911
#DAP2 12801 by 911, 
#DAP3 12817 by 911, 
#DAP4 12755 by 911,
#DAP5 12802 by 911,
#DAP6 12690 by 911
