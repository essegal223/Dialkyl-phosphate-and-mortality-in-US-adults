#This is the first section of code to run in analysis
#import datasets
#recode variables
#create weight variable and apply weights to survey design
#subset the datasets to satisfy inclusion criteria


#Note exposure and their code names; code names are used in code in all other R files:
#DMP=URXOP1
#DEP=URXOP2
#DMTP=URXOP3
#DETP=URXOP4
#DMTDP=URXOP5
#DETDP=URXOP6


setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Final Analysis Data")

library(haven)
library(tidyverse)
library(dplyr)

#keep outcomes variables (value are 0 and 1) as numeric for easier cox modeling

#import exposure data
ex<-read_xpt("DAP exposure/LAB26PP.XPT")
ex_B<-read_xpt("DAP exposure/L26PP_B.XPT")
ex_C<-read_xpt("DAP exposure/L26OPD_C.XPT")
ex_D<-read_xpt("DAP exposure/OPD_D.XPT")
ex_E<-read_xpt("DAP exposure/OPD_E.XPT")
ex_G<-read_xpt("DAP exposure/OPD_G.XPT")
ex_I<-read_xpt("DAP exposure/OPD_I.XPT")
ex_J<-read_xpt("DAP exposure/OPD_J.XPT")




#bind rows for all years exposure data was collected, stacks columns
ex_data<-bind_rows(ex,ex_B,ex_C,ex_D,ex_E,ex_G,ex_I,ex_J)
#check number
dim(ex_data) 


#import outcome data
setwd("Mortality Outcome")

srvyin <- paste("NHANES_1999_2000_MORT_2019_PUBLIC.dat")   
srvyout <- "NHANES_1999_2000"
# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin,
                col_types = "iiiiiiii",
                fwf_cols(SEQN = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)

srvyin <- paste("NHANES_2001_2002_MORT_2019_PUBLIC.dat")   
srvyout <- "NHANES_2001_2002"
# read in the fixed-width format ASCII file
dsn_B <- read_fwf(file=srvyin,
                  col_types = "iiiiiiii",
                  fwf_cols(SEQN = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)

# Structure and contents of data
str(dsn_B)

srvyin <- paste("NHANES_2003_2004_MORT_2019_PUBLIC.dat")   
srvyout <- "NHANES_2003_2004"
# read in the fixed-width format ASCII file
dsn_C <- read_fwf(file=srvyin,
                  col_types = "iiiiiiii",
                  fwf_cols(SEQN = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)
srvyin <- paste("NHANES_2005_2006_MORT_2019_PUBLIC.dat")   
srvyout <- "NHANES_2005_2006"
# read in the fixed-width format ASCII file
dsn_D <- read_fwf(file=srvyin,
                  col_types = "iiiiiiii",
                  fwf_cols(SEQN = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)

srvyin <- paste("NHANES_2007_2008_MORT_2019_PUBLIC.dat")   
srvyout <- "NHANES_2007_2008"
# read in the fixed-width format ASCII file
dsn_E <- read_fwf(file=srvyin,
                  col_types = "iiiiiiii",
                  fwf_cols(SEQN = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)
srvyin <- paste("NHANES_2011_2012_MORT_2019_PUBLIC.dat")   
srvyout <- "NHANES_2011_2012"
# read in the fixed-width format ASCII file
dsn_G <- read_fwf(file=srvyin,
                  col_types = "iiiiiiii",
                  fwf_cols(SEQN = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)
srvyin <- paste("NHANES_2015_2016_MORT_2019_PUBLIC.dat")   
srvyout <- "NHANES_2015_2016"
# read in the fixed-width format ASCII file
dsn_I <- read_fwf(file=srvyin,
                  col_types = "iiiiiiii",
                  fwf_cols(SEQN = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)
srvyin <- paste("NHANES_2017_2018_MORT_2019_PUBLIC.dat")   
srvyout <- "NHANES_2017_2018"
# read in the fixed-width format ASCII file
dsn_J <- read_fwf(file=srvyin,
                  col_types = "iiiiiiii",
                  fwf_cols(SEQN = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)


#bind rows for all years outcome data was collected
outcome_data<-bind_rows(dsn,dsn_B,dsn_C,dsn_D,dsn_E,dsn_G,dsn_I,dsn_J)
#outcome_data$mortstat<-as.factor(outcome_data$mortstat)
dim(outcome_data)
# Outcome Variable frequencies

#ELIGSTAT: Eligibility Status for Mortality Follow-up
table(outcome_data$eligstat)
#1 = "Eligible"
#2 = "Under age 18, not available for public release"
#3 = "Ineligible"

#MORTSTAT: Final Mortality Status
table(outcome_data$mortstat, useNA="ifany")
# 0 = Assumed alive
# 1 = Assumed deceased
# <NA> = Ineligible or under age 18

#UCOD_LEADING: Underlying Cause of Death: Recode
table(outcome_data$ucod_leading, useNA="ifany")
# 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
# 2 = Malignant neoplasms (C00-C97)
# 3 = Chronic lower respiratory diseases (J40-J47)
# 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
# 5 = Cerebrovascular diseases (I60-I69)
# 6 = Alzheimer's disease (G30)
# 7 = Diabetes mellitus (E10-E14)
# 8 = Influenza and pneumonia (J09-J18)
# 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
# 10 = All other causes (residual)
# <NA> = Ineligible, under age 18, assumed alive, or no cause of death data available

#DIABETES: Diabetes Flag from Multiple Cause of Death (MCOD)
table(outcome_data$diabetes, useNA="ifany")
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available

#HYPERTEN: Hypertension Flag from Multiple Cause of Death (MCOD)
table(outcome_data$hyperten, useNA="ifany")
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available


#full_join: merge by matching SEQN rows exposure and outcome data, keeps all data
ex_out_data<-full_join(ex_data, outcome_data, by="SEQN")

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Final Analysis Data")
#import demographic data
demo<-read_xpt("Demographic data/DEMO.XPT")
demo_B<-read_xpt("Demographic data/DEMO_B.XPT")
demo_C<-read_xpt("Demographic data/DEMO_C.XPT")
demo_D<-read_xpt("Demographic data/DEMO_D.XPT")
demo_E<-read_xpt("Demographic data/DEMO_E.XPT")
demo_G<-read_xpt("Demographic data/DEMO_G.XPT")
demo_I<-read_xpt("Demographic data/DEMO_I.XPT")
demo_J<-read_xpt("Demographic data/DEMO_J.XPT")

#bind rows for all years demographic data was collected
demo_data<-bind_rows(demo,demo_B,demo_C,demo_D,demo_E,demo_G,demo_I,demo_J)
#check number
dim(demo_data) 


#other covariates

#import creatinine data-variable is URXUCR
setwd("Labratory data-covariates only/Albumin and Creatinine")
ALB_CR<-read_xpt("LAB16.XPT")
ALB_CR_B<-read_xpt("L16_B.XPT")
ALB_CR_C<-read_xpt("L16_C.XPT")
ALB_CR_D<-read_xpt("ALB_CR_D.XPT")
ALB_CR_E<-read_xpt("ALB_CR_E.XPT")
ALB_CR_G<-read_xpt("ALB_CR_G.XPT")
ALB_CR_I<-read_xpt("ALB_CR_I.XPT")
ALB_CR_J<-read_xpt("ALB_CR_J.XPT")

#bind rows for all years creatinine data was collected
ALB_CR_data<-bind_rows(ALB_CR,ALB_CR_B,ALB_CR_C,ALB_CR_D,ALB_CR_E,ALB_CR_G,ALB_CR_I,ALB_CR_J)
#check number
dim(ALB_CR_data) 

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Final Analysis Data")
#import Body measure data
BMX<-read_xpt("Examination data/Body measures/BMX.XPT")
BMX_B<-read_xpt("Examination data/Body measures/BMX_B.XPT")
BMX_C<-read_xpt("Examination data/Body measures/BMX_C.XPT")
BMX_D<-read_xpt("Examination data/Body measures/BMX_D.XPT")
BMX_E<-read_xpt("Examination data/Body measures/BMX_E.XPT")
BMX_G<-read_xpt("Examination data/Body measures/BMX_G.XPT")
BMX_I<-read_xpt("Examination data/Body measures/BMX_I.XPT")
BMX_J<-read_xpt("Examination data/Body measures/BMX_J.XPT")
#P_BMX<-read_xpt("Examination data/Body measures/P_BMX.XPT")

#bind rows for all years body measures data was collected
BMX_data<-bind_rows(BMX,BMX_B,BMX_C,BMX_D,BMX_E,BMX_G,BMX_I,BMX_J)
#check number
dim(BMX_data) 

#import Blood Pressure examination
BPX<-read_xpt("Examination data/Blood Pressure/BPX.XPT")
BPX_B<-read_xpt("Examination data/Blood Pressure/BPX_B.XPT")
BPX_C<-read_xpt("Examination data/Blood Pressure/BPX_C.XPT")
BPX_D<-read_xpt("Examination data/Blood Pressure/BPX_D.XPT")
BPX_E<-read_xpt("Examination data/Blood Pressure/BPX_E.XPT")
BPX_G<-read_xpt("Examination data/Blood Pressure/BPX_G.XPT")
BPX_I<-read_xpt("Examination data/Blood Pressure/BPX_I.XPT")
BPX_J<-read_xpt("Examination data/Blood Pressure/BPX_J.XPT")

#bind rows for all years Blood and cholesterol data was collected
BPX_data<-bind_rows(BPX,BPX_B,BPX_C,BPX_D,BPX_E,BPX_G,BPX_I,BPX_J)
#check number
dim(BPX_data) 

#import Blood Pressure and Cholesterol Questionnaire
BPQ<-read_xpt("Questionnaire data/Blood Pressure and Cholesterol/BPQ.XPT")
BPQ_B<-read_xpt("Questionnaire data/Blood Pressure and Cholesterol/BPQ_B.XPT")
BPQ_C<-read_xpt("Questionnaire data/Blood Pressure and Cholesterol/BPQ_C.XPT")
BPQ_D<-read_xpt("Questionnaire data/Blood Pressure and Cholesterol/BPQ_D.XPT")
BPQ_E<-read_xpt("Questionnaire data/Blood Pressure and Cholesterol/BPQ_E.XPT")
BPQ_G<-read_xpt("Questionnaire data/Blood Pressure and Cholesterol/BPQ_G.XPT")
BPQ_I<-read_xpt("Questionnaire data/Blood Pressure and Cholesterol/BPQ_I.XPT")
BPQ_J<-read_xpt("Questionnaire data/Blood Pressure and Cholesterol/BPQ_J.XPT")

#bind rows for all years Blood and cholesterol data was collected
BPQ_data<-bind_rows(BPQ,BPQ_B,BPQ_C,BPQ_D,BPQ_E,BPQ_G,BPQ_I,BPQ_J)
#check number
dim(BPQ_data) 

#import Diabetes
DIQ<-read_xpt("Questionnaire data/Diabetes/DIQ.XPT")
DIQ_B<-read_xpt("Questionnaire data/Diabetes/DIQ_B.XPT")
DIQ_C<-read_xpt("Questionnaire data/Diabetes/DIQ_C.XPT")
DIQ_D<-read_xpt("Questionnaire data/Diabetes/DIQ_D.XPT")
DIQ_E<-read_xpt("Questionnaire data/Diabetes/DIQ_E.XPT")
DIQ_G<-read_xpt("Questionnaire data/Diabetes/DIQ_G.XPT")
DIQ_I<-read_xpt("Questionnaire data/Diabetes/DIQ_I.XPT")
DIQ_J<-read_xpt("Questionnaire data/Diabetes/DIQ_J.XPT")

#bind rows for all years Diabetes data was collected
DIQ_data<-bind_rows(DIQ,DIQ_B,DIQ_C,DIQ_D,DIQ_E,DIQ_G,DIQ_I,DIQ_J)
#check number
dim(DIQ_data) 

#import Occupation
OCQ<-read_xpt("Questionnaire data/Occupation/OCQ.XPT")
OCQ_B<-read_xpt("Questionnaire data/Occupation/OCQ_B.XPT")
OCQ_C<-read_xpt("Questionnaire data/Occupation/OCQ_C.XPT")
OCQ_D<-read_xpt("Questionnaire data/Occupation/OCQ_D.XPT")
OCQ_E<-read_xpt("Questionnaire data/Occupation/OCQ_E.XPT")
OCQ_G<-read_xpt("Questionnaire data/Occupation/OCQ_G.XPT")
OCQ_I<-read_xpt("Questionnaire data/Occupation/OCQ_I.XPT")
OCQ_J<-read_xpt("Questionnaire data/Occupation/OCQ_J.XPT")

#bind rows for all years Occupation data was collected
OCQ_data<-bind_rows(OCQ,OCQ_B,OCQ_C,OCQ_D,OCQ_E,OCQ_G,OCQ_I,OCQ_J)
#check number
dim(OCQ_data) 

#import Medical Conditions
MCQ<-read_xpt("Questionnaire data/Medical Conditions/MCQ.XPT")
MCQ_B<-read_xpt("Questionnaire data/Medical Conditions/MCQ_B.XPT")
MCQ_C<-read_xpt("Questionnaire data/Medical Conditions/MCQ_C.XPT")
MCQ_D<-read_xpt("Questionnaire data/Medical Conditions/MCQ_D.XPT")
MCQ_E<-read_xpt("Questionnaire data/Medical Conditions/MCQ_E.XPT")
MCQ_G<-read_xpt("Questionnaire data/Medical Conditions/MCQ_G.XPT")
MCQ_I<-read_xpt("Questionnaire data/Medical Conditions/MCQ_I.XPT")
MCQ_J<-read_xpt("Questionnaire data/Medical Conditions/MCQ_J.XPT")

#bind rows for all years Medical Conditions data was collected
MCQ_data<-bind_rows(MCQ,MCQ_B,MCQ_C,MCQ_D,MCQ_E,MCQ_G,MCQ_I,MCQ_J)
#check number
dim(MCQ_data) 

#import Cotinine
COT<-read_xpt("Labratory data-covariates only/Cotinine/LAB06.XPT")
COT_B<-read_xpt("Labratory data-covariates only/Cotinine/L06_B.XPT")
COT_C<-read_xpt("Labratory data-covariates only/Cotinine/L06COT_C.XPT")
COT_D<-read_xpt("Labratory data-covariates only/Cotinine/COT_D.XPT")
COT_E<-read_xpt("Labratory data-covariates only/Cotinine/COTNAL_E.XPT")
COT_G<-read_xpt("Labratory data-covariates only/Cotinine/COTNAL_G.XPT")
COT_I<-read_xpt("Labratory data-covariates only/Cotinine/COT_I.XPT")
COT_J<-read_xpt("Labratory data-covariates only/Cotinine/COT_J.XPT")

#bind rows for all years Cotinine data was collected
COT_data<-bind_rows(COT,COT_B,COT_C,COT_D,COT_E,COT_G,COT_I,COT_J)
#check number
dim(COT_data) 

#import LDL
LDL<-read_xpt("Labratory data-covariates only/LDL/LAB13AM.XPT")
LDL_B<-read_xpt("Labratory data-covariates only/LDL/L13AM_B.XPT")
LDL_C<-read_xpt("Labratory data-covariates only/LDL/L13AM_C.XPT")
LDL_D<-read_xpt("Labratory data-covariates only/LDL/TRIGLY_D.XPT")
LDL_E<-read_xpt("Labratory data-covariates only/LDL/TRIGLY_E.XPT")
LDL_G<-read_xpt("Labratory data-covariates only/LDL/TRIGLY_G.XPT")
LDL_I<-read_xpt("Labratory data-covariates only/LDL/TRIGLY_I.XPT")
LDL_J<-read_xpt("Labratory data-covariates only/LDL/TRIGLY_J.XPT")

#bind rows for all years Cotinine data was collected
LDL_data<-bind_rows(LDL,LDL_B,LDL_C,LDL_D,LDL_E,LDL_G,LDL_I,LDL_J)
#check number
dim(LDL_data) 

#import Total Cholesterol
TCHOL<-read_xpt("Labratory data-covariates only/Total Cholesterol/LAB13.XPT")
TCHOL_B<-read_xpt("Labratory data-covariates only/Total Cholesterol/L13_B.XPT")
TCHOL_C<-read_xpt("Labratory data-covariates only/Total Cholesterol/L13_C.XPT")
TCHOL_D<-read_xpt("Labratory data-covariates only/Total Cholesterol/TCHOL_D.XPT")
TCHOL_E<-read_xpt("Labratory data-covariates only/Total Cholesterol/TCHOL_E.XPT")
TCHOL_G<-read_xpt("Labratory data-covariates only/Total Cholesterol/TCHOL_G.XPT")
TCHOL_I<-read_xpt("Labratory data-covariates only/Total Cholesterol/TCHOL_I.XPT")
TCHOL_J<-read_xpt("Labratory data-covariates only/Total Cholesterol/TCHOL_J.XPT")

#bind rows for all years Cotinine data was collected
TCHOL_data<-bind_rows(TCHOL,TCHOL_B,TCHOL_C,TCHOL_D,TCHOL_E,TCHOL_G,TCHOL_I,TCHOL_J)
#check number
dim(TCHOL_data)


###MENGXI's variables: alcohol, physical activity, smoking#####

######use alcg variable from MENGXI dataset######
alc <- read_sas("alcohol_es.sas7bdat")
alc <- alc %>%
  select(SEQN, alcg)
table(alc$alcg)
dim(alc)

######use mvp (categorical), metscore (continuous minutes) metscore2 (continuous imputed minutes**) from MENGXI dataset####
pa <- read_sas("pa_met_es.sas7bdat")
pa <- pa %>%
  select(SEQN, mvp, metscore, metscore2)
table(pa$mvp)


######use smk (categorical) AND pack_yr (continuous) variable from MENGXI dataset for 20+ age people######
smk <- read_sas("smk_es.sas7bdat")

smk <- smk %>%
  select(SEQN, pack_yr, smk)
table(smk$smk)

#merge covariates

#put all data frames into list
demo_covs_list <- list(demo_data, ALB_CR_data, BMX_data, BPX_data, BPQ_data, COT_data, LDL_data, TCHOL_data, DIQ_data, OCQ_data, MCQ_data, alc, smk, pa)

#merge all data frames in list by SEQN
demo_covs_data<-demo_covs_list %>% reduce(full_join, by='SEQN')
#check number-should be same as dim(demo_data)
dim(demo_covs_data)

#merge covariates with exposure_outcome data
data<-full_join(demo_covs_data, ex_out_data, by="SEQN")
#check number-rows should be same as dim(demo_data) because we want to keep everyone for weighting purposes
dim(data)


#recode variables#


#recode sex#
data <- data %>%
  mutate(sex = case_when(
    RIAGENDR == 1 ~ "male",
    RIAGENDR == 2 ~ "female" 
  ),
  sex = factor(sex, levels = c("female", "male")) # Set reference level
  )
table(data$sex,useNA = "always")

# Recode ridreth1 into a new variable race#
data <- data %>%
  mutate(
    race = case_when(
      RIDRETH1 == 3 ~ "Non-Hispanic White",
      RIDRETH1 == 4 ~ "Non-Hispanic Black",
      RIDRETH1 %in% c(1, 2, 5) ~ "Other" #%in% returns TRUE or FALSE and determines whether lefthand side RIDRETH1 values is in right hand side
    ),
    race = factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Other")) # Set reference level
  )


table(data$race,useNA = "always")

# Recode DIQ010 into a new variable diabetes#
data <- data %>%
  mutate(
    diabetes_baseline = case_when(
      DIQ010 ==1 ~ "Yes",         
      DIQ010 ==2~ "No",
      DIQ010 == 3 ~ "Borderline",               
      TRUE ~ "Missing"                   # Missing or refused or don't know
    ),
    diabetes_baseline = factor(diabetes_baseline, levels = c("No", "Borderline", "Yes", "Missing")) # Set reference level
  )
table(data$diabetes_baseline)

# Recode DMDEDUC2 into a new variable edu with specified labels#
data <- data %>%
  mutate(
    edu = case_when(
      DMDEDUC2 %in% c(1, 2) ~ "less than HS graduate",       # Less than high school graduate
      DMDEDUC2 == 3 | DMDEDUC2 == 4 ~ "HS graduate/GED or equiv or some college",  # High school graduate or equivalent or some college
      DMDEDUC2 == 5 ~ "college graduate or above",                      # College graduate or above
      TRUE ~ "Missing"                                   # NA if missing or refused to answer or don't know
    ),
    edu = factor(edu, levels = c("college graduate or above", "less than HS graduate", "HS graduate/GED or equiv or some college", "Missing")) # Set reference level
  )

table(data$edu)

#Recode BMXBMI into bmi variable
data <- data %>%
  mutate(
    bmicat = case_when(
      BMXBMI < 18.5 ~ "underweight",         # BMXBMI < 18.5
      BMXBMI >= 18.5 & BMXBMI < 25 ~ "normal", # 18.5 <= BMXBMI < 25
      BMXBMI >= 25 & BMXBMI < 30 ~ "overweight", # 25 <= BMXBMI < 30
      BMXBMI >= 30 ~ "obese",                # BMXBMI >= 30
      TRUE ~ "Missing"                  # Handle NA or missing values
    ),
    bmicat = factor(bmicat, levels = c("normal", "underweight", "overweight", "obese", "Missing")) # Set reference level
  )
summary(data$bmicat)


# Recode INDFMPIR into a new variable pir with specified labels#

data <- data %>%
  mutate(
    pir = case_when(
      INDFMPIR < 1.3 ~ "<1.3",      
      INDFMPIR >= 1.3 & INDFMPIR <= 2.99 ~ "1.3-2.99",              
      INDFMPIR > 2.99 & INDFMPIR <= 4.99 ~ "3.00-4.99",                     
      INDFMPIR > 4.99 ~ ">=5",                    
      TRUE ~ "Missing"                                  
    ),
    pir = factor(pir, levels = c(">=5", "<1.3", "1.3-2.99", "3.00-4.99", "Missing")) # Set reference level
  )

summary(data$pir)




#recode systolic BP into variable called sbp and diastolic BP into variables called dbp
data$sbp <- rowMeans(data[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)
summary(data$sbp)
data$dbp <- rowMeans(data[, c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")], na.rm = TRUE)
summary(data$dbp)

# Recode sbp or dbp into a new variable hypertension#
data <- data %>%
  mutate(
    hypertension = case_when(
      sbp >= 140 | dbp >= 90~ "Hypertension",         
      sbp < 140 & dbp < 90~ "No Hypertension",               
      TRUE ~ "Missing"                   # Missing or refused or don't know
    ),
    hypertension = factor(hypertension, levels = c("Hypertension", "No Hypertension", "Missing")) # Set reference level
  )

summary(data$hypertension)

# Recode LBXTC into a new variable total cholesterol#
data <- data %>%
  mutate(
    cholesterol_cat = case_when(
      LBXTC >= 240~ "High Cholesterol",         
      LBXTC >=200~ "Borderline High Cholesterol", 
      LBXTC <200~ "Normal Cholesterol",
      TRUE ~ "Missing"                   # Missing or refused or don't know
    ),
    cholesterol_cat = factor(cholesterol_cat, levels = c("High Cholesterol", "Borderline High Cholesterol","Normal Cholesterol", "Missing")) # Set reference level
  )

summary(data$cholesterol_cat)

#create approximate birth year variable
data<- data %>%
  mutate(
    approx.birthyr_baseline = case_when(
      SDDSRVYR==1 ~ 2000-RIDAGEYR,
      SDDSRVYR==2 ~ 2002-RIDAGEYR,
      SDDSRVYR==3 ~ 2004-RIDAGEYR,
      SDDSRVYR==4 ~ 2006-RIDAGEYR,
      SDDSRVYR==5 ~ 2008-RIDAGEYR,
      SDDSRVYR==7 ~ 2012-RIDAGEYR,
      SDDSRVYR==9 ~ 2016-RIDAGEYR,
      TRUE ~ 2018-RIDAGEYR
    ) )

summary(data$approx.birthyr_baseline)


#recode smk variable to a factor variable

#recode NA as 99
#data$smk <- ifelse(is.na(data$smk), 99, data$smk) 
table(data$smk, useNA="always")
data <- data%>%
  mutate(smk_label = case_when(
    smk == 1 ~ "non smoker",
    smk == 2 ~ "past smoker",
    smk == 3 ~ "current smoker",
    TRUE ~ "Missing"  # Handle unexpected values with NA
  ),
  smk_label = factor(smk_label, levels = c("non smoker", "past smoker", "current smoker", "Missing"))
  )
table(data$smk_label) #there is a lot of missing, but this makes sense because the variable is only for 20+ year olds
sum(data$RIDAGEYR <20) #37510 are younger than 20

#pack year measurement variable is called pack_yr

#recode NA as 99
table(data$mvp, useNA="always")
data$mvp <- ifelse(is.na(data$mvp), 99, data$mvp)

#recode mvp
data <- data%>%
  mutate(mvp_label = case_when(
    mvp == 0 ~ "no activity",
    mvp == 1 ~ "activity",
    TRUE ~ "Missing"  # Handle unexpected values with NA
  ),
  mvp_label = factor(mvp_label, levels = c("activity", "no activity", "Missing"))
  )

table(data$mvp_label)


#recode NA as 99
data$alcg <- ifelse(is.na(data$alcg), 99, data$alcg)
#recode alcg
data <- data%>%
  mutate(alcg_label = case_when(
    alcg == 1 ~ "non drinker",
    alcg == 2 ~ "moderate drinker",
    alcg == 3 ~ "heavy drinker",
    TRUE ~ "Missing"  # Handle unexpected values with NA
  ),
  alcg_label = factor(alcg_label, levels = c("non drinker", "moderate drinker", "heavy drinker", "Missing"))
  )

table(data$alcg_label)

data$SDDSRVYR=as.factor(data$SDDSRVYR)



#handle the multiple creatinine variables as a result of it being reported in multiple datasets
data %>%
  group_by(SDDSRVYR) %>%
  summarise(count = n())

#this variable is missing for cycles 7,9,10
data %>%
  group_by(SDDSRVYR) %>%
  summarise(na_count = sum(is.na(URXUCR)))

#this variable is present in all survey cycles
data %>%
  group_by(SDDSRVYR) %>%
  summarise(
    na_count = sum(is.na(URXUCR.x)),
    total_count = n(),
    percent_missing = (na_count / total_count) * 100
  )
#this variable is missing for 1-4, 9-10
data %>%
  group_by(SDDSRVYR) %>%
  summarise(na_count = sum(is.na(URXUCR.y)))

data %>%
  filter(!is.na(URXUCR.x)) %>%  # Remove NAs
  ggplot(aes(x = URXUCR.x)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of URXUCR.x (Without NAs)",
    x = "URXUCR.x",
    y = "Frequency"
  ) +
  theme_minimal()

data <- data %>%
  select(-URXUCR, -URXUCR.y) %>%  # Remove specified columns
  rename(URXUCR = URXUCR.x)       # Rename URXUCR.x to URXUCR


#combining 8 year survey weights
#verify which survey cycles are in the data
unique(data$SDDSRVYR)




###follow up time variable as time scale###

#permth_exm
#note: there are missing values for permth_exm variable because either ineligible or younger than 18 yrs
summary(data$permth_exm)

#note that 2/3 of the people should not be given a weight because only 1/3 of the total population was tested for the exposure
#labratory data files analytic notes stated the specific subsample weights that should be used in each cycle file 
data$weight <- ifelse(
  data$SDDSRVYR %in% c(1, 2), #1999-2002
  2/8 * data$WTSPP4YR, 
  ifelse(
    data$SDDSRVYR %in% c(3, 4, 5, 7), #2003-08, 2011-12
    1/8 * data$WTSC2YR, 
    ifelse(
      data$SDDSRVYR %in% c(9, 10), #2015-18
      1/8 * data$WTSB2YR, 
      NA # Assign NA for cases not matching any condition
    )
  )
)


summary(data$weight)
sum(data$weight > 0, na.rm = TRUE)  #indivduals with >0 as weight: sampled to test, received test result
#20923
sum(data$weight == 0, na.rm = TRUE) #indivduals with 0 as weight: sampled to test, but no response
#1112
sum(is.na(data$weight)) #indivdiuals with NA as weight: not sampled to test
#58569

#total: 80604


#Drop those with weight=NA
data <- data %>% filter(!is.na(weight))
dim(data)
#remaining: 22035
sum(is.na(data$URXUCR)) / nrow(data) * 100



#apply weights
library(survey)

analysis_all <- svydesign(data=data, id=~SDMVPSU, strata=~SDMVSTRA, weights=~weight, nest=TRUE)

#weights have been calculated and applied, now we can subset the survey data

table(analysis_all$variables$mortstat, useNA="always")
dim(analysis_all)


#drop those not eligible for linkage
mort_eligDesign<-subset(analysis_all, eligstat == 1) #this automatically 
#removes younger than 18 because they are not eligible
dim(mort_eligDesign)
#no missing mortality status
table(mort_eligDesign$variables$mortstat, useNA="always")
subset.elig.18 <- subset(mort_eligDesign, RIDAGEYR>= 18)
dim(subset.elig.18) #13881

#drop those with missing follow up
#6 people have no follow up time from exam, we can exclude them here# (note: current written table1 includes them)
sum(subset.elig.18$variables$permth_exm==0)
subset.elig.18.followed=subset(subset.elig.18, permth_exm!=0)
dim(subset.elig.18.followed) #13875

#drop those with zero weight
subset.elig.18.followed.nonzeroweight=subset(subset.elig.18.followed, weight!=0)
dim(subset.elig.18.followed.nonzeroweight) #13125

#creatinine missingness
sum(is.na(subset.elig.18.followed.nonzeroweight$variables$URXUCR)) / nrow(subset.elig.18.followed.nonzeroweight) * 100 #about 1% of people
sum(is.na(subset.elig.18.followed.nonzeroweight$variables$URXUCR)) #159 people
subset.elig.18.followed.nonzeroweight$variables %>%
  group_by(SDDSRVYR) %>%
  summarise(
    na_count = sum(is.na(URXUCR)),
    total_count = n(),
    percent_missing = (na_count / total_count) * 100
  )
#drop those with missing creatinine
subset.elig.18.followed.nonzeroweight.creatinine=subset(subset.elig.18.followed.nonzeroweight, !is.na(URXUCR))
dim(subset.elig.18.followed.nonzeroweight.creatinine) #12966
#subset.elig.18.followed.nonzeroweight.creatinine dataset
#includes only those who are linkage eligible, older than 18, nonzero follow up time, nonzero weight and measured creatinine

#in individual exposure analysis code, those with NA of DAP of interest are dropped