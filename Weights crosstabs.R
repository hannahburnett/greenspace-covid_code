#Set up:

library(foreign)
dta_code <- read.xlsx(file.choose(), convert.underscore = T)

#import dta
dta_code <- SAV_for_University_of_Glasgow_Outside_Space_and_Covid_1_5_2020_CODES
dta_label <- SAV_for_University_of_Glasgow_Outside_Space_and_Covid_1_5_2020_LABEL
rm(SAV_for_University_of_Glasgow_Outside_Space_and_Covid_1_5_2020_LABEL)

#delete row
dta_code <- dta_code[-1,]
dta_label <- dta_label[-1,]
dta_label2 <- dta_label2[-1,]

#Weight tbl works*
class(dta_code$weight)
dta_code$weight <- as.numeric(dta_code$weight)
dta_label$weight <- as.numeric(dta_label$weight)
class(dta_label$weight)

#Ethnicity groups
dta_label$ethnicity <- NA
dta_label$ethnicity[dta_label2$profile_ethnicity=="White British"] <- "White"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Any other white background"] <- "White"
dta_label$ethnicity[dta_label2$profile_ethnicity=="White and Black Caribbean"] <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="White and Black African"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="White and Asian"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Any other mixed background"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Indian"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Pakistani"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Bangladeshi"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Any other Asian background"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Black Caribbean"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Black African"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Any other black background"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Chinese"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Other ethnic group"]  <- "BAME"
dta_label$ethnicity[dta_label2$profile_ethnicity=="Prefer not to say"]  <- NA
table(dta_label$ethnicity)
table(dta_label$profile_ethnicity)

#ethnicity - sort order
table(dta_label$ethnicity)
sort(table(dta_label$ethnicity), decreasing = TRUE)
names(sort(table(dta_label$ethnicity), decreasing = TRUE))
dta_label$ethnicity <- factor(dta_label$ethnicity, 
                              levels = names(sort(table(dta_label$ethnicity), decreasing = TRUE)))


#-----------------------------------------------------------------------------

#Crosstabs
library(descr)
library(stats)
library(tigerstats)
library(questionr)
library(weights)

#visit before lockdown#
#Change structure of responses
table(dta_label$LGO_Q1a)
sort(table(dta_label$LGO_Q1a), decreasing = TRUE)
names(sort(table(dta_label$LGO_Q1a), decreasing = TRUE))
dta_label$LGO_Q1a <- factor(dta_label$LGO_Q1a, 
                            levels = names(sort(table(dta_label$LGO_Q1a), decreasing = TRUE)))
#one var freq:
xtabs(dta_label$weight ~ dta_label$LGO_Q1a)
rowPerc(xtabs(dta_label$weight ~ dta_label$LGO_Q1a))
wtd.table(x = dta_label$LGO_Q1a, weights = dta_label$weight)

wpct(dta_label$LGO_before, weight=dta_label$weight)

table(dta_label$ethnicity)
#sex
crosstab(dta_label$LGO_before, dta_label$sex, prop.c=T, weight = dta_label$weight, chisq = T, plot = F)
#social grade
crosstab(dta_label$LGO_before, dta_label$socialgrade, prop.c=T,prop.r = T, weight = dta_label$weight, chisq = T, plot = F)
#age
crosstab(dta_label$LGO_before, dta_label$age, prop.c=T, prop.r = T, weight = dta_label$weight, chisq = T, plot = F)
#ethnicity
crosstab(dta_label$LGO_Q1a, dta_label$ethnicity, prop.c=T,prop.r = T, weight = dta_label$weight, chisq = T, plot = F)
#dog owners
crosstab(dta_label$LGO_before, dta_label$dogs, prop.c=T, prop.r = T, weight = dta_label$weight, chisq = T, plot = F)

wtd.table(dta_label$LGO_before, weights = dta_label$weight)

#Log Reg Models***********************

#Binary cats - Yes/No (don't know = missing)
dta_label$LGO_Q1a_cat <- NA
dta_label$LGO_Q1a_cat[dta_label$LGO_Q1a=="Yes, I did"] <- "Yes, I did"
dta_label$LGO_Q1a_cat[dta_label$LGO_Q1a=="No, I didn't"] <- "No, I didn't"
dta_label$LGO_Q1a_cat[dta_label$LGO_Q1a=="Don't know/ can't recall"] <- NA
table(dta_label$LGO_Q1a_cat)
#make numeric
dta_label$LGO_Q1a_cat <- as.numeric(as.character(dta_label$LGO_Q1a_cat))
dta_label$LGO_Q1a_cat <- as.factor(dta_label$LGO_Q1a_cat)
class(dta_label$LGO_Q1a_cat)


#Gender = factor
table(dta_label$profile_gender)
dta_label$sex <- NA
dta_label$sex[dta_label$profile_gender=="Female"] <- "Female"
dta_label$sex[dta_label$profile_gender=="Male"] <- "Male"
table(dta_label$sex)
#make factor
dta_label$sex <- as.factor(dta_label$sex)
class(dta_label$sex)

#LGM- Gender
#check which will be reference
contrasts(dta_label$sex)
library(stats)
lm.sex.before <- glm(dta_label$LGO_Q1a_cat ~ dta_label$sex, family = binomial, weights = dta_label$weight)
summary(lm.sex.before)
exp(cbind(OR = coef(lm.sex.before), confint.default(lm.sex.before)))

#Social grade = factor
dta_label$socialgrade <- NA
dta_label$socialgrade[dta_label$profile_socialgrade_cie_rc=="ABC1"] <- "ABC1"
dta_label$socialgrade[dta_label$profile_socialgrade_cie_rc=="C2DE"] <- "C2DE"
table(dta_label$profile_socialgrade_cie_rc)
#make factor
dta_label$socialgrade <- as.factor(dta_label$socialgrade)
class(dta_label$socialgrade)

#LGM - social grade
contrasts(dta_label$socialgrade)
library(stats)
lm.sg.before <- glm(dta_label$LGO_Q1a_cat ~ dta_label$socialgrade, family = binomial, weights = dta_label$weight)
summary(lm.sg.before)
exp(cbind(OR = coef(lm.sg.before), confint.default(lm.sg.before)))

#Age = factor
class(dta_label$profile_julesage)
table(dta_label$profile_julesage)
dta_label$age <- NA
dta_label$age[dta_label$profile_julesage=="18-24"] <- "18-24"
dta_label$age[dta_label$profile_julesage=="25-34"] <- "25-34"
dta_label$age[dta_label$profile_julesage=="35-44"] <- "35-44"
dta_label$age[dta_label$profile_julesage=="45-54"] <- "45-54"
dta_label$age[dta_label$profile_julesage=="55+"] <- "55+"
table(dta_label$age)
#make factor
dta_label$age <- as.factor(dta_label$age)
class(dta_label$age)

#LGM - age
contrasts(dta_label$age)
lm.age.before <- glm(dta_label$LGO_Q1a_cat ~ dta_label$age, family = binomial, weights = dta_label$weight)
summary(lm.age.before)
exp(cbind(OR = coef(lm.age.before), confint.default(lm.age.before)))

#**************************************************************
  
#visits since lockdown(yes/no)#
#Change structure of responses
table(dta_label$LGO_Q1)
sort(table(dta_label$LGO_Q1), decreasing = TRUE)
names(sort(table(dta_label$LGO_Q1), decreasing = TRUE))
dta_label$LGO_Q1 <- factor(dta_label$LGO_Q1, 
                            levels = names(sort(table(dta_label$LGO_Q1), decreasing = TRUE)))

#one var
rowPerc(xtabs(dta_label$weight ~ dta_label$LGO_Q1))
wtd.table(x = dta_label$LGO_Q1, weights = dta_label$weight)

table(dta_label$LGO_after)
contrasts(dta_label$LGO_after)

wpct(dta_label$LGO_after, weight=dta_label$weight)
wtd.table(dta_label$LGO_after, weights = dta_label$weight)
#sex
crosstab(dta_label$LGO_after, dta_label$sex, prop.c=T, weight = dta_label$weight, chisq = T, plot=F)
#social grade
crosstab(dta_label$LGO_after, dta_label$socialgrade, prop.c=T, weight = dta_label$weight, chisq = T,plot=F)
#age
crosstab(dta_label$LGO_after, dta_label$age, prop.c=T, weight = dta_label$weight, chisq = T,plot=F)
#ethnicity
crosstab(dta_label$LGO_after, dta_label$ethnicity, prop.c=T, weight = dta_label$weight, chisq = T, plot=F)
#dogs
crosstab(dta_label$LGO_after, dta_label$dogs, prop.c=T, weight = dta_label$weight, chisq = T, plot=F)

#Yes/No (don't know = missing)
dta_label$LGO_Q1_cat <- NA
dta_label$LGO_Q1_cat[dta_label$LGO_Q1=="Yes, I have"] <- "Yes"
dta_label$LGO_Q1_cat[dta_label$LGO_Q1=="No, I haven't"] <- "No"
dta_label$LGO_Q1_cat[dta_label$LGO_Q1=="Don't know/ can't recall"] <- NA
table(dta_label$LGO_Q1_cat)

#**************************************************************

#Visiting before vs since lockdown#
crosstab(dta_label$LGO_Q1a, dta_label$LGO_Q1, prop.c=T, weight = dta_label$weight, chisq = T, fisher=T)

#**************************************************************

#visitation change since lockdown#

#sex
crosstab(dta_label$LGO_change, dta_label$sex, prop.c=T, weight = dta_label$weight, chisq = T, plot=F)
#social grade
crosstab(dta_label$LGO_change, dta_label$socialgrade, prop.c=T, weight = dta_label$weight, chisq = T,plot=F)
#age
crosstab(dta_label$LGO_change, dta_label$age, prop.c=T,weight = dta_label$weight, chisq = T,plot=F)
#ethnicity
crosstab(dta_label$LGO_change, dta_label$ethnicity, prop.c=T,  weight = dta_label$weight, chisq = T,plot=F)
#dog owners
crosstab(dta_label$LGO_change, dta_label$dogs, prop.c=T,  weight = dta_label$weight, chisq = T,plot=F)

#Change structure - increase/decrease
dta_label$LGO_change <- NA
dta_label$LGO_change[dta_label$LGO_Q2=="Decreased a lot"] <- "Decreased"
dta_label$LGO_change[dta_label$LGO_Q2=="Decreased a little"] <- "Decreased"
dta_label$LGO_change[dta_label$LGO_Q2=="No difference"] <- "No difference"
dta_label$LGO_change[dta_label$LGO_Q2=="Increased a little"] <- "Increased"
dta_label$LGO_change[dta_label$LGO_Q2=="Increased a lot"] <- "Increased"
dta_label$LGO_change[dta_label$LGO_Q2=="Don't know"] <- NA
table(dta_label$LGO_change)
table(dta_label$LGO_Q2)

#make visit decrease variable 
dta_label$visit_decr <- NA
dta_label$visit_decr[dta_label$LGO_Q2=="Decreased a lot"] <- "Decreased"
dta_label$visit_decr[dta_label$LGO_Q2=="Decreased a little"] <- "Decreased"
dta_label$visit_decr[dta_label$LGO_Q2=="No difference"] <- "Other"
dta_label$visit_decr[dta_label$LGO_Q2=="Increased a little"] <- "Other"
dta_label$visit_decr[dta_label$LGO_Q2=="Increased a lot"] <- "Other"
dta_label$visit_decr[dta_label$LGO_Q2=="Don't know"] <- NA
table(dta_label$visit_decr)
table(dta_label$visit_increase)


table(dta_label$visit_increase)
sort(table(dta_label$visit_increase), decreasing = TRUE)
names(sort(table(dta_label$visit_increase), decreasing = TRUE))
dta_label$visit_increase <- factor(dta_label$visit_increase, 
                              levels = names(sort(table(dta_label$visit_increase), decreasing = TRUE)))
#one var
rowPerc(xtabs(dta_label$weight ~ dta_label$LGO_Q2_id))
wtd.table(x = dta_label$LGO_Q2_id, weights = dta_label$weight)

#sex
crosstab(dta_label$LGO_Q2_id, dta_label$profile_gender, prop.c=T, weight = dta_label$weight, chisq = T)
#social grade
crosstab(dta_label$LGO_Q2_id, dta_label$profile_socialgrade_cie_rc, prop.c=T, weight = dta_label$weight, chisq = T)
#age
crosstab(dta_label$LGO_Q2_id, dta_label$profile_julesage, prop.c=T, weight = dta_label$weight, chisq = T)
#ethnicity
crosstab(dta_label$LGO_Q2_id, dta_label$ethnicity, prop.c=T, weight = dta_label$weight, chisq = T)

#Increased vs others
dta_label$LGO_Q2_in <- NA
dta_label$LGO_Q2_in[dta_label$LGO_Q2=="Decreased a lot"] <- "Other"
dta_label$LGO_Q2_in[dta_label$LGO_Q2=="Decreased a little"] <- "Other"
dta_label$LGO_Q2_in[dta_label$LGO_Q2=="No difference"] <- "Other"
dta_label$LGO_Q2_in[dta_label$LGO_Q2=="Increased a little"] <- "Increased"
dta_label$LGO_Q2_in[dta_label$LGO_Q2=="Increased a lot"] <- "Increased"
dta_label$LGO_Q2_in[dta_label$LGO_Q2=="Don't know"] <- NA
table(dta_label$LGO_Q2_in)
is.na(dta_label$LGO_Q2_in)

#sex
crosstab(dta_label$visit_increase, dta_label$profile_gender, prop.c=T, weight = dta_label$weight, chisq = T)
#social grade
crosstab(dta_label$visit_increase, dta_label$profile_socialgrade_cie_rc, prop.c=T, weight = dta_label$weight, chisq = T)
#age
crosstab(dta_label$visit_increase, dta_label$profile_julesage, prop.c=T, weight = dta_label$weight, chisq = T)
#ethnicity
crosstab(dta_label$visit_increase, dta_label$ethnicity, prop.c=T, weight = dta_label$weight, chisq = T)
wpct(dta_label$LGO_change, weight=dta_label$weight)
wpct(dta_label$visit_increase, weight=dta_label$weight)
wpct(dta_label$visit_decr, weight=dta_label$weight)

wtd.table(dta_label$LGO_change, weights = dta_label$weight)
wtd.table(dta_label$visit_increase, weights = dta_label$weight)
wtd.table(dta_label$visit_decr, weights = dta_label$weight)


#**************************************************************

#Experience - MH benefit#

#Change structure
#agree/disagree
dta_label$exp_MH <- NA
dta_label$exp_MH[dta_code$LGO_Q3_1=="1"] <- "Agree"
dta_label$exp_MH[dta_code$LGO_Q3_1=="2"] <- "Agree"
dta_label$exp_MH[dta_code$LGO_Q3_1=="3"] <- "Neither agree nor disagree"
dta_label$exp_MH[dta_code$LGO_Q3_1=="4"] <- "Disagree"
dta_label$exp_MH[dta_code$LGO_Q3_1=="5"] <- "Disagree"
dta_label$exp_MH[dta_code$LGO_Q3_1=="977"] <- "Missing"
table(dta_label$exp_MH)
table(dta_label$LGO_Q3_1)

#for analysis: agree vs all others
dta_label$mentalhealth_agree <- NA
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="1"] <- "Agree"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="2"] <- "Agree"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="3"] <- "Other"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="4"] <- "Other"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="5"] <- "Other"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="977"] <- NA
table(dta_label$mentalhealth_agree)

#one var freq:
xtabs(dta_label$weight ~ dta_label$LGO_Q1a)
rowPerc(xtabs(dta_label$weight ~ dta_label$exp_MH))
wtd.table(x = dta_label$exp_MH, weights = dta_label$weight)

wpct(dta_label$mentalhealth_agree, weight=dta_label$weight)
wtd.table(dta_label$mentalhealth_agree, weights = dta_label$weight)
#sex
crosstab(dta_label$mentalhealth_agree, dta_label$sex, prop.c=T, weight = dta_label$weight, chisq = T, plot=F)
#social grade
crosstab(dta_label$mentalhealth_agree, dta_label$socialgrade, prop.c=T, weight = dta_label$weight, chisq = T,plot=F)
#age
crosstab(dta_label$mentalhealth_agree, dta_label$age, prop.c=T,  weight = dta_label$weight, chisq = T,plot=F)
#ethnicity
crosstab(dta_label$mentalhealth_agree, dta_label$ethnicity, prop.c=T,  weight = dta_label$weight, chisq = T,plot=F)
#dogs
crosstab(dta_label$mentalhealth_agree, dta_label$dogs, prop.c=T, weight = dta_label$weight, chisq = T,plot=F)


#***************************************************************************************

#Experience - social interaction#
#agree/disagree
dta_label$exp_SI <- NA
dta_label$exp_SI[dta_code$LGO_Q3_2=="1"] <- "Agree"
dta_label$exp_SI[dta_code$LGO_Q3_2=="2"] <- "Agree"
dta_label$exp_SI[dta_code$LGO_Q3_2=="3"] <- "Neither agree nor disagree"
dta_label$exp_SI[dta_code$LGO_Q3_2=="4"] <- "Disagree"
dta_label$exp_SI[dta_code$LGO_Q3_2=="5"] <- "Disagree"
# delete dta_label$exp_SI[dta_code$LGO_Q3_2=="977"] <- "Don't know"
table(dta_label$exp_SI)
table(dta_label$LGO_Q3_3)

#for analysis: agree vs all others
dta_label$miss_social_agree <- NA
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="1"] <- "Agree"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="2"] <- "Agree"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="3"] <- "Other"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="4"] <- "Other"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="5"] <- "Other"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="977"] <- NA
table(dta_label$miss_social_agree)

#one var freq:
rowPerc(xtabs(dta_label$weight ~ dta_label$exp_SI))
wtd.table(x = dta_label$exp_SI, weights = dta_label$weight)

rowPerc(xtabs(dta_label$weight ~ dta_label$LGO_Q3_2))
wtd.table(x = dta_label$LGO_Q3_2, weights = dta_label$weight)

wpct(dta_label$miss_social_agree, weight=dta_label$weight)
wtd.table(dta_label$miss_social_agree, weights = dta_label$weight)
#sex
crosstab(dta_label$miss_social_agree, dta_label$sex, prop.c=T, weight = dta_label$weight, plot = F, chisq = T)
#social grade
crosstab(dta_label$miss_social_agree, dta_label$socialgrade, prop.c=T,  weight = dta_label$weight, plot = F, chisq = T)
#age
crosstab(dta_label$miss_social_agree, dta_label$age, prop.c=T, weight = dta_label$weight, plot = F, chisq = T)
#ethnicity
crosstab(dta_label$miss_social_agree, dta_label$ethnicity, prop.c=T,  weight = dta_label$weight, chisq = T, plot=F)
#dogs
crosstab(dta_label$miss_social_agree, dta_label$dogs, prop.c=T,  weight = dta_label$weight, chisq = T, plot=F)

library(descr)
crosstab(dta_label$age, dta_label$ethnicity, prop.c=T, weight=dta_label$weight,plot=F)
#**************************************************************

#Experience - change in PA#

#agree/disagree
dta_label$exp_PA <- NA
dta_label$exp_PA [dta_code$LGO_Q3_5=="1"] <- "Agree"
dta_label$exp_PA [dta_code$LGO_Q3_5=="2"] <- "Agree"
dta_label$exp_PA [dta_code$LGO_Q3_5=="3"] <- "Neither agree nor disagree"
dta_label$exp_PA [dta_code$LGO_Q3_5=="4"] <- "Disagree"
dta_label$exp_PA [dta_code$LGO_Q3_5=="5"] <- "Disagree"
# delete dta_label$exp_PA [dta_code$LGO_Q3_5=="977"] <- "Don't know"
table(dta_label$exp_PA)
table(dta_label$LGO_Q3_5)

# for analysis: Agree vs all others
dta_label$increase_PA_agree <- NA
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="1"] <- "Agree"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="2"] <- "Agree"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="3"] <- "Other"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="4"] <- "Other"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="5"] <- "Other"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="977"] <- NA
table(dta_label$increase_PA_agree)

#one var freq:
rowPerc(xtabs(dta_label$weight ~ dta_label$exp_PA))
wtd.table(x = dta_label$increase_PA_agree, weights = dta_label$weight)

wpct(dta_label$increase_PA_agree, weight=dta_label$weight)
wtd.table(dta_label$increase_PA_agree, weights = dta_label$weight)

#sex
crosstab(dta_label$increase_PA_agree, dta_label$sex, prop.c=T,  weight = dta_label$weight, plot = F, chisq = T)
#social grade
crosstab(dta_label$increase_PA_agree, dta_label$socialgrade, prop.c=T, weight = dta_label$weight, plot = F, chisq = T)
#age
crosstab(dta_label$increase_PA_agree, dta_label$age, prop.c=T, weight = dta_label$weight, plot = F, chisq = T)
#ethnicity
crosstab(dta_label$increase_PA_agree, dta_label$ethnicity, prop.c=T, weight = dta_label$weight, plot=F, chisq = T)
#dogs
crosstab(dta_label$increase_PA_agree, dta_label$dogs, prop.c=T,  weight = dta_label$weight, chisq = T, plot=F)


#Extra------------------------------------------------------------------------------------------------------------

#Weight tbl works
library(questionr)
wtd.table(x = dta_label$LGO_Q3_1, y = dta_label$profile_gender, weights = dta_code$weight)

class(dta_label$weight)
dta_label$weight <- as.numeric(dta_label$weight)


#Weight ex
library(survey)
dta_code.w <- svydesign(ids = ~1, data = dta_code, weights = dta_code$weight)
summary(dta_code.w)
prop.table(table(dta_code$profile_gender, dta_code$LGO_Q1))
prop.table(svytable(~profile_gender, ~LGO_Q1, design = dta_code.w))
class(dta_code.w)

#Pred prob
library(glm.predict)
library(effects)

#Bind variables into a dataframe (for LGM = visit before + all )
predprob_before <- cbind.data.frame(VISITBEFORE=dta_label$LGO_before, SEX=dta_label$sex, 
                                      SOCIALGRADE=dta_label$socialgrade, AGE=dta_label$age,
                                      ETHNICITY=dta_label$ethnicity, DOGS=dta_label$dogs, WEIGHT=dta_label$weight)
freq(predprob_before$VISITBEFORE)
freq(dta_label$LGO_before)
freq(predprob_before$SEX)
freq(dta_label$sex)
freq(predprob_before$SOCIALGRADE)
freq(dta_label$socialgrade)
freq(predprob_before$AGE)
freq(dta_label$age)
freq(predprob_before$ETHNICITY)
freq(dta_label$ethnicity)
freq(predprob_before$DOGS)
freq(dta_label$dogs)
#create dtaframe with complete cases only
visitbefore_comp <- predprob_before[complete.cases(predprob_before),]


#run final model
before_sgeth <- glm(visitbefore_comp$VISITBEFORE ~ visitbefore_comp$SEX + visitbefore_comp$DOGS +
                             visitbefore_comp$AGE + visitbefore_comp$ETHNICITY*visitbefore_comp$SOCIALGRADE, binomial, weights = visitbefore_comp$WEIGHT)
summary(before_sgeth)

#Create predicted values:
before_sgeth$visitpred <- predict.glm(before_sgeth, type = "response")
predict(before_sgeth, before_sgeth$visitpred, type="response")


basepredict(lm.before.adj, c("Yes, I did", "Female", "C2DE", "65+", "BAME", "Dog"), conf.int=0.95)

logitgof(visitbefore_comp$VISITBEFORE, visitbefore_comp$visitpred, g=10)
logitgof(visitbefore_comp$VISITBEFORE, visitbefore_comp$visitpred, g=8)
logitgof(visitbefore_comp$VISITBEFORE, visitbefore_comp$visitpred, g=12)
logitgof(visitbefore_comp$VISITBEFORE, visitbefore_comp$visitpred, g=20)


allEffects(before.sgeth.int)
test <- predictorEffect(before.sgeth.int)
