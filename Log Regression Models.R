

#Before starting...

library(foreign)
library(lmtest)

# Interaction 
#sg/ethnicity
lm.before.adj.int <- glm(dta_label$LGO_before ~ dta_label$sex + dta_label$socialgrade + dta_label$age
                     + dta_label$ethnicity + dta_label$socialgrade*dta_label$ethnicity,
                     family = binomial, weights = dta_label$weight)
summary(lm.before.adj.int)
exp(cbind(OR = coef(lm.before.adj.int), confint.default(lm.before.adj.int)))
lrtest(lm.before.adj, before.sgeth.int)


lm.before.sgsex.int <- glm(dta_label$LGO_before ~ dta_label$sex + dta_label$socialgrade + dta_label$age
                         + dta_label$ethnicity + dta_label$sex:dta_label$socialgrade,
                         family = binomial, weights = dta_label$weight)
summary(lm.before.sgsex.int)
exp(cbind(OR = coef(lm.before.sgsex.int), confint.default(lm.before.sgsex.int)))
lrtest(lm.before.adj, lm.before.sgsex.int)

#****************************
#Visiting GS after lockdown

#Visit = binary cats - yes/no
#Yes/No (don't know = missing)
dta_label$LGO_after <- NA
dta_label$LGO_after[dta_label$LGO_Q1=="Yes, I have"] <- "Yes"
dta_label$LGO_after[dta_label$LGO_Q1=="No, I haven't"] <- "No"
dta_label$LGO_after[dta_label$LGO_Q1=="Don't know/ can't recall"] <- NA
table(dta_label$LGO_after)
#make factor
dta_label$LGO_after <- as.factor(dta_label$LGO_after)
class(dta_label$LGO_after)
contrasts(dta_label$LGO_after)

#check NA/missing
sum(is.na(dta_label$LGO_after))
sum(is.na(dta_label$LGO_Q1))
table(dta_label$LGO_Q1)

#LGM - sex
contrasts(dta_label$sex)
lm.sex.after <- glm(dta_label$LGO_after ~ dta_label$sex, family = binomial, weights = dta_label$weight)
summary(lm.sex.after)
exp(cbind(OR = coef(lm.sex.after), confint.default(lm.sex.after)))

#LGM - social grade
lm.sg.after <- glm(dta_label$LGO_after ~ dta_label$socialgrade, family = binomial, weights = dta_label$weight)
summary(lm.sg.after)
exp(cbind(OR = coef(lm.sg.after), confint.default(lm.sg.after)))

#LGM - age
lm.age.after <- glm(dta_label$LGO_after ~ dta_label$age, family = binomial, weights = dta_label$weight)
summary(lm.age.after)
exp(cbind(OR = coef(lm.age.after), confint.default(lm.age.after)))

#LGM - ethnicity
lm.eth.after <- glm(dta_label$LGO_after ~ dta_label$ethnicity, family = binomial, weights = dta_label$weight)
summary(lm.eth.after)
exp(cbind(OR = coef(lm.eth.after), confint.default(lm.eth.after)))

#LGM - dog owners
lm.dogs.after <- glm(dta_label$LGO_after ~ dta_label$dogs, family = binomial, weights = dta_label$weight)
summary(lm.dogs.after)
exp(cbind(OR = coef(lm.dogs.after), confint.default(lm.dogs.after)))

#All
lm.after.adj <- glm(LGO_after ~ sex + social_grade + age
                     + ethnicity + 
                      dogs, family = binomial, weights = weight,
                    data = dta_label)
summary(lm.after.adj)
exp(cbind(OR = coef(lm.after.adj), confint.default(lm.after.adj)))
dim(dta_label)

#****plots - testing****works: (probability)
library(effects)
plot(allEffects(lm.after.adj), ci.style = "band", 
     ylab = "Visited after restrictions" )


#*********************
#Visiting GS after lockdown - haven't use (No)

#Visit = binary cats - yes/no
#Yes/No (don't know = missing)
dta_label$notused_after <- NA
dta_label$notused_after[dta_label$LGO_Q1=="Yes, I have"] <- "Yes"
dta_label$notused_after[dta_label$LGO_Q1=="No, I haven't"] <- "No"
dta_label$notused_after[dta_label$LGO_Q1=="Don't know/ can't recall"] <- NA
table(dta_label$notused_after)
#make factor
dta_label$notused_after <- as.factor(dta_label$notused_after)
class(dta_label$notused_after)
contrasts(dta_label$notused_after)

#Swap ref cats:
contrasts(dta_label$notused_after)
table(dta_label$notused_after)
sort(table(dta_label$notused_after), decreasing = TRUE)
names(sort(table(dta_label$notused_after), decreasing = TRUE))
dta_label$notused_after <- factor(dta_label$notused_after, 
                              levels = names(sort(table(dta_label$notused_after), decreasing = TRUE)))

#LGM - sex
contrasts(dta_label$sex)
lm.sex.notused <- glm(dta_label$notused_after ~ dta_label$sex, family = binomial, weights = dta_label$weight)
summary(lm.sex.notused)
exp(cbind(OR = coef(lm.sex.notused), confint.default(lm.sex.notused)))

#LGM - social grade
lm.sg.notused <- glm(dta_label$notused_after ~ dta_label$socialgrade, family = binomial, weights = dta_label$weight)
summary(lm.sg.notused)
exp(cbind(OR = coef(lm.sg.notused), confint.default(lm.sg.notused)))

#LGM - age
lm.age.notused <- glm(dta_label$notused_after ~ dta_label$age, family = binomial, weights = dta_label$weight)
summary(lm.age.notused)
exp(cbind(OR = coef(lm.age.notused), confint.default(lm.age.notused)))

#LGM - ethnicity
lm.eth.notused <- glm(dta_label$notused_after ~ dta_label$ethnicity, family = binomial, weights = dta_label$weight)
summary(lm.eth.notused)
exp(cbind(OR = coef(lm.eth.notused), confint.default(lm.eth.notused)))

#LGM - dog owners
lm.dogs.notused <- glm(dta_label$notused_after ~ dta_label$dogs, family = binomial, weights = dta_label$weight)
summary(lm.dogs.notused)
exp(cbind(OR = coef(lm.dogs.notused), confint.default(lm.dogs.notused)))

#All
lm.notused.adj <- glm(dta_label$notused_after ~ dta_label$sex + dta_label$socialgrade + dta_label$age
                    + dta_label$ethnicity + dta_label$dogs, family = binomial, weights = dta_label$weight)
summary(lm.notused.adj)
exp(cbind(OR = coef(lm.notused.adj), confint.default(lm.notused.adj)))



#*********************
#Increased use during lockdown?

#Increased vs others
dta_label$visit_increase <- NA
dta_label$visit_increase[dta_label$LGO_Q2=="Decreased a lot"] <- "Other"
dta_label$visit_increase[dta_label$LGO_Q2=="Decreased a little"] <- "Other"
dta_label$visit_increase[dta_label$LGO_Q2=="No difference"] <- "Other"
dta_label$visit_increase[dta_label$LGO_Q2=="Increased a little"] <- "Increased"
dta_label$visit_increase[dta_label$LGO_Q2=="Increased a lot"] <- "Increased"
dta_label$visit_increase[dta_label$LGO_Q2=="Don't know"] <- NA
table(dta_label$visit_increase)
is.na(dta_label$visit_increase)
#make factor
class(dta_label$visit_increase)
dta_label$visit_increase <- as.factor(dta_label$visit_increase)

#change order of increase
table(dta_label$visit_increase)
sort(table(dta_label$visit_increase), decreasing = TRUE)
names(sort(table(dta_label$visit_increase), decreasing = TRUE))
dta_label$visit_increase <- factor(dta_label$visit_increase, 
                                   levels = names(sort(table(dta_label$visit_increase), decreasing = TRUE)))
contrasts(dta_label$visit_increase)

#check NA/missing
sum(is.na(dta_label$visit_increase))
sum(is.na(dta_label$LGO_Q2))
table(dta_label$LGO_Q2)

#LGM  = visit inc + sex
contrasts(dta_label$sex)
lm.sex.increase <- glm(dta_label$visit_increase ~ dta_label$sex, family = binomial, weights = dta_label$weight)
summary(lm.sex.increase)
exp(cbind(OR = coef(lm.sex.increase), confint.default(lm.sex.increase)))

#LGM = visit inc + social grade
lm.sg.increase <- glm(dta_label$visit_increase ~ dta_label$socialgrade, family = binomial, weights = dta_label$weight)
summary(lm.sg.increase)
exp(cbind(OR = coef(lm.sg.increase), confint.default(lm.sg.increase)))

#LGM = visit inc + age
lm.age.increase <- glm(dta_label$visit_increase ~ dta_label$age, family = binomial, weights = dta_label$weight)
summary(lm.age.increase)
exp(cbind(OR = coef(lm.age.increase), confint.default(lm.age.increase)))
contrasts(dta_label$visit_increase)

#LGM = visit inc + ethnicity
lm.eth.increase <- glm(dta_label$visit_increase ~ dta_label$ethnicity, family = binomial, weights = dta_label$weight)
summary(lm.eth.increase)
exp(cbind(OR = coef(lm.eth.increase), confint.default(lm.eth.increase)))

#LGM - visit inc + dog owners
lm.dogs.increase <- glm(dta_label$visit_increase ~ dta_label$dogs, family = binomial, weights = dta_label$weight)
summary(lm.dogs.increase)
exp(cbind(OR = coef(lm.dogs.increase), confint.default(lm.dogs.increase)))

#LGM = all 
lm.increase.adj <- glm(visit_increase ~ sex + social_grade + age
                    + ethnicity + dogs, family = binomial, weights = weight,
                    data = dta_label)
summary(lm.increase.adj)
exp(cbind(OR = coef(lm.increase.adj), confint.default(lm.increase.adj)))

#****plots - testing****works: (probability)
library(effects)
plot(allEffects(lm.increase.adj), ci.style = "band", 
     ylab = "Increased use since restrictions" )

#****************************************************************************
#Visit decrease

#make new variable for visit decrease
dta_label$visit_decr <- NA
dta_label$visit_decr[dta_label$LGO_Q2=="Decreased a lot"] <- "Decreased"
dta_label$visit_decr[dta_label$LGO_Q2=="Decreased a little"] <- "Decreased"
dta_label$visit_decr[dta_label$LGO_Q2=="No difference"] <- "Other"
dta_label$visit_decr[dta_label$LGO_Q2=="Increased a little"] <- "Other"
dta_label$visit_decr[dta_label$LGO_Q2=="Increased a lot"] <- "Other"
dta_label$visit_decr[dta_label$LGO_Q2=="Don't know"] <- NA
table(dta_label$visit_decr)
table(dta_label$visit_increase)

#make factor
class(dta_label$visit_decr)
dta_label$visit_decr <- as.factor(dta_label$visit_decr)

#change order of decrease variable
table(dta_label$visit_decr)
sort(table(dta_label$visit_decr), increasing = TRUE)
names(sort(table(dta_label$visit_decr), increasing = TRUE))
dta_label$visit_decr <- factor(dta_label$visit_decr, 
                                   levels = names(sort(table(dta_label$visit_decr), increasing = TRUE)))
contrasts(dta_label$visit_decr)

#check NA/missing
sum(is.na(dta_label$visit_decr))
sum(is.na(dta_label$LGO_Q2))
table(dta_label$LGO_Q2)

#LGM  = visit dec + sex
contrasts(dta_label$sex)
lm.sex.decrease <- glm(dta_label$visit_decr ~ dta_label$sex, family = binomial, weights = dta_label$weight)
summary(lm.sex.decrease)
exp(cbind(OR = coef(lm.sex.decrease), confint.default(lm.sex.decrease)))

#LGM = visit dec + social grade
lm.sg.decrease <- glm(dta_label$visit_decr ~ dta_label$socialgrade, family = binomial, weights = dta_label$weight)
summary(lm.sg.decrease)
exp(cbind(OR = coef(lm.sg.decrease), confint.default(lm.sg.decrease)))

#LGM = visit dec + age
lm.age.decrease <- glm(dta_label$visit_decr ~ dta_label$age, family = binomial, weights = dta_label$weight)
summary(lm.age.decrease)
exp(cbind(OR = coef(lm.age.decrease), confint.default(lm.age.decrease)))


#LGM = visit dec + ethnicity
lm.eth.decrease <- glm(dta_label$visit_decr ~ dta_label$ethnicity, family = binomial, weights = dta_label$weight)
summary(lm.eth.decrease)
exp(cbind(OR = coef(lm.eth.decrease), confint.default(lm.eth.decrease)))

#LGM - visit dec + dog owners
lm.dogs.decrease <- glm(dta_label$visit_decr ~ dta_label$dogs, family = binomial, weights = dta_label$weight)
summary(lm.dogs.decrease)
exp(cbind(OR = coef(lm.dogs.decrease), confint.default(lm.dogs.decrease)))

#LGM = all 
lm.decrease.adj <- glm(visit_decr ~ sex + socialgrade + age
                       + ethnicity + dogs, family = binomial, data = dta_label, weights = weight)
summary(lm.decrease.adj)
exp(cbind(OR = coef(lm.decrease.adj), confint.default(lm.decrease.adj)))

#****plots - testing****works: (probability)
library(effects)
plot(allEffects(lm.decrease.adj), ci.style = "band", 
     ylab = "Decreased use since restrictions" )

#****************************************************************************
#Improve MH

#for analysis: agree vs all others
dta_label$mentalhealth_agree <- NA
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="1"] <- "Agree"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="2"] <- "Agree"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="3"] <- "Other"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="4"] <- "Other"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="5"] <- "Other"
dta_label$mentalhealth_agree[dta_code$LGO_Q3_1=="977"] <- NA
table(dta_label$mentalhealth_agree)
table(dta_label$LGO_Q3_1)

#Check NA/missing
sum(is.na(dta_label$mentalhealth_agree))
sum(is.na(dta_label$LGO_Q3_1))

#make factor
class(dta_label$mentalhealth_agree)
dta_label$mentalhealth_agree <- as.factor(dta_label$mentalhealth_agree)

#check order + change order
contrasts(dta_label$mentalhealth_agree)
table(dta_label$mentalhealth_agree)
sort(table(dta_label$mentalhealth_agree), increasing = TRUE)
names(sort(table(dta_label$mentalhealth_agree), increasing = TRUE))
dta_label$mentalhealth_agree <- factor(dta_label$mentalhealth_agree, 
                                   levels = names(sort(table(dta_label$mentalhealth_agree), increasing = TRUE)))
contrasts(dta_label$mentalhealth_agree)


#LGM = MH benefit + sex
lm.sex.MHbenefit <- glm(dta_label$mentalhealth_agree ~ dta_label$sex, family = binomial, weights = dta_label$weight)
summary(lm.sex.MHbenefit)
exp(cbind(OR = coef(lm.sex.MHbenefit), confint.default(lm.sex.MHbenefit)))

#LGM = MH benefit + social grade
lm.sg.MHbenefit <- glm(dta_label$mentalhealth_agree ~ dta_label$socialgrade, family = binomial, weights = dta_label$weight)
summary(lm.sg.MHbenefit)
exp(cbind(OR = coef(lm.sg.MHbenefit), confint.default(lm.sg.MHbenefit)))

#LGM = MH benefit + age
lm.age.MHbenefit <- glm(dta_label$mentalhealth_agree ~ dta_label$age, family = binomial, weights = dta_label$weight)
summary(lm.age.MHbenefit)
exp(cbind(OR = coef(lm.age.MHbenefit), confint.default(lm.age.MHbenefit)))

#LGM = MH benefit + ethnicity
lm.eth.MHbenefit <- glm(dta_label$mentalhealth_agree ~ dta_label$ethnicity, family = binomial, weights = dta_label$weight)
summary(lm.eth.MHbenefit)
exp(cbind(OR = coef(lm.eth.MHbenefit), confint.default(lm.eth.MHbenefit)))

#LGM - MH benefit + dog owners
lm.dogs.MHbenefit <- glm(dta_label$mentalhealth_agree ~ dta_label$dogs, family = binomial, weights = dta_label$weight)
summary(lm.dogs.MHbenefit)
exp(cbind(OR = coef(lm.dogs.MHbenefit), confint.default(lm.dogs.MHbenefit)))

#LGM = MH benefit + all
lm.MHbenefit.adj <- glm(mentalhealth_agree ~ sex + social_grade + age
                       + ethnicity + dogs, family = binomial, data = dta_label, weights = weight)
summary(lm.MHbenefit.adj)
exp(cbind(OR = coef(lm.MHbenefit.adj), confint.default(lm.MHbenefit.adj)))

#****plots - testing****works: (probability)
library(effects)
plot(allEffects(lm.MHbenefit.adj), ci.style = "band", 
     ylab = "Agree more MH benefit now" )

#*********************************************************************************
#Social interaction

#for analysis: agree vs all others
dta_label$miss_social_agree <- NA
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="1"] <- "Agree"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="2"] <- "Agree"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="3"] <- "Other"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="4"] <- "Other"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="5"] <- "Other"
dta_label$miss_social_agree[dta_code$LGO_Q3_2=="977"] <- NA
table(dta_label$miss_social_agree)
table(dta_code$LGO_Q3_2)
#make factor
class(dta_label$miss_social_agree)
dta_label$miss_social_agree <- as.factor(dta_label$miss_social_agree)

#check order + change
contrasts(dta_label$miss_social_agree)
table(dta_label$miss_social_agree)
sort(table(dta_label$miss_social_agree), increasing = TRUE)
names(sort(table(dta_label$miss_social_agree), increasing = TRUE))
dta_label$miss_social_agree <- factor(dta_label$miss_social_agree, 
                                       levels = names(sort(table(dta_label$miss_social_agree), increasing = TRUE)))
contrasts(dta_label$miss_social_agree)

#check NA/missing
sum(is.na(dta_label$miss_social_agree))
sum(is.na(dta_label$LGO_Q3_2))

# LGM = Miss social int + sex
lm.sex.socialint <- glm(dta_label$miss_social_agree ~ dta_label$sex, family = binomial, weights = dta_label$weight)
summary(lm.sex.socialint)
exp(cbind(OR = coef(lm.sex.socialint), confint.default(lm.sex.socialint)))

# LGM = Miss social int + social grade
lm.sg.socialint <- glm(dta_label$miss_social_agree ~ dta_label$socialgrade, family = binomial, weights = dta_label$weight)
summary(lm.sg.socialint)
exp(cbind(OR = coef(lm.sg.socialint), confint.default(lm.sg.socialint)))

# LGM = Miss social int + age
lm.age.socialint <- glm(dta_label$miss_social_agree ~ dta_label$age, family = binomial, weights = dta_label$weight)
summary(lm.age.socialint)
exp(cbind(OR = coef(lm.age.socialint), confint.default(lm.age.socialint)))

# LGM = Miss social int + ethnicity
lm.eth.socialint <- glm(dta_label$miss_social_agree ~ dta_label$ethnicity, family = binomial, weights = dta_label$weight)
summary(lm.eth.socialint)
exp(cbind(OR = coef(lm.eth.socialint), confint.default(lm.eth.socialint)))

#LGM - Miss social int + dog owners
lm.dogs.socialint <- glm(dta_label$miss_social_agree ~ dta_label$dogs, family = binomial, weights = dta_label$weight)
summary(lm.dogs.socialint)
exp(cbind(OR = coef(lm.dogs.socialint), confint.default(lm.dogs.socialint)))

# LGM = Miss social int + all
lm.socialint.adj <- glm(miss_social_agree ~ sex + socialgrade + age
                        + ethnicity + dogs, family = binomial, data = dta_label, weights = weight)
summary(lm.socialint.adj)
exp(cbind(OR = coef(lm.socialint.adj), confint.default(lm.socialint.adj)))

#****plots - testing****works: (probability)
library(effects)
plot(allEffects(lm.socialint.adj), ci.style = "band", 
     ylab = "Agree miss social int now" )

#*************************************************************************************
# Increased PA 

# for analysis: Agree vs all others
dta_label$increase_PA_agree <- NA
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="1"] <- "Agree"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="2"] <- "Agree"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="3"] <- "Other"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="4"] <- "Other"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="5"] <- "Other"
dta_label$increase_PA_agree [dta_code$LGO_Q3_5=="977"] <- NA
table(dta_label$increase_PA_agree)
table(dta_code$LGO_Q3_5)
#make factor
class(dta_label$increase_PA_agree)
dta_label$increase_PA_agree <- as.factor(dta_label$increase_PA_agree)

#check order + change
contrasts(dta_label$increase_PA_agree)
table(dta_label$increase_PA_agree)
sort(table(dta_label$increase_PA_agree), decreasing = TRUE)
names(sort(table(dta_label$increase_PA_agree), decreasing = TRUE))
dta_label$increase_PA_agree <- factor(dta_label$increase_PA_agree, 
                                      levels = names(sort(table(dta_label$increase_PA_agree), decreasing = TRUE)))
contrasts(dta_label$increase_PA_agree)

#Check NA/missing
sum(is.na(dta_label$increase_PA_agree))
sum(is.na(dta_label$LGO_Q3_5))

# LGM = increased PA + sex
lm.sex.increasePA <- glm(dta_label$increase_PA_agree ~ dta_label$sex, family = binomial, weights = dta_label$weight)
summary(lm.sex.increasePA)
exp(cbind(OR = coef(lm.sex.increasePA), confint.default(lm.sex.increasePA)))

# LGM = increased PA + social grade
lm.sg.increasePA <- glm(dta_label$increase_PA_agree ~ dta_label$socialgrade, family = binomial, weights = dta_label$weight)
summary(lm.sg.increasePA)
exp(cbind(OR = coef(lm.sg.increasePA), confint.default(lm.sg.increasePA)))

# LGM = increased PA + age
lm.age.increasePA <- glm(dta_label$increase_PA_agree ~ dta_label$age, family = binomial, weights = dta_label$weight)
summary(lm.age.increasePA)
exp(cbind(OR = coef(lm.age.increasePA), confint.default(lm.age.increasePA)))

# LGM = increased PA + ethnicity
lm.eth.increasePA <- glm(dta_label$increase_PA_agree ~ dta_label$ethnicity, family = binomial, weights = dta_label$weight)
summary(lm.eth.increasePA)
exp(cbind(OR = coef(lm.eth.increasePA), confint.default(lm.eth.increasePA)))

#LGM - increase PA + dog owners
lm.dogs.increasePA <- glm(dta_label$increase_PA_agree ~ dta_label$dogs, family = binomial, weights = dta_label$weight)
summary(lm.dogs.increasePA)
exp(cbind(OR = coef(lm.dogs.increasePA), confint.default(lm.dogs.increasePA)))

# LGM = increased PA + all
lm.increasePA.adj <- glm(increase_PA_agree ~ sex + socialgrade + age
                        + ethnicity + dogs, family = binomial, data = dta_label, weights = dta_label$weight)
summary(lm.increasePA.adj)
exp(cbind(OR = coef(lm.increasePA.adj), confint.default(lm.increasePA.adj)))

#****plots - testing****works: (probability)
library(effects)
plot(allEffects(lm.increasePA.adj), ci.style = "band", 
     ylab = "Agree do more PA now" )

#******************************************************************
#Didn't visit before but are now 
crosstab(dta_label$LGO_Q1a, dta_label$LGO_Q1, prop.c=T, weight = dta_label$weight, chisq = T, fisher=T)

table(dta_label$LGO_before)
dta_label$before <- NA
dta_label$before[dta_label$LGO_before=="Yes, I did"] <- "1"
dta_label$before[dta_label$LGO_before=="No, I didn't"] <- "4"

dta_label$after <- NA
dta_label$after[dta_label$LGO_after=="Yes"] <- "3"
dta_label$after[dta_label$LGO_after=="No"] <- "2"
table(dta_label$after)

dta_label$before_after <- dta_label$before + dta_label$after
class(dta_label$before)
dta_label$before <- as.numeric(dta_label$before)
dta_label$after <- as.numeric(dta_label$after)
class(dta_label$after)
table(dta_label$before_after)
dta_label$nowuse <- NA
dta_label$nowuse[dta_label$before_after<"7"] <- "No"
dta_label$nowuse[dta_label$before_after=="7"] <- "Now use"
table(dta_label$nowuse)

crosstab(dta_label$nowuse, dta_label$sex, prop.c=T, weight = dta_label$weight, chisq = T, plot = F)

#############################################################################
#Other edits: 

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

#Change age cats
dta_label$age2 <- dta_label$age
dta_label$age <- dta_label_age_$profile_julesage2
table(dta_label$age)
table(dta_label$age2)

#make age cat (younger,middle,older):
dta_label$age <- NA
dta_label$age[dta_label$age2=="18-24"] <- "18-24"
dta_label$age[dta_label$age2=="25-34"] <- "25-64"
dta_label$age[dta_label$age2=="35-44"] <- "25-64"
dta_label$age[dta_label$age2=="45-54"] <- "25-64"
dta_label$age[dta_label$age2=="55-64"] <- "25-64"
dta_label$age[dta_label$age2=="65+"] <- "65+"
table(dta_label$age)
#make factor
dta_label$age <- as.factor(dta_label$age)
class(dta_label$age)
#check which will be reference
contrasts(dta_label$age)
table(dta_label$age)
dta_label$age <- relevel(dta_label$age, ref = "25-64")
contrasts(dta_label$age)

#make/check dog cat
table(dta_label$pets_rcy)
class(dta_label$pets_rcy)
dta_label$pets_rcy <- as.factor(dta_label$pets_rcy)
dta_label$dogs <- NA
dta_label$dogs[dta_label$pets_rcy=="Dog owners"] <- "Dog"
dta_label$dogs[dta_label$pets_rcy=="NA"] <- "None"
table(dta_label$dogs)
dta_label$dogs[is.na(dta_label$dogs)] <- "None"
#Make factor
class(dta_label$dogs)
dta_label$dogs <- as.factor(dta_label$dogs)
contrasts(dta_label$dogs)
#Swap ref cats
table(dta_label$dogs)
sort(table(dta_label$dogs), decreasing = TRUE)
names(sort(table(dta_label$dogs), decreasing = TRUE))
dta_label$dogs <- factor(dta_label$dogs, 
                        levels = names(sort(table(dta_label$dogs), decreasing = TRUE)))
contrasts(dta_label$dogs)

#new social grade cat for plots
dta_label$social_grade <- NA
dta_label$social_grade[dta_label$socialgrade=="ABC1"] <- "Higher SG"
dta_label$social_grade[dta_label$socialgrade=="C2DE"] <- "Lower SG"
table(dta_label$social_grade)
dta_label$social_grade <- as.factor(dta_label$social_grade)
contrasts(dta_label$socialgrade)
contrasts(dta_label$social_grade)

#2: Log Reg Models***********************
library(generalhoslem)
library(stats)
library(descr)

#Visit GS before lockdown

#Visit = change to binary cats - Yes/No (don't know = missing)
dta_label$LGO_before <- NA
dta_label$LGO_before[dta_label$LGO_Q1a=="Yes, I did"] <- "Yes, I did"
dta_label$LGO_before[dta_label$LGO_Q1a=="No, I didn't"] <- "No, I didn't"
dta_label$LGO_before[dta_label$LGO_Q1a=="Don't know/ can't recall"] <- NA
table(dta_label$LGO_before)
#make factor
dta_label$LGO_before <- as.factor(dta_label$LGO_before)
class(dta_label$LGO_before)
contrasts(dta_label$LGO_before)

#check NA/missing
sum(is.na(dta_label$LGO_before))
sum(is.na(dta_label$LGO_Q1a))
table(dta_label$LGO_Q1a)


#Sex = turn into factor
table(dta_label$profile_gender)
dta_label$sex <- NA
dta_label$sex[dta_label$profile_gender=="Female"] <- "Female"
dta_label$sex[dta_label$profile_gender=="Male"] <- "Male"
table(dta_label$sex)
#make factor
dta_label$sex <- as.factor(dta_label$sex)
class(dta_label$sex)

# Swap ref cats:
# Sex
table(dta_label$sex)
sort(table(dta_label$sex), increasing = TRUE)
names(sort(table(dta_label$sex), increasing = TRUE))
dta_label$sex <- factor(dta_label$sex, 
                        levels = names(sort(table(dta_label$sex), increasing = TRUE)))
contrasts(dta_label$sex)

#LGM- Sex
#check which will be reference
contrasts(dta_label$sex) 
#model
lm.sex.before <- glm(dta_label$LGO_before ~ dta_label$sex, family = binomial, weights = dta_label$weight)
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
lm.sg.before <- glm(dta_label$LGO_before ~ dta_label$socialgrade, family = binomial, weights = dta_label$weight)
summary(lm.sg.before)
exp(cbind(OR = coef(lm.sg.before), confint.default(lm.sg.before)))

#Age = factor
#class(dta_label$profile_julesage)
#table(dta_label$profile_julesage2)
#dta_label$age <- NA
#dta_label$age[dta_label$profile_julesage2=="18-24"] <- "18-24"
#dta_label$age[dta_label$profile_julesage2=="25-34"] <- "25-34"
#dta_label$age[dta_label$profile_julesage2=="35-44"] <- "35-44"
#dta_label$age[dta_label$profile_julesage2=="45-54"] <- "45-54"
#dta_label$age[dta_label$profile_julesage2=="55-64"] <- "55-64"
#dta_label$age[dta_label$profile_julesage2=="65+"] <- "65+"
table(dta_label$age)
#make factor
dta_label$age <- as.factor(dta_label$age)
class(dta_label$age)

#LGM - age
contrasts(dta_label$age)
lm.age.before <- glm(dta_label$LGO_before ~ dta_label$age, family = binomial, weights = dta_label$weight)
summary(lm.age.before)
exp(cbind(OR = coef(lm.age.before), confint.default(lm.age.before)))

#Ethnicity = factor
class(dta_label$ethnicity)
table(dta_label$ethnicity)
dta_label$ethnicity <- as.factor(dta_label$ethnicity)

#Swap ref cats:
# Ethnicity
contrasts(dta_label$ethnicity)
table(dta_label$ethnicity)
sort(table(dta_label$ethnicity), decreasing = TRUE)
names(sort(table(dta_label$ethnicity), decreasing = TRUE))
dta_label$ethnicity <- factor(dta_label$ethnicity, 
                              levels = names(sort(table(dta_label$ethnicity), decreasing = TRUE)))
contrasts(dta_label$ethnicity)

#LGM - ethnicity
contrasts(dta_label$ethnicity)
lm.eth.before <- glm(dta_label$LGO_before ~ dta_label$ethnicity, family = binomial, weights = dta_label$weight)
summary(lm.eth.before)
exp(cbind(OR = coef(lm.eth.before), confint.default(lm.eth.before)))

#LGM - dog owners
contrasts(dta_label$dogs)
lm.dogs.before <- glm(dta_label$LGO_before ~ dta_label$dogs, family = binomial, weights = dta_label$weight)
summary(lm.dogs.before)
exp(cbind(OR = coef(lm.dogs.before), confint.default(lm.dogs.before)))

#LGM = visit before + All
lm.before.adj <- glm(LGO_before ~ sex + social_grade + age
                     + ethnicity + dogs,
                     family = binomial, weights = dta_label$weight, data=dta_label)
summary(lm.before.adj)
exp(cbind(OR = coef(lm.before.adj), confint.default(lm.before.adj)))

#****plots - testing****works: (probability)
library(effects)
plot(allEffects(lm.before.adj), ci.style = "band", 
     ylab = "Visited before restrictions" )
plot(lm.before.adj)

#but need weights below:
library(finalfit)
library(dplyr)
library(ggplot2)
data(dta_label)
explanatory = c("sex", "age", "socialgrade", "ethnicity", "dogs")
dependent = "LGO_before"
dta_label %>%
  or_plot(dependent, explanatory)

dta_label %>%
  or_plot(dependent = "LGO_before", explanatory = c("sex", "age", "socialgrade", "ethnicity", "dogs"), weights = weight)
plot(allEffects(lm.before.adj))


