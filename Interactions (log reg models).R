
#Interactions
library(lmtest)

#Before lockdown

#sex:age
before.sexage.int <- glm(LGO_before ~ socialgrade + ethnicity
                         + dogs + sex*age,
                         family = binomial, weights = weight, data=dta_label)
summary(before.sexage.int)
anova(lm.before.adj, before.sexage.int, test="Chi")
lrtest(lm.before.adj, before.sexage.int)
allEffects(before.sexage.int)

#sex:ethnicity
before.sexeth.int <- glm(LGO_before ~  socialgrade + age
                          + dogs + sex*ethnicity,
                         family = binomial, weights = weight, data=dta_label)
summary(before.sexeth.int)
anova(lm.before.adj, before.sexeth.int, test="Chi")
allEffects(before.sexeth.int)


#sex:sg
before.sgsex.int <- glm(LGO_before ~  age + ethnicity 
                        + dogs + sex*socialgrade,
                        family = binomial, weights = weight, data=dta_label)
summary(before.sgsex.int)
anova(lm.before.adj, before.sgsex.int, test="Chi")


#sg:ethnicity
before.sgeth.int <- glm(LGO_before ~ sex  + age
                         + dogs + socialgrade*ethnicity,
                         family = binomial, weights = weight, data=dta_label)
summary(before.sgeth.int)
anova(lm.before.adj, before.sgeth.int, test="Chi")

#sg:age
before.sgage.int <- glm(LGO_before ~ sex  + ethnicity
                        + dogs + socialgrade*age,
                         family = binomial, weights = weight, data=dta_label)
summary(before.sgage.int)
anova(lm.before.adj, before.sgage.int, test="Chi")

#age:eth
before.ageeth.int <- glm(LGO_before ~ sex + socialgrade
                         + dogs + age*ethnicity,
                        family = binomial, weights = weight, data=dta_label)
summary(before.ageeth.int)
anova(lm.before.adj, before.ageeth.int, test="Chi")
allEffects(before.ageeth.int)

#sex:dog owner
before.sexdog.int <- glm(LGO_before ~  age + ethnicity 
                         + socialgrade + sex*dogs,
                         family = binomial, weights = weight, data=dta_label)
summary(before.sexdog.int)
anova(lm.before.adj, before.sexdog.int, test="Chi")

#sg : dogs
before.sgdog.int <- glm(LGO_before ~  age + ethnicity 
                         + sex + socialgrade*dogs,
                         family = binomial, weights = weight, data=dta_label)
summary(before.sgdog.int)
anova(lm.before.adj, before.sgdog.int, test="Chi")

#age: dogs
before.agedog.int <- glm(LGO_before ~  socialgrade + ethnicity 
                        + sex + age*dogs,
                        family = binomial, weights = weight, data=dta_label)
summary(before.agedog.int)
anova(lm.before.adj, before.agedog.int, test="Chi")

#eth: dogs
before.ethdog.int <- glm(LGO_before ~  socialgrade + age 
                         + sex + ethnicity*dogs,
                         family = binomial, weights = weight, data=dta_label)
summary(before.ethdog.int)
anova(lm.before.adj, before.ethdog.int, test="Chi")


# After lockdown ***********************************************************
dta_label$LGO_after <- as.factor(dta_label$LGO_after)
class(dta_label$LGO_after)

# sex:sg

after.sexsg.int <- glm(LGO_after ~  age
                         + ethnicity + dogs + sex*socialgrade,
                         family = binomial, weights = weight, data=dta_label)
summary(after.sexsg.int)
anova(lm.after.adj, after.sexsg.int, test="Chi")
#lrtest(lm.after.adj, after.sexsg.int)

# sex:age
after.sexage.int <- glm(LGO_after ~ socialgrade
                       + ethnicity + dogs + sex*age,
                       family = binomial, weights = weight, data=dta_label)
summary(after.sexage.int)
anova(lm.after.adj, after.sexage.int , test="Chi")


#sex:ethnicity
after.sexeth.int <- glm(LGO_after ~ socialgrade + age
                         + dogs + sex*ethnicity,
                         family = binomial, weights = weight, data=dta_label)
summary(after.sexeth.int)
anova(lm.after.adj, after.sexeth.int, test="Chi")

#sg:ethnicity
after.sgeth.int <- glm(LGO_after ~ sex + age
                        + dogs + socialgrade*ethnicity,
                        family = binomial, weights = weight, data=dta_label)
summary(after.sgeth.int)
anova(lm.after.adj, after.sgeth.int, test="Chi")
allEffects(after.sgeth.int)


#sg:age
after.sgage.int <- glm(LGO_after ~ sex 
                        + ethnicity + dogs + socialgrade*age,
                        family = binomial, weights = weight, data=dta_label)
summary(after.sgage.int)
anova(lm.after.adj, after.sgage.int, test="Chi")

#lrtest(lm.after.adj, after.sgage.int)

#age:eth
after.ageeth.int <- glm(LGO_after ~ sex + socialgrade + dogs
                        + age*ethnicity,
                         family = binomial, weights = weight, data=dta_label)
summary(after.ageeth.int)
anova(lm.after.adj, after.ageeth.int, test="Chi")



#sex:dog owner
after.sexdog.int <- glm(LGO_after ~  age + ethnicity 
                         + socialgrade + sex*dogs,
                         family = binomial, weights = weight, data=dta_label)
summary(after.sexdog.int)
anova(lm.after.adj, after.sexdog.int, test="Chi")

#sg : dogs
after.sgdog.int <- glm(LGO_after ~  age + ethnicity 
                        + sex + socialgrade*dogs,
                        family = binomial, weights = weight, data=dta_label)
summary(after.sgdog.int)
anova(lm.after.adj, after.sgdog.int, test="Chi")

#age: dogs
after.agedog.int <- glm(LGO_after ~  sex + ethnicity 
                        + socialgrade + age*dogs,
                        family = binomial, weights = weight, data=dta_label)
summary(after.agedog.int)
anova(lm.after.adj, after.agedog.int, test="Chi")

#eth: dogs
after.ethdog.int <- glm(LGO_after ~  age + sex 
                        + socialgrade + ethnicity*dogs,
                        family = binomial, weights = weight, data=dta_label)
summary(after.ethdog.int)
anova(lm.after.adj, after.ethdog.int, test="Chi")


#******************************************
#Increased after lockdown?
dta_label$visit_increase <- as.factor(dta_label$visit_increase)
class(dta_label$visit_increase)

# sex:sg

increase.sexsg.int <- glm(visit_increase ~  age
                       + ethnicity + dogs + sex*socialgrade,
                       family = binomial, weights = weight, data=dta_label)
summary(increase.sexsg.int)
anova(lm.increase.adj, increase.sexsg.int, test="Chi")

# sex:age
increase.sexage.int <- glm(visit_increase ~ socialgrade 
                        + ethnicity + dogs + sex*age,
                        family = binomial, weights = weight, data=dta_label)
summary(increase.sexage.int)
anova(lm.increase.adj, increase.sexage.int, test="Chi")


#sex:ethnicity
increase.sexeth.int <- glm(visit_increase ~  socialgrade + age
                        +  dogs + sex*ethnicity,
                        family = binomial, weights = weight, data=dta_label)
summary(increase.sexeth.int)
anova(lm.increase.adj, increase.sexeth.int, test="Chi")

#sg:ethnicity
increase.sgeth.int <- glm(visit_increase ~ sex +  age
                       + dogs + socialgrade*ethnicity,
                       family = binomial, weights = weight, data=dta_label)
summary(increase.sgeth.int)
anova(lm.increase.adj, increase.sgeth.int, test="Chi")
confint(increase.sgeth.int, level=.95)


#sg:age
increase.sgage.int <- glm(visit_increase ~ sex 
                       + ethnicity + dogs + socialgrade*age,
                       family = binomial, weights = weight, data=dta_label)
summary(increase.sgage.int)
anova(lm.increase.adj, increase.sgage.int, test="Chi")

#age:eth
increase.ageeth.int <- glm(visit_increase ~ sex + socialgrade
                           + dogs + age*ethnicity,
                        family = binomial, weights = weight,data=dta_label)
summary(increase.ageeth.int)
anova(lm.increase.adj, increase.ageeth.int, test="Chi")


table(dta_label$age)

#sex:dog owner
increase.sexdog.int <- glm(visit_increase ~  age + ethnicity 
                        + socialgrade + sex*dogs,
                        family = binomial, weights = weight, data=dta_label)
summary(increase.sexdog.int)
anova(lm.increase.adj, increase.sexdog.int, test="Chi")

# sg:dog
increase.sgdog.int <- glm(visit_increase ~  age + ethnicity 
                           + sex + socialgrade*dogs,
                           family = binomial, weights = weight, data=dta_label)
summary(increase.sgdog.int)
anova(lm.increase.adj, increase.sgdog.int, test="Chi")

# age:dog
increase.agedog.int <- glm(visit_increase ~  socialgrade + ethnicity 
                          + sex + age*dogs,
                          family = binomial, weights = weight, data=dta_label)
summary(increase.agedog.int)
anova(lm.increase.adj, increase.agedog.int, test="Chi")

#eth:dog
increase.ethdog.int <- glm(visit_increase ~  socialgrade + age 
                           + sex + ethnicity*dogs,
                           family = binomial, weights = weight, data=dta_label)
summary(increase.ethdog.int)
anova(lm.increase.adj, increase.ethdog.int, test="Chi")

# Decreased after lockdown ***********************************************************

# sex:sg

decr.sexsg.int <- glm(visit_decr ~  age
                      + ethnicity + dogs + sex*socialgrade,
                      family = binomial, weights = weight, data=dta_label)
summary(decr.sexsg.int)
anova(lm.decrease.adj, decr.sexsg.int, test="Chi")

# sex:age
decr.sexage.int <- glm(visit_decr ~ socialgrade 
                       + ethnicity + dogs + sex*age,
                       family = binomial, weights = weight, data=dta_label)
summary(decr.sexage.int)
anova(lm.decrease.adj, decr.sexage.int, test="Chi")


#sex:ethnicity
decr.sexeth.int <- glm(visit_decr ~ + socialgrade + age
                        + dogs + sex*ethnicity,
                       family = binomial, weights = weight, data=dta_label)
summary(decr.sexeth.int)
anova(lm.decrease.adj, decr.sexeth.int, test="Chi")

#sg:ethnicity
decr.sgeth.int <- glm(visit_decr ~ sex + age
                      + dogs + socialgrade*ethnicity,
                      family = binomial, weights = weight, data=dta_label)
summary(decr.sgeth.int)
anova(lm.decrease.adj, decr.sgeth.int, test="Chi")


#sg:age
decr.sgage.int <- glm(visit_decr ~ sex 
                      + ethnicity + dogs + socialgrade*age,
                      family = binomial, weights = weight, data=dta_label)
summary(decr.sgage.int)
anova(lm.decrease.adj, decr.sgage.int, test="Chi")

#age:eth
decr.ageeth.int <- glm(visit_decr ~ sex + socialgrade + 
                         dogs + age*ethnicity,
                       family = binomial, weights = weight, data=dta_label)
summary(decr.ageeth.int)
anova(lm.decrease.adj, decr.ageeth.int, test="Chi")
allEffects(decr.ageeth.int)


table(dta_label$age)

#sex:dog owner
decr.sexdog.int <- glm(visit_decr ~  age + ethnicity 
                       + socialgrade + sex*dogs,
                       family = binomial, weights = weight, data=dta_label)
summary(decr.sexdog.int)
anova(lm.decrease.adj, decr.sexdog.int, test="Chi")

# sg:dog
decr.sgdog.int <- glm(visit_decr ~  age + ethnicity 
                      + sex + socialgrade*dogs,
                      family = binomial, weights = weight, data=dta_label)
summary(decr.sgdog.int)
anova(lm.decrease.adj, decr.sgdog.int, test="Chi")

# age:dog
decr.agedog.int <- glm(visit_decr ~  socialgrade + ethnicity 
                       + sex + age*dogs,
                       family = binomial, weights = weight, data=dta_label)
summary(decr.agedog.int)
anova(lm.decrease.adj, decr.agedog.int, test="Chi")

#eth:dog
decr.ethdog.int <- glm(visit_decr ~  socialgrade + age 
                       + sex + ethnicity*dogs,
                       family = binomial, weights = weight, data=dta_label)
summary(decr.ethdog.int)
anova(lm.decrease.adj, decr.ethdog.int, test="Chi")


# MH benefit***********************************************************

# sex:sg

MH.sexsg.int <- glm(mentalhealth_agree ~  age
                    + ethnicity + dogs + sex*socialgrade,
                    family = binomial, weights = weight, data=dta_label)
summary(MH.sexsg.int)
anova(lm.MHbenefit.adj, MH.sexsg.int, test="Chi")

# sex:age
MH.sexage.int <- glm(mentalhealth_agree ~ socialgrade 
                     + ethnicity + dogs + sex*age,
                     family = binomial, weights = weight, data=dta_label)
summary(MH.sexage.int)
anova(lm.MHbenefit.adj, MH.sexage.int, test="Chi")


#sex:ethnicity
MH.sexeth.int <- glm(mentalhealth_agree ~  socialgrade + age
                      + dogs + sex*ethnicity,
                     family = binomial, weights = weight, data=dta_label)
summary(MH.sexeth.int)
anova(lm.MHbenefit.adj, MH.sexeth.int, test="Chi")

#sg:ethnicity
MH.sgeth.int <- glm(mentalhealth_agree ~ sex + age
                    + dogs + socialgrade*ethnicity,
                    family = binomial, weights = weight, data=dta_label)
summary(MH.sgeth.int)
anova(lm.MHbenefit.adj, MH.sgeth.int, test="Chi")


#sg:age
MH.sgage.int <- glm(mentalhealth_agree ~ sex + 
                      ethnicity + dogs + socialgrade*age,
                    family = binomial, weights = weight, data=dta_label)
summary(MH.sgage.int)
anova(lm.MHbenefit.adj, MH.sgage.int, test="Chi")

#age:eth
MH.ageeth.int <- glm(mentalhealth_agree ~ sex + socialgrade 
                     + dogs + age*ethnicity,
                     family = binomial, weights = weight, data=dta_label)
summary(MH.ageeth.int)
anova(lm.MHbenefit.adj, MH.ageeth.int, test="Chi")

table(dta_label$age)

#sex:dog owner
MH.sexdog.int <- glm(mentalhealth_agree ~  age + ethnicity 
                     + socialgrade + sex*dogs,
                     family = binomial, weights = weight, data=dta_label)
summary(MH.sexdog.int)
anova(lm.MHbenefit.adj, MH.sexdog.int, test="Chi")

# sg:dog
MH.sgdog.int <- glm(mentalhealth_agree ~  age + ethnicity 
                    + sex + socialgrade*dogs,
                    family = binomial, weights = weight, data=dta_label)
summary(MH.sgdog.int)
anova(lm.MHbenefit.adj, MH.sgdog.int, test="Chi")

# age:dog
MH.agedog.int <- glm(mentalhealth_agree ~  socialgrade + ethnicity 
                     + sex + age*dogs,
                     family = binomial, weights = weight, data=dta_label)
summary(MH.agedog.int)
anova(lm.MHbenefit.adj, MH.agedog.int, test="Chi")

#eth:dog
MH.ethdog.int <- glm(mentalhealth_agree ~  socialgrade + age 
                     + sex + ethnicity*dogs,
                     family = binomial, weights = weight, data=dta_label)
summary(MH.ethdog.int)
anova(lm.MHbenefit.adj, MH.ethdog.int, test="Chi")

# Social int***********************************************************

# sex:sg

SI.sexsg.int <- glm(miss_social_agree ~  age
                    + ethnicity + dogs + sex*socialgrade,
                    family = binomial, weights = weight, data=dta_label)
summary(SI.sexsg.int)
anova(lm.socialint.adj, SI.sexsg.int, test="Chi")

# sex:age
SI.sexage.int <- glm(miss_social_agree ~ socialgrade 
                     + ethnicity + dogs + sex*age,
                     family = binomial, weights = weight, data=dta_label)
summary(SI.sexage.int)
anova(lm.socialint.adj, SI.sexage.int, test="Chi")


#sex:ethnicity
SI.sexeth.int <- glm(miss_social_agree ~ socialgrade + age
                     + dogs + sex*ethnicity,
                     family = binomial, weights = weight, data=dta_label)
summary(SI.sexeth.int)
anova(lm.socialint.adj, SI.sexeth.int, test="Chi")

#sg:ethnicity
SI.sgeth.int <- glm(miss_social_agree ~ sex +  age
                     + dogs + socialgrade*ethnicity,
                    family = binomial, weights = weight, data=dta_label)
summary(SI.sgeth.int)
anova(lm.socialint.adj, SI.sgeth.int, test="Chi")


#sg:age
SI.sgage.int <- glm(miss_social_agree ~ sex + 
                      ethnicity + dogs + socialgrade*age,
                    family = binomial, weights = weight, data=dta_label)
summary(SI.sgage.int)
anova(lm.socialint.adj, SI.sgage.int, test="Chi")

#age:eth
SI.ageeth.int <- glm(miss_social_agree ~ sex + socialgrade 
                     + dogs + age*ethnicity,
                     family = binomial, weights = weight, data=dta_label)
summary(SI.ageeth.int)
anova(lm.socialint.adj, SI.ageeth.int, test="Chi")

table(dta_label$age)

#sex:dog owner
SI.sexdog.int <- glm(miss_social_agree ~  age + ethnicity 
                     + socialgrade + sex*dogs,
                     family = binomial, weights = weight, data=dta_label)
summary(SI.sexdog.int)
anova(lm.socialint.adj, SI.sexdog.int, test="Chi")

# sg:dog
SI.sgdog.int <- glm(miss_social_agree ~  age + ethnicity 
                    + sex + socialgrade*dogs,
                    family = binomial, weights = weight, data=dta_label)
summary(SI.sgdog.int)
anova(lm.socialint.adj, SI.sgdog.int, test="Chi")

# age:dog
SI.agedog.int <- glm(miss_social_agree ~  socialgrade + ethnicity 
                     + sex + age*dogs,
                     family = binomial, weights = weight, data=dta_label)
summary(SI.agedog.int)
anova(lm.socialint.adj, SI.agedog.int, test="Chi")

#eth:dog
SI.ethdog.int <- glm(miss_social_agree ~  socialgrade + age 
                     + sex + ethnicity*dogs,
                     family = binomial, weights = weight, data=dta_label)
summary(SI.ethdog.int)
anova(lm.socialint.adj, SI.ethdog.int, test="Chi")

# Increase PA ***********************************************************

# sex*sg

PA.sexsg.int <- glm(increase_PA_agree ~  age
                    + ethnicity + dogs + sex*socialgrade,
                    family = binomial, weights = weight, data=dta_label)
summary(PA.sexsg.int)
anova(lm.increasePA.adj, PA.sexsg.int, test="Chi")

# sex*age
PA.sexage.int <- glm(increase_PA_agree ~  socialgrade 
                     + ethnicity + dogs + sex*age,
                     family = binomial, weights = weight, data=dta_label)
summary(PA.sexage.int)
anova(lm.increasePA.adj, PA.sexage.int, test="Chi")


#sex*ethnicity
PA.sexeth.int <- glm(increase_PA_agree ~  socialgrade + age
                     + dogs + sex*ethnicity,
                     family = binomial, weights = weight, data=dta_label)
summary(PA.sexeth.int)
anova(lm.increasePA.adj, PA.sexeth.int, test="Chi")

#sg*ethnicity
PA.sgeth.int <- glm(increase_PA_agree ~ sex +  age
                    + dogs + socialgrade*ethnicity,
                    family = binomial, weights = weight, data=dta_label)
summary(PA.sgeth.int)
anova(lm.increasePA.adj, PA.sgeth.int, test="Chi")


#sg*age
PA.sgage.int <- glm(increase_PA_agree ~ sex 
                    + ethnicity + dogs + socialgrade*age,
                    family = binomial, weights = weight, data=dta_label)
summary(PA.sgage.int)
anova(lm.increasePA.adj, PA.sgage.int, test="Chi")

#age*eth
PA.ageeth.int <- glm(increase_PA_agree ~ sex + socialgrade + 
                       dogs + age*ethnicity,
                     family = binomial, weights = weight, data=dta_label)
summary(PA.ageeth.int)
anova(lm.increasePA.adj, PA.ageeth.int, test="Chi")

table(age)

#sex*dog owner
PA.sexdog.int <- glm(increase_PA_agree ~  age + ethnicity 
                     + socialgrade + sex*dogs,
                     family = binomial, weights = weight, data=dta_label)
summary(PA.sexdog.int)
anova(lm.increasePA.adj, PA.sexdog.int, test="Chi")

# sg*dog
PA.sgdog.int <- glm(increase_PA_agree ~  age + ethnicity 
                    + sex + socialgrade*dogs,
                    family = binomial, weights = weight, data=dta_label)
summary(PA.sgdog.int)
anova(lm.increasePA.adj, PA.sgdog.int, test="Chi")

# age*dog
PA.agedog.int <- glm(increase_PA_agree ~  socialgrade + ethnicity 
                     + sex + age*dogs,
                     family = binomial, weights = weight, data=dta_label)
summary(PA.agedog.int)
anova(lm.increasePA.adj, PA.agedog.int, test="Chi")

#eth*dog
PA.ethdog.int <- glm(increase_PA_agree ~  socialgrade + age 
                     + sex + ethnicity*dogs,
                     family = binomial, weights = weight, data=dta_label)
summary(PA.ethdog.int)
anova(lm.increasePA.adj, PA.ethdog.int, test="Chi")

