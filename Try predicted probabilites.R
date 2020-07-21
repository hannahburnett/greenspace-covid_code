#Natalie code for pred prob of interactions

#Before = social grade:ethnicity 
library(effects)
before.sgeth.int <- glm(LGO_before ~ sex + age + dogs + socialgrade*ethnicity,
                        family = binomial, weights = weight, data = dta_label)
summary(before.sgeth.int)

allEffects(before.sgeth.int)
before.sgeth.pred <- effect("socialgrade:ethnicity", before.sgeth.int)
before.sgeth.pred
#plot effects
plot(allEffects(before.sgeth.int))
exp(cbind(OR = coef(before.sgeth.pred), confint.default(before.sgeth.pred)))
summary(before.sgeth.pred)

predf <- before.sgeth.pred$fit
allEffects(predf)
confint(before.sgeth.pred)

#Before = social grade:age (not sig)
before.sgage.int <- glm(LGO_before ~ sex  + ethnicity
                        + dogs + socialgrade*age,
                        family = binomial, weights = weight, data = dta_label)
summary(before.sgage.int)
allEffects(before.sgage.int)
before.sgage.pred <- effect("socialgrade:age", before.sgage.int)
before.sgage.pred
model.tables()

plot(allEffects(before.sgage.int))
plot(allEffects(before.sgage.int), multiline=TRUE, ci.style="bars")

install.packages(jtools)
library(jtools)
cat_plot(before.sgage.pred, pred=socialgrade, modx = age)

#Before = Age:dogs  (not sig)
before.agedog.int <- glm(LGO_before ~  socialgrade + ethnicity 
                         + sex + age*dogs,
                         family = binomial, weights = weight, data = dta_label)
summary(before.agedog.int)
anova(lm.before.adj, before.agedog.int, test="Chi")

allEffects(before.agedog.int)
before.agedog.pred <- effect("age:dogs", before.agedog.int)
before.agedog.pred

#After = Age:ethnicity
after.ageeth.int <- glm(LGO_after ~ sex + socialgrade + dogs
                        + age*ethnicity,
                        family = binomial, weights = weight, data = dta_label)
summary(after.ageeth.int)
anova(lm.after.adj, after.ageeth.int, test="Chi")

allEffects(after.ageeth.int)
after.ageeth.pred <- effect("age:ethnicity", after.ageeth.int)
after.ageeth.pred

#plot effects
#change order
contrasts(dta_label$age)
table(dta_label$age)
dta_label$age <- relevel(dta_label$age, ref = "18-24")
contrasts(dta_label$age)
dta_label$age <- relevel(dta_label$age, ref = "25-64")
contrasts(dta_label$age)
#then change back
#plot
plot(allEffects(after.ageeth.int))

#Increase: Age: ethnicity 
increase.ageeth.int <- glm(visit_increase ~ sex + socialgrade
                           + dogs + age*ethnicity,
                           family = binomial, weights = weight, data = dta_label)
summary(increase.ageeth.int)
anova(lm.increase.adj, increase.ageeth.int, test="Chi")
lrtest(lm.increase.adj, increase.ageeth.int)

library(effects)
allEffects(increase.ageeth.int)
increase.ageeth.pred <- effect("age:ethnicity", increase.ageeth.int)
increase.ageeth.pred
plot(effect("age:ethnicity", increase.ageeth.int))
plot(allEffects(increase.ageeth.int))
