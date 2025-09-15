library(nlme)
library(MuMIn)  # for R-squared calculations

#filter data for +/- 3 years 
dat.lme <- flossdvi[abs(flossdvi$Years_From_Disturbance) <= 3 & 
                       !is.na(flossdvi$MidGreendown_DOY) & 
                       !is.na(flossdvi$Years_From_Disturbance), ]

# Check data structure to make sure there aren't glaring errors 
str(flossdvi[, c("Year", "Label", "MidGreendown_DOY", "Disturbance_Year", "Years_From_Disturbance")])

# Check sample sizes to ensure there are an equal number of labels (sample size) and no .Na values 
length(unique(flossdvi$Label))
table(flossdvi$Label)
sum(is.na(flossdvi$MidGreendown_DOY))

# Look at the range of Years_From_Disturbance
range(flossdvi$Years_From_Disturbance, na.rm = TRUE)

#Model accounting for weather and site as a random effect
lmemod <- lme(MidGreendown_DOY ~ Years_From_Disturbance, random = list(Year = ~1, Label = ~1), data = dat.lme)

#testing for signifigance
summary(lmemod)
r.squaredGLMM(lmemod)
anova(lmemod)
#aaaannnnd there does not seem to be

#model for just weather 'oops all weather'
lmemodwe <- lme(MidGreendown_DOY ~ Years_From_Disturbance,random = ~1|Year, data = dat.lme)

# signif testing
summary(lmemodwe)
r.squaredGLMM(lmemodwe)
anova(lmemodwe)

# again no signifigance 

#model for just site 'oops all sites'
lmemodsit <- lme(MidGreendown_DOY ~ Years_From_Disturbance, random = ~1|Label, data = dat.lme)

summary(lmemodsit)
r.squaredGLMM(lmemodsit)
anova(lmemodsit)

#three strikes
