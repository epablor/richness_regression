#Pablo Rocha
#2023 - 04 - 2023
#Run linear regression on richness data from point counts on the SCR 
#Breeding season 2022


#Not all packages used but may come in handy with more complex models
install.packages("MuMIn")
install.packages("DHARMa")
install.packages("sjPlot")
install.packages("mgcv")
install.packages("piecewiseSEM")
install.packages("multcomp")
install.packages("devtools")
install.packages("jtools")
install.packages("Hmisc")
install.packages("glmmTMB")


library(MuMIn)
library(DHARMa)
library(sjPlot)
library(mgcv)
library(piecewiseSEM)
library(multcomp)
library(devtools)
library(jtools)
library(Hmisc)
library(glmmTMB)

#Data 
points22 <- read.csv("C:/Users/eprocha/Documents/Personal/MS Work/Data Wrangling/SCR_Avian_R/data/richness22points.csv")
exrich <- read.csv("C:/Users/eprocha/Documents/Personal/MS Work/Data Wrangling/SCR_Avian_R/data/exrichness22points.csv")

#scatter plot of richness and flow
#data at the point and date scale
ggplot(data = points22, mapping = aes(x = flow, y = Richness)) +
  geom_point(position = "jitter")

#Histogram plots for normality, just to see how the data fall out
hist(points22$Richness)


#scatterplot matrix 
#to visually identify independent/dependent variable relationships
pairs(~ Richness + canopy + cat1 + cat2 + cat3 + 
        flow + Transect_ID, data = points22, main="Scatterplot Matrix")

#Run linear regression with all covariates
#Reach as a random factor
full22<-glmmTMB(Richness ~ canopy + cat1 + cat2 + cat3 + flow +
              (1|reach) , data = points22)


#basic summary
summary(full22)
#summary table with the random factor information
tab_model(full22)
#r2
performance::r2(full22)
#confidence interval
confint(full22)

#zero inflation test
testZeroInflation(full22)
#outlier test
testOutliers(full22)
#Residuals
testDispersion(full22)
plotResiduals(full22)
#qq plots are great for seeing how well the actual data points fit your model. 
plotQQunif(full22)
res_test = simulateResiduals(full22)
plot(res_test, asFactor = T)

#plots predictor coefficients with confidence intervals
plot_model(full22)
#plots residuals
plot_model(full22, type = "resid")
#plots predicted values from your model with confidence intervals
plot_model(full22, type = "pred")

#AIC approach
#runs all possible combinations of your predictors, ranks the different iterations by predictive power
#make delta bigger to include worse performing models, ex: delta<6
options(na.action = "na.fail")
test.dredge<-dredge(full22, rank="AICc")
test.conf<-subset(test.dredge, delta<2, recalc.weights=TRUE)
View(test.conf)



#Run the same models using cumulative species richness at the point scale


#scatter plot of richness and flow
#data at the point and date scale
ggplot(data = exrich, mapping = aes(x = flow, y = Richness)) +
  geom_point(position = "jitter")

#Histogram plots for normality, just to see how the data fall out
hist(exrich$Richness)


#scatterplot matrix 
#to visually identify independent/dependent variable relationships
pairs(~ Richness + canopy + cat1 + cat2 + cat3 + 
        flow + Transect_ID, data = exrich, main="Scatterplot Matrix")

#Run linear regression with all covariates
#Point as a random factor
exfull<-glmmTMB(Richness ~ canopy + cat1 + cat2 + cat3 + flow +
                (1|point) , data = exrich)

#basic summary
summary(exfull)
# nice summary table with the random factor information
tab_model(exfull)
#r2
performance::r2(exfull)
#confidence interval
confint(exfull)

#zero inflation test
testZeroInflation(exfull)
#outlier test
testOutliers(exfull)
#Residuals
testDispersion(exfull)
plotResiduals(exfull)
#qq plots are great for seeing how well the actual data points fit your model. 
plotQQunif(exfull)
res_test = simulateResiduals(exfull)
plot(res_test, asFactor = T)

#plots predictor coefficients with confidence intervals
plot_model(exfull)
#plots residuals
plot_model(exfull, type = "resid")
#plots predicted values from your model with confidence intervals
plot_model(exfull, type = "pred")

#AIC approach
#runs all possible combinations of your predictors, ranks the different iterations by predictive power
#make delta bigger to include worse performing models, ex: delta<6
options(na.action = "na.fail")
test.dredge<-dredge(exfull, rank="AICc")
test.conf<-subset(test.dredge, delta<2, recalc.weights=TRUE)
View(test.conf)


