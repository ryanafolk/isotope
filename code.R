library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(performance)
library(InformationValue)
library(caret)
library(cAIC4)
library(car)
library(cutpointr)

library(MuMIn)
library(ggeffects)
library(devtools)
library(raredd/plotr)
library(lattice)
library(ggplot2)
install_github("davebraze/FDB1")
library(FDB1)
library(dplyr)


#########
# Load data
#########

# Set working directory to GitHub base directory
Nitfix_Isotopes_all_2N <- read_csv("./Nitfix_Isotopes_all_2N_fixed.csv")

# Scale the variables
Nitfix_Isotopes_all_2N$sc_d13CpermilvsVPDB <- scale(Nitfix_Isotopes_all_2N$d13CpermilvsVPDB)
Nitfix_Isotopes_all_2N$sc_d15NpermilvsAIR <- scale(Nitfix_Isotopes_all_2N$d15NpermilvsAIR)
Nitfix_Isotopes_all_2N$sc_wtN <- scale(Nitfix_Isotopes_all_2N$wtN)
Nitfix_Isotopes_all_2N$sc_wtC <- scale(Nitfix_Isotopes_all_2N$wtC)

#########
# Model fitting for question 1
#########

initial_model <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=Nitfix_Isotopes_all_2N)
summary(initial_model)
vif(initial_model)

# Perform model selection
model_step <- stepcAIC(initial_model, direction = "backward", trace = TRUE, data = Nitfix_Isotopes_all_2N)

# Report r2 of best model
r2_nakagawa(initial_model)


#########
# Plots for Fig. 1
#########

plot_model(initial_model, type = "pred", terms = c("sc_d15NpermilvsAIR")) + ggtitle("N-fixing vs. δ15N") + labs(y = "Proportion N-fixing", x = "δ15N")
plot_model(initial_model, type = "pred", terms = c("sc_d13CpermilvsVPDB")) + ggtitle("N-fixing vs. δ13C") + labs(y = "Proportion N-fixing", x = "δ13C")
plot_model(initial_model, type = "pred", terms = c("sc_wtN")) + ggtitle("N-fixing vs. N content") + labs(y = "Proportion N-fixing", x = "wtN (standardized)")
plot_model(initial_model, type = "pred", terms = c("sc_wtC")) + ggtitle("N-fixing vs. N content") + labs(y = "Proportion N-fixing", x = "wtC (standardized)")

plot_model(initial_model, type = "re", terms = c("Habitat")) + ggtitle("N-fixing vs. habitat") + labs(y = "N-fixing effect", x = "Habitat")



#########
# Train phenotype prediction
#########

# Build training set 1 - randomly pick 75% for training the model and 25% for testing
inTrain <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training1 <- Nitfix_Isotopes_all_2N[ inTrain,]
testing1 <- Nitfix_Isotopes_all_2N[ -inTrain,]

# Build training set 2 - randomly pick 75% for training the model and 25% for testing
inTrain2 <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training2 <- Nitfix_Isotopes_all_2N[ inTrain2,]
testing2 <- Nitfix_Isotopes_all_2N[ -inTrain2,]

# Build training set 3 - randomly pick 75% for training the model and 25% for testing
inTrain3 <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training3 <- Nitfix_Isotopes_all_2N[ inTrain3,]
testing3 <- Nitfix_Isotopes_all_2N[ -inTrain3,]

# Build training set 4 - randomly pick 75% for training the model and 25% for testing
inTrain4 <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training4 <- Nitfix_Isotopes_all_2N[ inTrain4,]
testing4 <- Nitfix_Isotopes_all_2N[ -inTrain4,]

# Build training set 5 - randomly pick 75% for training the model and 25% for testing
inTrain5 <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training5 <- Nitfix_Isotopes_all_2N[ inTrain5,]
testing5 <- Nitfix_Isotopes_all_2N[ -inTrain5,]

# Build training set 6 - randomly pick 75% for training the model and 25% for testing
inTrain6 <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training6 <- Nitfix_Isotopes_all_2N[ inTrain6,]
testing6 <- Nitfix_Isotopes_all_2N[ -inTrain6,]

# Build training set 7 - randomly pick 75% for training the model and 25% for testing
inTrain7 <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training7 <- Nitfix_Isotopes_all_2N[ inTrain7,]
testing7 <- Nitfix_Isotopes_all_2N[ -inTrain7,]

# Build training set 8 - randomly pick 75% for training the model and 25% for testing
inTrain8 <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training8 <- Nitfix_Isotopes_all_2N[ inTrain8,]
testing8 <- Nitfix_Isotopes_all_2N[ -inTrain8,]

# Build training set 9 - randomly pick 75% for training the model and 25% for testing
inTrain9 <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training9 <- Nitfix_Isotopes_all_2N[ inTrain9,]
testing9 <- Nitfix_Isotopes_all_2N[ -inTrain9,]

# Build training set 10 - randomly pick 75% for training the model and 25% for testing
inTrain10 <- createDataPartition(Nitfix_Isotopes_all_2N$Fixing, p = .75,list=FALSE)
training10 <- Nitfix_Isotopes_all_2N[ inTrain10,]
testing10 <- Nitfix_Isotopes_all_2N[ -inTrain10,]

# Train the model 
model1_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training1)
model2_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training2)
model3_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training3)
model4_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training4)
model5_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training5)
model6_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training6)
model7_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training7)
model8_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training8)
model9_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training9)
model10_train <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=training10)

# Predicted_values_from_model
predicted1 <- predict(model1_train, testing1, type="response")
predicted2 <- predict(model2_train, testing2, type="response")
predicted3 <- predict(model3_train, testing3, type="response")
predicted4 <- predict(model4_train, testing4, type="response")
predicted5 <- predict(model5_train, testing5, type="response")
predicted6 <- predict(model6_train, testing6, type="response")
predicted7 <- predict(model7_train, testing7, type="response")
predicted8 <- predict(model8_train, testing8, type="response")
predicted9 <- predict(model9_train, testing9, type="response")
predicted10 <- predict(model10_train, testing10, type="response")

# cutpointr
predtest1 <- as.data.frame(cbind(predicted1,testing1$Fixing))
cutpoints1 <- cutpointr(predtest1,predicted1, V2)

predtest2 <- as.data.frame(cbind(predicted2,testing2$Fixing))
cutpoints2 <- cutpointr(predtest2,predicted2, V2)

predtest3 <- as.data.frame(cbind(predicted3,testing3$Fixing))
cutpoints3 <- cutpointr(predtest3,predicted3, V2)

predtest4 <- as.data.frame(cbind(predicted4,testing4$Fixing))
cutpoints4 <- cutpointr(predtest4,predicted4, V2)

predtest5 <- as.data.frame(cbind(predicted5,testing5$Fixing))
cutpoints5 <- cutpointr(predtest5,predicted5, V2)

predtest6 <- as.data.frame(cbind(predicted6,testing6$Fixing))
cutpoints6 <- cutpointr(predtest6,predicted6, V2)

predtest7 <- as.data.frame(cbind(predicted7,testing7$Fixing))
cutpoints7 <- cutpointr(predtest7,predicted7, V2)

predtest8 <- as.data.frame(cbind(predicted8,testing8$Fixing))
cutpoints8 <- cutpointr(predtest8,predicted8, V2)

predtest9 <- as.data.frame(cbind(predicted9,testing9$Fixing))
cutpoints9 <- cutpointr(predtest9,predicted9, V2)

predtest10 <- as.data.frame(cbind(predicted10,testing10$Fixing))
cutpoints10 <- cutpointr(predtest10,predicted10, V2)

# Let's look at one cutpointr output
summary(cutpoints10)
plot(cutpoints10)

# Standard summaries
min(cutpoints1$acc,cutpoints2$acc,cutpoints3$acc,cutpoints4$acc,cutpoints5$acc,cutpoints6$acc,cutpoints7$acc,cutpoints8$acc,cutpoints9$acc,cutpoints10$acc)
max(cutpoints1$acc,cutpoints2$acc,cutpoints3$acc,cutpoints4$acc,cutpoints5$acc,cutpoints6$acc,cutpoints7$acc,cutpoints8$acc,cutpoints9$acc,cutpoints10$acc)

min(cutpoints1$AUC,cutpoints2$AUC,cutpoints3$AUC,cutpoints4$AUC,cutpoints5$AUC,cutpoints6$AUC,cutpoints7$AUC,cutpoints8$AUC,cutpoints9$AUC,cutpoints10$AUC)
max(cutpoints1$AUC,cutpoints2$AUC,cutpoints3$AUC,cutpoints4$AUC,cutpoints5$AUC,cutpoints6$AUC,cutpoints7$AUC,cutpoints8$AUC,cutpoints9$AUC,cutpoints10$AUC)

min(cutpoints1$sensitivity,cutpoints2$sensitivity,cutpoints3$sensitivity,cutpoints4$sensitivity,cutpoints5$sensitivity,cutpoints6$sensitivity,cutpoints7$sensitivity,cutpoints8$sensitivity,cutpoints9$sensitivity,cutpoints10$sensitivity)
max(cutpoints1$sensitivity,cutpoints2$sensitivity,cutpoints3$sensitivity,cutpoints4$sensitivity,cutpoints5$sensitivity,cutpoints6$sensitivity,cutpoints7$sensitivity,cutpoints8$sensitivity,cutpoints9$sensitivity,cutpoints10$sensitivity)

min(cutpoints1$specificity,cutpoints2$specificity,cutpoints3$specificity,cutpoints4$specificity,cutpoints5$specificity,cutpoints6$specificity,cutpoints7$specificity,cutpoints8$specificity,cutpoints9$specificity,cutpoints10$specificity)
max(cutpoints1$specificity,cutpoints2$specificity,cutpoints3$specificity,cutpoints4$specificity,cutpoints5$specificity,cutpoints6$specificity,cutpoints7$specificity,cutpoints8$specificity,cutpoints9$specificity,cutpoints10$specificity)






#######
# Fixer-non-fixer comparison
#######

# First let's subset to N-fixers
Nitfix_Isotopes_all_2N_fixers <- subset(Nitfix_Isotopes_all_2N, Fixing=="1")

# Correct naming by string replacement
Nitfix_Isotopes_all_2N_fixers$Family[Nitfix_Isotopes_all_2N_fixers$Family == 'Myrtaceae'] <- 'Myricaceae'

# Let's also create a Fabaceae subset and a exotic/native determination present subset
Nitfix_Isotopes_all_2N_fixers_Fabaceae <- subset(Nitfix_Isotopes_all_2N, Family=="Fabaceae")
Nitfix_Isotopes_all_2N_fixers_Fabaceae2 <- subset(Nitfix_Isotopes_all_2N, Native_status!="na")

# Do exotic and native fixers have different wtN and d15N?
model_fixer_wtN <- lmer(wtN ~ Native_status  +  (1|Habitat) + (1| Tribe), data=Nitfix_Isotopes_all_2N_fixers_Fabaceae2)
model_fixer_d15N <- lmer(d15NpermilvsAIR ~ Native_status  +  (1|Habitat) + (1| Tribe),  data=Nitfix_Isotopes_all_2N_fixers_Fabaceae2)
summary(model_fixer_d15N)
summary(model_fixer_wtN)
r2_nakagawa(model_fixer_wtN)
r2_nakagawa(model_fixer_d15N)

# One concern -- are we conflating exotic/native with taxonomic groupings - this code just checks to see what percentage are exotics in each Tribe -- looks like 
# Note that taxonomy was included above so it should be robust
Nitfix_Isotopes_all_2N_fixers_Fabaceae2_perc <- Nitfix_Isotopes_all_2N_fixers_Fabaceae2 %>% group_by(Tribe) %>% dplyr::summarize(exotic_perc=(100*sum(Native_status=="E")/n()),total=n())



#######
# Investigate element and isotope data by family
#######

Nmodel <- lm(d15NpermilvsAIR ~ Family, data=Nitfix_Isotopes_all_2N)
Cmodel <- lm(d13CpermilvsVPDB ~ Family, data=Nitfix_Isotopes_all_2N)
wtNmodel <- lm(wtN ~ Family, data=Nitfix_Isotopes_all_2N)
wtCmodel <- lm(wtC ~ Family, data=Nitfix_Isotopes_all_2N)
compoundmodel <- lm(d15NpermilvsAIR*d13CpermilvsVPDB*wtN*wtC ~ Family, data=Nitfix_Isotopes_all_2N)

summary(model)

library(sjPlot)
Nplot <- plot_model(Nmodel, type = "pred", terms = c("Family")) + ggtitle("δ15N vs. family") + labs(y = "δ15N", x = "Family") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
Cplot <- plot_model(Cmodel, type = "pred", terms = c("Family")) + ggtitle("δ13C vs. family") + labs(y = "δ13C", x = "Family") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
wtNplot <- plot_model(wtNmodel, type = "pred", terms = c("Family")) + ggtitle("N content vs. family") + labs(y = "wtN", x = "Family") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
wtCplot <- plot_model(wtCmodel, type = "pred", terms = c("Family")) + ggtitle("C content vs. family") + labs(y = "wtC", x = "Family") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

compoundmodelplot <- plot_model(compoundmodel, type = "pred", terms = c("Family")) + ggtitle("N") + labs(y = "N", x = "Family") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#######
# Investigate element and isotope data by habitat
#######

Nitfix_Isotopes_all_2N$Habitat2<- as.factor(Nitfix_Isotopes_all_2N$Habitat)
library(performance)

Habitatmodel_d15N <- lmer(d15NpermilvsAIR ~ Genus + (1|Habitat), data=Nitfix_Isotopes_all_2N)
summary(Habitatmodel_d15N)
Habitatmodel_d13C <- lmer(d13CpermilvsVPDB ~ Genus + (1|Habitat), data=Nitfix_Isotopes_all_2N)
summary(Habitatmodel_d13C)
Habitatmodel_wtN <- lmer(wtN ~ Genus + (1|Habitat), data=Nitfix_Isotopes_all_2N)
summary(Habitatmodel_wtN)
Habitatmodel_wtC <- lmer(wtC ~ Genus + (1|Habitat), data=Nitfix_Isotopes_all_2N)
summary(Habitatmodel_wtC)

model_step <- stepcAIC(Habitatmodel_d15N, direction  = "backward", trace = TRUE, data = Nitfix_Isotopes_all_2N)
model_step <- stepcAIC(Habitatmodel_d13C, direction  = "backward", trace = TRUE, data = Nitfix_Isotopes_all_2N)
model_step <- stepcAIC(Habitatmodel_wtN, direction  = "backward", trace = TRUE, data = Nitfix_Isotopes_all_2N)
model_step <- stepcAIC(Habitatmodel_wtC, direction  = "backward", trace = TRUE, data = Nitfix_Isotopes_all_2N)

r2_nakagawa(Habitatmodel_wtN)


Habitatmodelplot <- plot_model(Habitatmodel_wtN, type = "pred", terms = c("Genus")) + ggtitle("N") + ggtitle("N content vs. genus,\nhabitat as a random effect") + labs(y = "wtN", x = "Genus") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
Habitatmodelplot <- plot_model(Habitatmodel_wtN, type = "re", terms = c("Genus")) + ggtitle("N") + ggtitle("N content vs. habitat,\ngenus as a fixed effect") + labs(y = "Effect", x = "Habitat")


#Marginal R2 provides the variance explained only by fixed effects and conditional R2 provides the variance explained by the entire model, i.e., both fixed effects and random effects.


#######
# Investigate impact of soil properties on element and isotope data
#######

Nitfix_Isotopes_all_2N$Soil_pct_N <- (as.numeric(Nitfix_Isotopes_all_2N$Soil_pct_N_1) + as.numeric(Nitfix_Isotopes_all_2N$Soil_pct_N_2))/2
Nitfix_Isotopes_all_2N$Soil_pct_C <- (as.numeric(Nitfix_Isotopes_all_2N$Soil_pct_C_1) + as.numeric(Nitfix_Isotopes_all_2N$Soil_pct_C_2))/2
soilmodel_d15N <- lmer(d15NpermilvsAIR ~ Soil_pct_N + Soil_pct_C + (1 | Genus), data = Nitfix_Isotopes_all_2N)
summary(soilmodel_d15N)
soilmodel_d13C <- lmer(d13CpermilvsVPDB ~ Soil_pct_N + Soil_pct_C + (1 | Genus), data = Nitfix_Isotopes_all_2N)
summary(soilmodel_d13C)
soilmodel_wtN <- lmer(wtN ~ Soil_pct_N + Soil_pct_C + (1 | Genus), data = Nitfix_Isotopes_all_2N)
summary(soilmodel_wtN)
soilmodel_wtC <- lmer(wtC ~ Soil_pct_N + Soil_pct_C + (1 | Genus), data = Nitfix_Isotopes_all_2N)
summary(soilmodel_wtC)

soilmodelplot <- plot_model(soilmodel_d15N, type = "re", terms = c("Genus")) + ggtitle("N") + labs(y = "N", x = "Genus")




#######
# Investigate native status, controlling for fixers
#######

native_wtN <- lmer(formula = wtN ~ Native_status + (1 | Habitat) + (1 | Fixing), data = Nitfix_Isotopes_all_2N[Nitfix_Isotopes_all_2N$Native_status != "n/a", ])
plot_model(native_wtN, type = "pred", terms = c("Native_status")) + ggtitle("Native status vs. wtN") + labs(y = "wtN", x = "Native status")
plot_model(native_wtN, type = "re", terms = c("Habitat"))[[1]] + ggtitle("Habitat vs. wtN effect")
plot_model(native_wtN, type = "re", terms = c("Fixing"))[[2]] + ggtitle("Fixing status vs. wtN effect")
summary(native_wtN)
r2(native_wtN)

native_wtC <- lmer(formula = wtC ~ Native_status + (1 | Habitat) + (1 | Fixing), data = Nitfix_Isotopes_all_2N[Nitfix_Isotopes_all_2N$Native_status != "n/a", ])
summary(native_wtC)
plot_model(native_wtC, type = "pred", terms = c("Native_status")) + ggtitle("Native status vs. wtC") + labs(y = "wtC", x = "Native status")

native_d15N <- lmer(formula = d15NpermilvsAIR ~ Native_status + (1 | Habitat) + (1 | Fixing), data = Nitfix_Isotopes_all_2N[Nitfix_Isotopes_all_2N$Native_status != "n/a", ])
summary(native_d15N)
plot_model(native_d15N, type = "pred", terms = c("Native_status")) + ggtitle("Native status vs. d15N") + labs(y = "d15N", x = "Native status")

native_d13C <- lmer(formula = d13CpermilvsVPDB ~ Native_status + (1 | Habitat) + (1 | Fixing), data = Nitfix_Isotopes_all_2N[Nitfix_Isotopes_all_2N$Native_status != "n/a", ])
summary(native_d13C)
plot_model(native_d13C, type = "pred", terms = c("Native_status")) + ggtitle("Native status vs. d13C") + labs(y = "d13C", x = "Native status")



#######
# COMPARE TO ABOVE
#######


Nitfix_Isotopes_all_disturbed <- read.csv("C:/Users/joshu/Documents/Projects/Nitfix Isotopes/Nitfix_Isotopes_all_disturbed.csv")

wantedcolumns <- Nitfix_Isotopes_all_2 %>% dplyr::select(Soil.N_1, Soil.C_1, Locality, Habitat)

Nitfix_Isotopes_all_4 <- wantedcolumns %>% dplyr::distinct(Soil.N_1, Soil.C_1, Locality, Habitat, keep_all = T)

boxplot <- boxplot(as.numeric(Soil.N_1)~Habitat,data= Nitfix_Isotopes_all_4, main="Soil N by Habitat", xlab="Habitat", ylab="Soil N")

boxplot2 <- boxplot(as.numeric(Soil.C_1)~Habitat,data= Nitfix_Isotopes_all_4, main="Soil C by Habitat", xlab="Habitat", ylab="Soil C")

exotic <- lm(Soil.N_1 ~ Habitat, data=Nitfix_Isotopes_all_2)

summary(exotic)

vif(exotic)

library(car)

exotic2 <- lm(wtN ~ Native_status, data=Nitfix_Isotopes_all_disturbed)

summary(exotic2)

r2(exotic)

library(tidyverse)

nitfix <- Nitfix_Isotopes_all_2 %>% drop_na()
interactionmodel <- lm(d15NpermilvsAIR ~ Habitat*Native_status, data=nitfix)

noninteractionmodel <- lm(d15NpermilvsAIR ~ Habitat+Native_status, data=nitfix)

anovaint <- anova(interactionmodel,noninteractionmodel)

AIC(interactionmodel)

AIC(noninteractionmodel)

summary(noninteractionmodel)

wtNinteractionmodel <- lm(wtN ~ Habitat*Native_status, data=nitfix)

wtNnoninteractionmodel <- lm(wtN ~ Habitat+Native_status, data=nitfix)

anova_wtN <- anova(interactionmodel,noninteractionmodel)

AIC(wtNinteractionmodel)

summary(wtNnoninteractionmodel)

AIC(wtNnoninteractionmodel)


#######
# DEPRECATED, COMPARE TO ABOVE
#######

#install.packages("ISLR")
library(ISLR)

#sets a random seed for splitting training and testing
set.seed(299)

#this creates a random sample of true and false values equal to number of rows
#in our nitfix dataframe with the probabilities at .75 for TRUE and .25 for FALSE 
sample <- sample(c(TRUE, FALSE), nrow(Nitfix_Isotopes_all_2N), replace=TRUE, prob=c(0.75,0.25))

#we use the sample vector to basically split the file into a training dataset
#the rows matching TRUE go to train, the ones matching FALSE to test
train <- Nitfix_Isotopes_all_2N[sample, ]
test <- Nitfix_Isotopes_all_2N[!sample, ]

#we fit the model predicting Fixing yes/no from our isotope and wtN and C values
#I am using the training dataset to _calibrate_ the model
#this is a logistic regression since we are predicting a binary outcome
model <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=train)

# This is the first magical part where we predict probability of fixing for our test data
predicted <- predict(model, test, type="response")

# Thresholding step
optimal <- optimalCutoff(test$Fixing, predicted)[1]

#this just summarizes everything into a confusion matrix
confusionMatrix(as.factor(test$Fixing), as.factor(ifelse(predicted > optimal, 1, 0)))


