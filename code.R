library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(performance)
library(InformationValue)
library(caret)
library(cAIC4)
library(car)

# Load data
# Set working directory to GitHub base directory
Nitfix_Isotopes_all_2N <- read_csv("./Nitfix_Isotopes_all_2N.csv")

# Scale the variables
Nitfix_Isotopes_all_2N$sc_d13CpermilvsVPDB <- scale(Nitfix_Isotopes_all_2N$d13CpermilvsVPDB)
Nitfix_Isotopes_all_2N$sc_d15NpermilvsAIR <- scale(Nitfix_Isotopes_all_2N$d15NpermilvsAIR)
Nitfix_Isotopes_all_2N$sc_wtN <- scale(Nitfix_Isotopes_all_2N$wtN)
Nitfix_Isotopes_all_2N$sc_wtC <- scale(Nitfix_Isotopes_all_2N$wtC)

# Model fitting for question 1
initial_model <- glmer(Fixing ~ sc_d13CpermilvsVPDB + sc_d15NpermilvsAIR + sc_wtN + sc_wtC + (1|Habitat), family=binomial, data=Nitfix_Isotopes_all_2N)
summary(initial_model)
vif(initial_model)

# Perform model selection
model_step <- stepcAIC(initial_model, direction = "backward", trace = TRUE, data = Nitfix_Isotopes_all_2N)

# Report r2 of best model
r2_nakagawa(initial_model)

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

#######
# Fixer-non-fixer comparison
#######

# First let's subset to N-fixers
Nitfix_Isotopes_all_2N_fixers <- subset(Nitfix_Isotopes_all_2N, Fixing=="1")

# Correct naming by string replacement
Nitfix_Isotopes_all_2N_fixers$Family[Nitfix_Isotopes_all_2N_fixers$Family == 'Myrtaceae'] <- 'Myricaceae'

# Let's also create a Fabaceae subset and a exotic/native subset
Nitfix_Isotopes_all_2N_fixers_Fabaceae <- subset(Nitfix_Isotopes_all_2N, Family=="Fabaceae")
Nitfix_Isotopes_all_2N_fixers_Fabaceae2 <- subset(Nitfix_Isotopes_all_2N, Native_status!="na")

#do exotic and native fixers have different wtN and d15N
model_fixer_wtN <- lmer(wtN ~ Native_status  +  (1|Habitat) + (1| Tribe), data=Nitfix_Isotopes_all_2N_fixers_Fabaceae2)
model_fixer_d15N <- lmer(d15NpermilvsAIR ~ Native_status  +  (1|Habitat) + (1| Tribe),  data=Nitfix_Isotopes_all_2N_fixers_Fabaceae2)
summary(model_fixer_d15N)
summary(model_fixer_wtN)
r2_nakagawa(model_fixer_wtN)
r2_nakagawa(model_fixer_d15N)

#one concern -- are we conflating exotic/native with taxonomic groupings - this code just checks to see what percentage are exotics in each Tribe -- looks like 
Nitfix_Isotopes_all_2N_fixers_Fabaceae2_perc <- Nitfix_Isotopes_all_2N_fixers_Fabaceae2 %>% group_by(Tribe) %>% dplyr::summarize(exotic_perc=(100*sum(Native_status=="E")/n()),total=n())