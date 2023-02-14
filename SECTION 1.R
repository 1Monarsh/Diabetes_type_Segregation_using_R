# Our task is to train a model to predict the glass type, 6 classes as output,
# based on the oxide content b to j columns, input, as stated by the 
# United States Forensic Science Service.

# This chunk installs necessary packages if not installed
list.of.packages <- c("mlbench", "tidyverse", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Tidyverse for an easier data processing
library(tidyverse)
# caret for an easier modeling and assessment
library(caret)

# We'll utilize the PimaIndiansDiabetes2 [in mlbench package] 
# to estimate the likelihood of having diabetes positive based 
# on various clinical characteristics in this section.
library(mlbench)
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)


# Split the data into training (80%) and test set (20%)
# We set seed for reproducibility reasons
set.seed(2)

# split the data into 80% training and 20% testing with shuffling
train.data <- PimaIndiansDiabetes2[sample(1:nrow(PimaIndiansDiabetes2),
                                          floor(0.8*nrow(PimaIndiansDiabetes2))), ]

# remove the train rows and keep the rest for testing
test.data <- PimaIndiansDiabetes2[-train.data[,1],]

# Fit the model
model <- glm( diabetes ~., data = train.data, family = binomial)

# (b)
# Summarize the model
summary(model)

# Significant features based on p-value are:
# - Intercept: -11.10
# - glucose: 0.04
# - mass: 0.06
# - pedigree: 0.94
# - age: 0.06


# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
print(predicted.classes)

# Functional form of the optimal predictive model
# log(diabetes) = + 0.01 pregnant + 0.04 glucose 
#                 - 0.002 pressure + 0.02 triceps 
#                 - 0.002 insulin + 0.06 mass 
#                 + 0.94 pedigree + 0.06 age - 11.10



observed.classes <- test.data$diabetes
predicted.classes <- as.factor(predicted.classes)
confusionMatrix(predicted.classes, observed.classes,
                positive = "pos")
# Model accuracy
mean(predicted.classes == observed.classes)

# Confusion matrix
#               Reference
# Prediction    neg pos
# neg           230  48
# pos           26  72
# Accuracy : 0.8032          
# 95% CI : (0.7593, 0.8422)
# No Information Rate : 0.6809          
# P-Value [Acc > NIR] : 7.723e-08 

# Probability of correctness of predictions = 80.32 %  

