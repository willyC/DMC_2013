################################################################################
##
## R_modelling.R
## Workflow: #2
## 
## Purpose: Train models
## 
## Additional Notes: This is the second R script file. It does not need to be
## run since it is sourced in R_final.R
##
################################################################################

source("R_setup.R")
library(party)          # Condtional Tree
library(randomForest)   # Random Forest
library(e1071)          # Naives Bayes Classifier


# Create Train, Validation, Test ------------------------------------------
train.df <- subset(df, validation == 0)
test.df <- subset(df, validation == 1)
index <- 1:nrow(train.df)
trainindex <- sample(index, as.integer(length(index)*0.6))

# Convert to fraud_ind to a factor
# Also note we define a new data frame (as a backup)
train2.df <- train.df
train2.df$fraud_indf <- factor(train2.df$fraud_ind)

# Create final train set (60%) and validation set (40%)
train.sub <- train2.df[trainindex, ]
valid.sub <- train2.df[-trainindex, ]


# Train Models ------------------------------------------------------------
# Fit basic GLM model
m.glm <- glm(fraud_ind ~ sb_indf + cnp_indf + authzn_amt + cl_amt,
             family=binomial(logit), data=train.sub)

# Fit ctree model
m.ctree <- ctree(fraud_indf ~ cnp_indf + authzn_amt + cl_amt +
                   mgroup1 + mgroup2 + mgroup3,
                 data=train.sub)

# Fit random forest model
m.rf <- randomForest(fraud_ind ~ sb_indf + cnp_indf + authzn_amt + cl_amt +
                       mgroup1 + mgroup2 + mgroup3 +
                       count_mgroup1 + count_mgroup2 + count+mgroup3,
                     data=train.sub)

# Fit indicator variable GLM model
m.glm2 <- glm(fraud_indf ~ cnp_indf + authzn_amt + cl_amt + 
                mgroup1 + mgroup2 + mgroup3 +
                count_mgroup1 + count_mgroup2 + count_mgroup3 + is_gt_maxpurchase,
              family=binomial(logit), data=train.sub)
summary(m.glm2)

# Fit Naives Bayes Classifer
m.class <- naiveBayes(fraud_indf ~ cnp_indf + authzn_amt + cl_amt +
                        mgroup1 + mgroup2 + mgroup3 +
                        count_mgroup1 + count_mgroup2 + count_mgroup3 + is_gt_maxpurchase,
                      data=train.sub)


# Test Models -------------------------------------------------------------
valid.sub$pred_glm <- round(predict(m.glm, valid.sub, type="response"), digits=0)
valid.sub$pred_ctree <- predict(m.ctree, valid.sub) 
valid.sub$pred_glm2 <- ifelse(predict(m.glm2, valid.sub, type="response") >= 0.5, 1, 0)
valid.sub$pred_rf <- round(predict(m.rf, valid.sub, type="response"), digits=0)
valid.sub$pred_class <- predict(m.class, valid.sub, type="class")


# Calculate Attrition Cost ------------------------------------------------
# The lowest attrition cost is the Naives Bayes Classifer. This was to be
# expected (see powerpoint presentation). This is therefore the chosen model
DfAttritionCost(valid.sub, "pred_glm")
DfAttritionCost(valid.sub, "pred_ctree")
DfAttritionCost(valid.sub, "pred_glm2")
DfAttritionCost(valid.sub, "pred_rf")
DfAttritionCost(valid.sub, "pred_class")


# Train Model on Entire Data Set ------------------------------------------
m.final <- naiveBayes(fraud_indf ~ cnpindf + authzn_amt + cl_amt +
                        mgroup1 + mgroup2 + mgroup3 +
                        count_mgroup1 + count_mgroup2 + count_mgroup3 +
                        is_gt_maxpurchase,
                      data=train.df)