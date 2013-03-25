################################################################################
##
## R_final.R
## Workflow: #3
## 
## Purpose: Final Prediction and create output file
##
## Additional Notes: This should be the R file that is run to reproduce results
## 
################################################################################

source("R_modelling.R")

final.predictions <- predict(m.final, test.df)
write.csv(final.predictions, file="outputfile.csv", row.names=FALSE, sep=",")