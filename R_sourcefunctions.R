# AttritionCost : calculates the attrition cost given the 4 arguments
AttritionCost <- function(obs, pred, creditlimit, purchase_amt) {
  
  # Safety check for data types
  purchase_amt <- as.numeric(purchase_amt)
  obs <- as.integer(obs)
  pred <- as.integer(pred)
  
  if (obs == pred) cost <- 0
  else {
    if (obs == 1 && pred == 0) cost <- purchase_amt
    else {
      if (creditlimit <= 2000) cost <- 0.02*75 + purchase_amt*0.0075
      else cost <- 0.05*275 + 4*purchase_amt*0.005
    }
  }
  return(cost)
}

# DfAttritionCost : calculates the total attrition cost for an entire data frame
DfAttritionCost <- function(df, pred) {
  cnames <- c("fraud_ind", pred, "cl_amt", "authzn_amt")
  df2 <- subset(df, select = cnames)
  names(df2) <- c("fraud_ind", "pred", "cl_amt", "authzn_amt")
  df2 <- subset(df2, fraud_ind != pred)
  acost <- sum(apply(df2, 1, function(x)
    AttritionCost(x['fraud_ind'], x['pred'], x['cl_amt'], x['authzn_amt'])))
  return(acost)
}