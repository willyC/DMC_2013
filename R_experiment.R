rm(list=ls())

library(plyr)
library(stringr)
library(reshape2)
library(rstudio)


# Vector of group names
group.names <- c(seq(16), "17_E", "17_U", "17_K", "17_S", seq(18, 24))

# Fraud Transaction Characteristics ---------------------------------------
fraud.df <- subset(df, fraud_ind == 1)

fraud.agg <- ddply(train.df, .(fraud_ind, cnp_ind, sb_ind), summarise,
                   mean_AVL_CREDIT = mean(avl_credit),
                   mean_AUTHZN_AMT = mean(authzn_amt),
                   counts = length(validation))


# Indicator Group Analysis ------------------------------------------------

# What is the most fraud-vulnerable group
indgroup.vec <- df.names[str_detect(df.names, "ind_group")]
indgroup.df <- subset(train.df, select = c("fraud_ind", indgroup.vec))
indgroup.melt <- melt(indgroup.df, id.vars = "fraud_ind")
indgroup.agg <- aggregate(value ~ fraud_ind+variable, indgroup.melt, sum)
indgroup.cast <- dcast(indgroup.agg, variable ~ fraud_ind, value.var="value")
indgroup.cast$perc <- indgroup.cast$`1` / (indgroup.cast$`0` + indgroup.cast$`1`)
indgroup.cast <- indgroup.cast[order(-indgroup.cast$perc), ]

# Average Amount per group
amtgroup.df <- subset(train.df, select = c("fraud_ind", indgroup.vec, "authzn_amt"))
amtgroup.melt <- subset(melt(amtgroup.df, id.vars = c("fraud_ind", "authzn_amt")),
                        value == 1)
amtgroup.agg <- ddply(amtgroup.melt, .(fraud_ind, variable), summarise,
                      avg = mean(authzn_amt))
amtgroup.cast <- dcast(amtgroup.agg, variable ~ fraud_ind, value.var = "avg")
amtgroup.cast$ratio <- amtgroup.cast$`1` / amtgroup.cast$`0`
amtgroup.cast <- amtgroup.cast[order(-amtgroup.cast$ratio), ]


# Data Shaping ------------------------------------------------------------

# Create IND_GROUP variable
ind.vec <- df.names[str_detect(df.names, "IND_GROUP") == 1]
ind.df <- subset(fraud.df, select = ind.vec)
ind.df$counts <- apply(ind.df, 1, sum)


# Time Series Indicator ---------------------------------------------------
vec <- df.names[str_detect(df.names, "GROUP17_E_")]
fn.vec <- c("SUM", "COUNT", "MAX", "MEAN", "STD", "MAX")
vec2 <- vec[str_detect(vec, fn.vec[5])]

ts.df <- subset(train.df, select = c("encryption_key", vec2))

