################################################################################
##
## R_setup.R
## Workflow: #1
## 
## Purpose: Load files into workspace; create new variables
## 
## Additional Notes: This is the initial R script file. It does not need to be
## run since it is sourced in R_modelling.R
##
################################################################################

rm(list=ls())
source("R_sourcefunctions.R")   # useful functions
set.seed(1234)
load("uwdmc2013_raw")
library(stringr)
library(plyr)
library(reshape2)
names(df) <- tolower(names(df))
df.names <- names(df)

# Metagroup Indicator Variables -------------------------------------------
# Group 1: location
# Group 2: POS method
# Group 3: Type
# Group 4: Time
# Group 5: Amount - type
# Group 6: Behavioural

# Metagroup 1: location
df$mgroup1 <- factor(df$ind_group13)

# Metagroup 2: POS method
df$mgroup2 <- "other"
df[df$ind_group17_e == 1, ]$mgroup2 <- "e"
df[df$ind_group17_k == 1, ]$mgroup2 <- "k"
df[df$ind_group17_u == 1, ]$mgroup2 <- "u"
# df[df$ind_group17_s == 1, ]$mgroup2 <- "s"
df$mgroup2 <- factor(df$mgroup2)

# Metagroup 3: Type
df$mgroup3 <- "other"
df[df$ind_group4 == 1, ]$mgroup3 <- "auto"
df[df$ind_group5 == 1, ]$mgroup3 <- "discount"
df[df$ind_group7 == 1, ]$mgroup3 <- "elctrncs"
df[df$ind_group9 == 1, ]$mgroup3 <- "fastfood"
df[df$ind_group10 == 1, ]$mgroup3 <- "gas"
df[df$ind_group18 == 1, ]$mgroup3 <- "risky"
df[df$ind_group19 == 1, ]$mgroup3 <- "te"
df[df$ind_group24 == 1, ]$mgroup3 <- "jewellery"
df$mgroup3 <- factor(df$mgroup3)

# Metagroup 4: Time
df$mgroup4 <- factor(df$ind_group21)

# Metagroup 5: Amount - type
df$mgroup5 <- factor(df$ind_group8)

# Metagroup 6: Behavioural
# This is a unique group because a new variable has been created:
# discret : Discretionary (samething as ind_group6)
# Nondiscretcomp : Non-discretionary and compulsory
df$mgroup6 <- "other"
df[df$ind_group6 == 1, ]$mgroup6 <- "discret"
df[(df$ind_group6 == 0 & df$ind_group4 == 1), ]$mgroup6 <- "nondiscretcomp"
df$mgroup6 <- factor(df$mgroup6)


# Count: mgroup ----------------------------------------------
# This variable is picked because its put emphasis on categories of purchases
# that have never been made before and de-emphasizes categories of purchases
# that are made consistently
# Metagroup 1: location (90d)
df$count_mgroup1 <- df$count_group13_p180d

# Metagroup 2: POS method (90d)
df$count_mgroup2 <- 0
df[df$ind_group17_e == 1, ]$count_mgroup2 <- df[df$ind_group17_e == 1, ]$count_group17_e_p180d
# df[df$ind_group17_s == 1, ]$count_mgroup2 <- df$count_group17_s_p90d
df[df$ind_group17_u == 1, ]$count_mgroup2 <- df[df$ind_group17_u == 1, ]$count_group17_u_p180d
df[df$ind_group17_k == 1, ]$count_mgroup2 <- df[df$ind_group17_k == 1, ]$count_group17_k_p180d

# Metagroup 3: Type
df$count_mgroup3 <- 0
df[df$ind_group4 == 1, ]$count_mgroup3 <- df[df$ind_group4 == 1, ]$count_group4_p90d
df[df$ind_group5 == 1, ]$count_mgroup3 <- df[df$ind_group5 == 1, ]$count_group5_p90d
df[df$ind_group7 == 1, ]$count_mgroup3 <- df[df$ind_group7 == 1, ]$count_group7_p30d
df[df$ind_group9 == 1, ]$count_mgroup3 <- df[df$ind_group9 == 1, ]$count_group9_p90d
df[df$ind_group10 == 1, ]$count_mgroup3 <- 3*df[df$ind_group10 == 1, ]$count_group10_p30d
df[df$ind_group18 == 1, ]$count_mgroup3 <- df[df$ind_group18 == 1, ]$count_group18_p180d/2
df[df$ind_group19 == 1, ]$count_mgroup3 <- df[df$ind_group19 == 1, ]$count_group19_p180d/2
# df[df$ind_group24 == 1, ]$count_mgroup3 <- df$count_group24_p90d

df.names[str_detect(df.names, "count_group4")]
l <- subset(df, ind_group4 == 1, select = c("fraud_ind", "count_group4_p180d"))
aggregate(count_group4_p180d ~ fraud_ind, l, max)


# Max: mgroup -------------------------------------------------------------
# This is an indicator variable that tells us whether the current purchase
# is greater than previous purchases
max.vec <- df.names[str_detect(df.names, "max")]
max.df <- subset(df, select = max.vec)
df$max_purchase <- apply(max.df, 1, max)
df$is_gt_maxpurchase <- factor(ifelse(df$authzn_amt > df$max_purchase, 1, 0))



# Change Integer Factors to Factor Variables ------------------------------
df$sb_indf <- factor(df$sb_ind)
df$cnp_indf <- factor(df$cnp_ind)