load("uwdmc2013_raw")
names(df) <- tolower(names(df))
df.names <- names(df)
train.df <- subset(df, validation == 0)
test.df <- subset(df, validation == 1)

# Split up indicator variables into mutually exclusive groups
# Group 1: location
# Group 2: type
# Group 3: POS method

indgroup.vec2 <- indgroup.vec[!(indgroup.vec %in% c(
  "ind_group1",
  "ind_group2",
  "ind_group12",
  "ind_group14",
  "ind_group15",
  "ind_group16",
  "ind_group20",
  "ind_group22",
  "ind_group23"
  ))]

l <- subset(train.df, ind_group4 == 1, select = indgroup.vec2)
l.melt <- melt(l, id.vars="ind_group4")
l.agg <- aggregate(value ~ variable, l.melt, sum)
l.agg <- l.agg[order(-l.agg$value), ]
l.agg$value2 <- l.agg$value / nrow(l)
l.agg