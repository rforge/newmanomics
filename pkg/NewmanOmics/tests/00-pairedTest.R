library(NewmanOmics)

csvfile <- system.file("extdata", "LungNormalTumorPair.csv",
                       package="NewmanOmics")

lung <- read.csv(csvfile)
summary(lung)

set.seed(12345)
picked <- sample(nrow(lung), 1000)

normal <- log(1 + as.matrix(lung[picked, 2, drop=FALSE]))
tumor  <- log(1 + as.matrix(lung[picked, 3, drop=FALSE]))

ps <- pairedStat(normal, tumor)
attributes(ps) # should be two
dim(ps$nu.statstics)
dim(ps$p.values)

summary(ps$nu.statistics)
summary(ps$p.values)

head(ps$nu.statistics)
head(ps$p.values)
