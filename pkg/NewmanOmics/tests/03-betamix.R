library(NewmanOmics)

### pairedStat
csvfile <- system.file("extdata", "LungNormalTumorPair.csv",
                       package="NewmanOmics")
lung <- read.csv(csvfile, row.names=1)
summary(lung)
lung <- as.matrix(log2(1 + lung))

set.seed(12345)
normal <- lung[, 1, drop=FALSE]
tumor  <- lung[, 2, drop=FALSE]
ps <- pairedStat(normal, tumor)

fm <- fitMix3(as.vector(ps@p.values), print.level = 2)

plot(fm)

