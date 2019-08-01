library(NewmanOmics)

### pairedStat
data(LungPair)
lung <- as.matrix(log2(1 + LungPair))

set.seed(12345)
normal <- lung[, 1, drop=FALSE]
tumor  <- lung[, 2, drop=FALSE]
ps <- pairedStat(normal, tumor)
summary(ps@p.values)

fm <- fitMix3(as.vector(ps@p.values), print.level = 0)
fm@mle
round(fm@psi, 5)

plot(fm)

