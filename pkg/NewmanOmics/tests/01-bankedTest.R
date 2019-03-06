library(NewmanOmics)

csvfile <- system.file("extdata", "GSE6631_Head&NeckSq.csv",
                       package="NewmanOmics")
HN <- as.matrix(read.csv(csvfile, row.names=1))
HN <- log2((1/19)*median(HN) + HN)

set.seed(12345)
picked <- sample(nrow(HN), 2000)
bankMatrix <- HN[picked, seq(1, ncol(HN), 2)]
testSet <- HN[picked, seq(2, 6, 2)]

bs <- bankStat(testSet = testSet, bankMatrix = bankMatrix)
class(bs) # now a list. Should we create a class?
names(bs) # two entries
summary(bs$nu.statistics)
summary(bs$p.values)

bankObj <- createBank(bankMatrix)
summary(bankObj$rowStats)
dim(bankObj$background)
mean(bankObj$background)
range(bankObj$background)

bs2 <- bankStat(bankObj, testSet)
all(bs$nu.statistics == bs2$nu.statistics)
