GSE6631 <- as.matrix(read.csv("GSE6631_Head&NeckSq.csv", row.names=1))

set.seed(941846)
pick <- sample(nrow(GSE6631), 2000)
GSE6631 <- GSE6631[pick,]
save(GSE6631, file=file.path("..", "..", "data", "GSE6631.rda"))
