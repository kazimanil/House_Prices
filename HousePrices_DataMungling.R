# Data Input
rm(list = ls()); gc(); dev.off()
train <- fread(input = "train.csv")
test  <- fread(input = "test.csv")
