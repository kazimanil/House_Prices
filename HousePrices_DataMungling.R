# Data Input ----
rm(list = ls()); gc(); dev.off()
train <- fread(input = "train.csv", stringsAsFactors = T)
test  <- fread(input = "test.csv", stringsAsFactors = T)

# Utilities
train[Utilities == "AllPub", ':='(Electricity = 1, Gas = 1, Water = 1, Sewer = 1)]
train[Utilities == "NoSewr", ':='(Electricity = 1, Gas = 1, Water = 1, Sewer = 0)]
train[Utilities == "NoSeWa", ':='(Electricity = 1, Gas = 1, Water = 0, Sewer = 0)]
train[Utilities == "ELO", ':='(Electricity = 1, Gas = 0, Water = 0, Sewer = 0)]

# Proximity
train[, Artery := ifelse(Condition1 == "Artery" | Condition2 == "Artery", 1, 0)]
train[, Feedr  := ifelse(Condition1 == "Feedr"  | Condition2 == "Feedr", 1, 0)]
train[, RRNn   := ifelse(Condition1 == "RRNn"   | Condition2 == "RRNn", 1, 0)]
train[, RRAn   := ifelse(Condition1 == "RRAn"   | Condition2 == "RRAn", 1, 0)]
train[, PosN   := ifelse(Condition1 == "PosN"   | Condition2 == "PosN", 1, 0)]
train[, PosA   := ifelse(Condition1 == "PosA"   | Condition2 == "PosA", 1, 0)]
train[, RRNe   := ifelse(Condition1 == "RRNe"   | Condition2 == "RRNe", 1, 0)]
train[, RRAe   := ifelse(Condition1 == "RRAe"   | Condition2 == "RRAe", 1, 0)]
train[, Norm   := ifelse(Artery + Feedr + RRNn + RRAn + PosN + PosA + RRNe + RRAe < 1, 1, 0)]

# Exterior
train[Exterior2nd == "Brk Cmn", Exterior2nd := "BrkComm"]
train[Exterior2nd == "CmentBd", Exterior2nd := "CemntBd"]
train[Exterior2nd == "Wd Shng", Exterior2nd := "WdShing"]
