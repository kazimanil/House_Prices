# Data Input ----
rm(list = ls()); gc();
train <- fread(input = "train.csv", stringsAsFactors = T)
test  <- fread(input = "test.csv" , stringsAsFactors = T)

# Train Data Manipulation ----
# Logarithmic Transformations
train[, logLotArea := log10(LotArea)]
train[, logLotFrontage := log10(LotFrontage)]
train[, logSP := log10(SalePrice - MiscVal)]; 
train[, ':='(MiscVal = NULL)]

# Dwelling Classes
train[, MSSubClass := as.factor(MSSubClass)]
train[MSSubClass %in% c("30", "70"), Style := as.factor("Old")] 
train[MSSubClass %in% c("20", "60", "120", "160"), Style := as.factor("New")]
train[MSSubClass %in% c("40", "45", "50", "75", "90", "150", "190"), Style := as.factor("All")]
train[, New := as.factor(ifelse(Style == "New", 1, 0))]
train[is.na(New), New := as.factor(0)]
train[, PUD := as.factor(ifelse(MSSubClass %in% c("120", "150", "160", "180"), 1, 0))]
train[MSSubClass %in% c("20", "30", "40", "120"),       Story := as.factor("1")]
train[MSSubClass %in% c("45", "50", "150"),             Story := as.factor("1-1/2")]
train[MSSubClass %in% c("60", "70", "85", "90", "160"), Story := as.factor("2")]
train[MSSubClass %in% c("75", "80", "180", "190"),      Story := as.factor("MULTI")]

# Utilities
train[, Utilities := NULL] # There is no discrepancy among the variable.
# train[Utilities == "AllPub", ':='(Electricity = 1, Gas = 1, Water = 1, Sewer = 1)]
# train[Utilities == "NoSewr", ':='(Electricity = 1, Gas = 1, Water = 1, Sewer = 0)]
# train[Utilities == "NoSeWa", ':='(Electricity = 1, Gas = 1, Water = 0, Sewer = 0)]
# train[Utilities == "ELO", ':='(Electricity = 1, Gas = 0, Water = 0, Sewer = 0)]

# Proximity
train[Condition1 %in% c("Artery", "Feedr") | Condition2 %in% c("Artery", "Feedr"), Proximity := "Main"]
train[Condition1 %in% c("PosN", "PosA") | Condition2 %in% c("PosN", "PosA"), Proximity := "Positive"]
train[Condition1 %in% c("RRNn", "RRAn", "RRNe", "RRAe") | Condition2 %in% c("RRNn", "RRAn", "RRNe", "RRAe"), Proximity := "Railroad"]
train[is.na(Proximity), Proximity := "Norm"]
train[, Proximity := as.factor(Proximity)]
train[, Proximity := relevel(Proximity, "Norm")]
train[, ':='(Condition1 = NULL, Condition2 = NULL)]
train[, PavedDrive := relevel(PavedDrive, "N")]

# Exterior
train[, Exterior1st := as.character(Exterior1st)]
train[, Exterior2nd := as.character(Exterior2nd)]
train[Exterior2nd == "Brk Cmn", Exterior2nd := "BrkComm"]
train[Exterior2nd == "CmentBd", Exterior2nd := "CemntBd"]
train[Exterior2nd == "Wd Shng", Exterior2nd := "WdShing"]
train[, DoubleExterior := ifelse(Exterior1st != Exterior2nd, 1, 0)]
train[, Shingles := ifelse(Exterior1st %in% c("AsbShng", "AsphShn", "WdShing") | Exterior2nd %in% c("AsbShng", "AsphShn", "WdShing"), 1, 0)]
train[, Brick    := ifelse(Exterior1st %in% c("BrkComm", "BrkFace", "CBlock", "Stone") | Exterior2nd %in% c("BrkComm", "BrkFace", "CBlock", "Stone"), 1, 0)]
train[, Board    := ifelse(Exterior1st %in% c("CemntBd", "HdBoard", "Plywood") | Exterior2nd %in% c("CemntBd", "HdBoard", "Plywood"), 1, 0)]
train[, Stucco   := ifelse(Exterior1st %in% c("ImStucc", "Stucco") | Exterior2nd %in% c("ImStucc", "Stucco"), 1, 0)]
train[, Siding   := ifelse(Exterior1st %in% c("MetalSd", "VinylSd", "Wd Sdng") | Exterior2nd %in% c("MetalSd", "VinylSd", "Wd Sdng"), 1, 0)]
train[, Other    := ifelse(Exterior1st %in% c("Other", "PreCast") | Exterior2nd %in% c("Other", "PreCast"), 1, 0)]
train[, Exterior1st := as.factor(Exterior1st)]
train[, Exterior2nd := as.factor(Exterior2nd)]

# Lot Features
train[, LotShape := ifelse(LotShape == "Reg", "Regular", "Irregular")]
train[, CulDeSac := as.factor(ifelse(LotConfig == "CulDSac", 1, 0))]

# Age
train[, ':='(HouseAge = 2011 - YearBuilt,
             RemodeledAge = 2011 - YearRemodAdd,
             GarageAge = 2011 - GarageYrBlt)]
train[, ':='(HouseAge2     = HouseAge^2,
             RemodeledAge2 = RemodeledAge^2,
             GarageAge2    = GarageAge^2)]
train[, ':='(YearBuilt = NULL, YearRemodAdd = NULL, GarageYrBlt = NULL)]

# Masonry Veneer
train[is.na(MasVnrType), MasVnrType := "None"]
train[MasVnrType == "None" & MasVnrArea > 0, MasVnrType := "BrkCmn"]
train[, MasVnrType := relevel(MasVnrType, "None")]

# Area & Rooms
train[, TotalPorchSF := ScreenPorch + `3SsnPorch` + WoodDeckSF + EnclosedPorch + OpenPorchSF]
train[, OneKitchen := (KitchenAbvGr == 1)]

# Test Data Manipulation ----
# Logarithmic Transformations
test[, logLotArea := log10(LotArea)]
test[, logLotFrontage := log10(LotFrontage)]

# Dwelling Classes
test[, MSSubClass := as.factor(MSSubClass)]
test[MSSubClass %in% c("30", "70"), Style := as.factor("Old")] 
test[MSSubClass %in% c("20", "60", "120", "160"), Style := as.factor("New")]
test[MSSubClass %in% c("40", "45", "50", "75", "90", "150", "190"), Style := as.factor("All")]
test[, New := as.factor(ifelse(Style == "New", 1, 0))]
test[is.na(New), New := as.factor(0)]
test[, PUD := as.factor(ifelse(MSSubClass %in% c("120", "150", "160", "180"), 1, 0))]
test[MSSubClass %in% c("20", "30", "40", "120"),       Story := as.factor("1")]
test[MSSubClass %in% c("45", "50", "150"),             Story := as.factor("1-1/2")]
test[MSSubClass %in% c("60", "70", "85", "90", "160"), Story := as.factor("2")]
test[MSSubClass %in% c("75", "80", "180", "190"),      Story := as.factor("MULTI")]

# Utilities
test[, Utilities := NULL] # There is no discrepancy among the variable.
# test[Utilities == "AllPub", ':='(Electricity = 1, Gas = 1, Water = 1, Sewer = 1)]
# test[Utilities == "NoSewr", ':='(Electricity = 1, Gas = 1, Water = 1, Sewer = 0)]
# test[Utilities == "NoSeWa", ':='(Electricity = 1, Gas = 1, Water = 0, Sewer = 0)]
# test[Utilities == "ELO", ':='(Electricity = 1, Gas = 0, Water = 0, Sewer = 0)]

# Proximity
test[Condition1 %in% c("Artery", "Feedr") | Condition2 %in% c("Artery", "Feedr"), Proximity := "Main"]
test[Condition1 %in% c("PosN", "PosA") | Condition2 %in% c("PosN", "PosA"), Proximity := "Positive"]
test[Condition1 %in% c("RRNn", "RRAn", "RRNe", "RRAe") | Condition2 %in% c("RRNn", "RRAn", "RRNe", "RRAe"), Proximity := "Railroad"]
test[is.na(Proximity), Proximity := "Norm"]
test[, Proximity := as.factor(Proximity)]
test[, Proximity := relevel(Proximity, "Norm")]
test[, ':='(Condition1 = NULL, Condition2 = NULL)]
test[, PavedDrive := relevel(PavedDrive, "N")]

# Exterior
test[, Exterior1st := as.character(Exterior1st)]
test[, Exterior2nd := as.character(Exterior2nd)]
test[Exterior2nd == "Brk Cmn", Exterior2nd := "BrkComm"]
test[Exterior2nd == "CmentBd", Exterior2nd := "CemntBd"]
test[Exterior2nd == "Wd Shng", Exterior2nd := "WdShing"]
test[, DoubleExterior := ifelse(Exterior1st != Exterior2nd, 1, 0)]
test[, Shingles := ifelse(Exterior1st %in% c("AsbShng", "AsphShn", "WdShing") | Exterior2nd %in% c("AsbShng", "AsphShn", "WdShing"), 1, 0)]
test[, Brick    := ifelse(Exterior1st %in% c("BrkComm", "BrkFace", "CBlock", "Stone") | Exterior2nd %in% c("BrkComm", "BrkFace", "CBlock", "Stone"), 1, 0)]
test[, Board    := ifelse(Exterior1st %in% c("CemntBd", "HdBoard", "Plywood") | Exterior2nd %in% c("CemntBd", "HdBoard", "Plywood"), 1, 0)]
test[, Stucco   := ifelse(Exterior1st %in% c("ImStucc", "Stucco") | Exterior2nd %in% c("ImStucc", "Stucco"), 1, 0)]
test[, Siding   := ifelse(Exterior1st %in% c("MetalSd", "VinylSd", "Wd Sdng") | Exterior2nd %in% c("MetalSd", "VinylSd", "Wd Sdng"), 1, 0)]
test[, Other    := ifelse(Exterior1st %in% c("Other", "PreCast") | Exterior2nd %in% c("Other", "PreCast"), 1, 0)]
test[, Exterior1st := as.factor(Exterior1st)]
test[, Exterior2nd := as.factor(Exterior2nd)]

# Lot Features
test[, LotShape := ifelse(LotShape == "Reg", "Regular", "Irregular")]
test[, CulDeSac := as.factor(ifelse(LotConfig == "CulDSac", 1, 0))]

# Age
test[, ':='(HouseAge = 2011 - YearBuilt,
            RemodeledAge = 2011 - YearRemodAdd,
            GarageAge = 2011 - GarageYrBlt)]
test[, ':='(HouseAge2     = HouseAge^2,
            RemodeledAge2 = RemodeledAge^2,
            GarageAge2    = GarageAge^2)]
test[, ':='(YearBuilt = NULL, YearRemodAdd = NULL, GarageYrBlt = NULL)]

# Masonry Veneer
test[is.na(MasVnrType), MasVnrType := "None"]
test[MasVnrType == "None" & MasVnrArea > 0, MasVnrType := "BrkCmn"]
test[, MasVnrType := relevel(MasVnrType, "None")]

# Area
test[, TotalPorchSF := ScreenPorch + `3SsnPorch` + WoodDeckSF + EnclosedPorch + OpenPorchSF]
test[, OneKitchen := (KitchenAbvGr == 1)]