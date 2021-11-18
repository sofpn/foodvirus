filepath <- system.file(
    "extdata", "oyster.txt",
    package = "foodvirus"
)

oyster <- readFoodVirusData(filepath)

save(oyster, file = "oyster.RData")

tools::resaveRdaFiles(".")
