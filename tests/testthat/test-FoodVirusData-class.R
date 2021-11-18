# Load example data
data("oyster")

test_that("Constructor works", {
    x <- as.data.frame(oyster)
    x <- FoodVirusData(x)
    expect_s4_class(x, "FoodVirusData")
})

test_that("Validation works", {
    x <- as.data.frame(oyster)
    y <- x
    names(y)[[1]] <- "A"
    expect_error(FoodVirusData(y))
    y <- x
    y <- y[, 1]
    expect_error(FoodVirusData(y))
    y <- x
    expect_s4_class(FoodVirusData(y[1:5, ]), "FoodVirusData")
    y$obtained <- as.character(y$obtained)
    expect_error(FoodVirusData(y))
})
