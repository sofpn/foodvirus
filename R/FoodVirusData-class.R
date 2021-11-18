#' An S4 class for representation of foodvirus-data
#'
#' @description
#' The \code{FoodVirusData}-class stores verification and validation data,
#' which are used as input to the foodvirus Shiny application.
#' A \code{FoodVirusData}-object has a data-frame structure and contains
#' two numeric variables: dilution and obtained.
#'
#' The class extends the \code{S4Vectors::DataFrame} class
#' (Pages et al., 2021), without any additional slots.
#'
#' @param ...
#' A data frame or list to be converted into a \code{FoodVirusData}-object.
#'
#' @return
#' A \code{FoodVirusData}-object if validation succeeds, an error
#' message otherwise.
#'
#' @import methods
#'
#' @importClassesFrom S4Vectors DataFrame
#'
#' @export
#'
#' @references
#' Pages, H., Lawrence, M., and Aboyoun, R. (2021). S4Vectors:
#' Foundation of vector-like and list-like containers in
#' Bioconductor. R package version 0.30.0.
#'
#' @examples
#' ## Constructor
#' data("oyster")
#' x <- as.data.frame(oyster)
#' FoodVirusData(x)
#'
#' ## Coercion to a traditional data-frame
#' as.data.frame(x)
#' as(x, "data.frame")
FoodVirusData <- function(...) {
    x <- S4Vectors::DataFrame(..., row.names = NULL, check.names = TRUE)
    names(x) <- tolower(names(x))
    .FoodVirusData(x)
}

.FoodVirusData <- setClass("FoodVirusData", contains = "DataFrame")

S4Vectors::setValidity2("FoodVirusData", \(object) {
    msg <- NULL
    colnames <- c("dilution", "obtained")
    if (!all(colnames %in% names(object))) {
        msg <- c(
            msg, "The dataset must contain the variables 'dilution' and 'obtained'."
        )
    }
    if ("dilution" %in% names(object)) {
        if (!is.numeric(object$dilution)) {
            msg <- c(
                msg, "'dilution' must be in numeric format."
            )
        }
        if (any(object$dilution > 1) || any(object$dilution < 0)) {
            msg <- c(
                msg, "'dilution'-values cannot be higher than 1 or lower than 0."
            )
        }
    }
    if ("obtained" %in% names(object)) {
        if (!is.numeric(object$obtained)) {
            msg <- c(
                msg, "'obtained' must be in numeric format."
            )
        }
    }
    if (is.null(msg)) {
        TRUE
    } else {
        msg
    }
})
