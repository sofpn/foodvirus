#' @noRd
#'
#' @examples
#' path <- system.file("extdata", "oyster.txt", package = "foodvirus")
#' readFoodVirusData(path)
readFoodVirusData <- function(file, sep = "\t", dec = ".", ...) {
  x <- utils::read.table(file, header = TRUE, sep = sep, dec = dec, ...)
  FoodVirusData(x)
}

#' @noRd
#'
#' @examples
#' x <- runif(10)
#' geometricMean(x)
geometricMean <- function(x) {
  x <- x[x > 0]
  exp(mean(log(x), na.rm = TRUE))
}

#' @noRd
#'
#' @examples
#' data("oyster")
#' addAnticipated(oyster)
addAnticipated <- function(x) {
  neat <- geometricMean(x[x$dilution == 1, ]$obtained)
  anticipated <- neat * x$dilution
  cbind(x, anticipated)
}

#' @noRd
#'
#' @examples
#' data("oyster")
#' addDetected(oyster)
addDetected <- function(x, threshold = 0) {
  detected <- x$obtained > threshold & !is.na(x$obtained)
  cbind(x, detected)
}

#' @noRd
#'
#' @examples
#' data("oyster")
#' addAnticipatedDetected(oyster)
addAnticipatedDetected <- function(x) {
  x <- addAnticipated(x)
  x <- addDetected(x)
  x[c("dilution", "anticipated", "obtained", "detected")]
}


#' @noRd
#'
#' @examples
#' data("oyster")
#' oyster <- addAnticipatedDetected(oyster)
#' eLod(oyster)
eLod <- function(x) {
  count <- countTable(x)
  allDetected <- count$positive / count$total == 1
  if (!allDetected[[1]]) {
    stop("eLOD cannot be established.", call. = FALSE)
  }
  toRow <- rle(allDetected)$lengths[[1]]
  count <- count[seq_len(toRow), ]
  min(count$anticipated)
}


#' @noRd
#'
#' @examples
#' data("oyster")
#' oyster <- addAnticipatedDetected(oyster)
#' countTable(oyster)
countTable <- function(x) {
  total <- tapply(x$detected, x$anticipated, length)
  positive <- tapply(x$detected, x$anticipated, sum)
  proportion <- positive/total
  df <- data.frame(anticipated = names(total), total, positive, proportion)
  df$anticipated <- as.double(df$anticipated)
  df <- df[rev(seq_len(nrow(df))), ]
  rownames(df) <- NULL
  df
}

#' @noRd
#'
#' @examples
#' vec <- runif(110)
#' cv(vec)
cv <- function(x) {
  stats::sd(x, na.rm = TRUE) /  mean(x, na.rm = TRUE) * 100
}

#' @noRd
#'
#' @examples
#' data("oyster")
#' oyster <- addAnticipatedDetected(oyster)
#' precisionTable(oyster)
precisionTable <- function(x) {
  log10obtained <- ifelse(x$obtained == 0, NA, log10(x$obtained))
  data.frame(
    "anticipated" = unique(x$anticipated),
    "sd" = unname(
      rev(tapply(x$obtained, x$anticipated, stats::sd, na.rm = TRUE))
    ),
    "cv" = unname(
      rev(tapply(x$obtained, x$anticipated, cv))
    ),
    "anticipatedLog10" = unique(log10(x$anticipated)),
    "sdLog10" = unname(
      rev(tapply(log10obtained, x$anticipated, stats::sd, na.rm = TRUE))
    )
  )
}

#' @noRd
#'
#' @examples
#' data("oyster")
#' oyster <- addAnticipatedDetected(oyster)
#' oysterPrecision <- precisionTable(oyster)
#' loq(oysterPrecision)
loq <- function(x) {
  valid <- x$sdLog10 <= 0.33
  valid[is.na(valid)] <- FALSE
  if (!valid[[1]]) {
    stop("LOQ cannot be established.", call. = FALSE)
  }
  toRow <- rle(valid)$lengths[[1]]
  x <- x[seq_len(toRow), ]
  min(x$anticipated)
}
