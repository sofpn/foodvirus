#' @noRd
#'
#' @examples
#' data("oyster")
#' oyster <- addAnticipatedDetected(oyster)
#' plotLinearity(oyster)
plotLinearity <- function(x, threshold = NULL) {
    if (is.null(threshold)) {
        threshold <- 1
    }
    anticipated <- obtained <- NULL
    x <- as.data.frame(x)
    x <- x[!is.na(x$obtained), ]
    x$anticipated <- log2(x$anticipated)
    x$obtained <- log2(x$obtained)
    minmax <- c(
        min(c(x$obtained, x$anticipated), na.rm = TRUE),
        max(c(x$obtained, x$anticipated), na.rm = TRUE)
    )
    slope <- round(stats::lm(
        obtained ~ anticipated,
        subset(x, x$anticipated >= log2(threshold) - 0.01))$coefficients[[2]], 2
    )
    aboveThreshold <- x$anticipated >= log2(threshold) - 0.01
    ggplot2::ggplot(
        data = x, ggplot2::aes(x = anticipated, y = obtained)
    ) +
        ggplot2::geom_abline(slope = 1, col = "white", size = 1) +
        ## Make color scheme
        ggplot2::geom_point(ggplot2::aes(color = aboveThreshold)) +
        ggplot2::geom_smooth(
            method = "lm", col = "black",
            data = subset(x, anticipated >= log2(threshold) - 0.01)
        ) +
        ggplot2::ylab("Obtained") +
        ggplot2::xlab("Anticipated") +
        ggplot2::scale_x_continuous(
            labels = round(2^(unique(x$anticipated)), 2),
            breaks = unique(x$anticipated)
        ) +
        ggplot2::scale_y_continuous(
            labels = 2^(0:100),
            breaks = 0:100
        ) +
        ggplot2::geom_label(
            x = - Inf, y =  + Inf, hjust = -1, vjust = 2,
            label = paste("Slope =", slope)
        ) +
        ggplot2::theme(legend.position = "none")
}

#' @noRd
#'
#' @examples
#' data("oyster")
#' oyster <- addAnticipatedDetected(oyster)
#' plotPrecision(oyster)
plotPrecision <- function(x, threshold = NULL) {
    anticipated <- obtained <- sd <- sdLog10 <- NULL
    x <- as.data.frame(x)
    x$anticipated <- log2(x$anticipated)
    x$obtained <- log10(x$obtained)
    new <- data.frame(
        "anticipated" = unique(x$anticipated),
        "sdLog10" = unname(
            rev(tapply(x$obtained, x$anticipated, stats::sd, na.rm = TRUE))
        )
    )
    loqValue <- loq(new)
    aboveLoq <- new$anticipated >= loqValue
    if (!is.null(threshold)) {
        threshold <- log2(threshold)
        if (threshold > loqValue) {
            aboveLoq <- new$anticipated >= threshold - 0.01
            loqValue <- min(new$anticipated[aboveLoq])
        }
    }
    ggplot2::ggplot(data = new, ggplot2::aes(x = anticipated, y = sdLog10)) +
        ggplot2::geom_point(size = 2, ggplot2::aes(color = aboveLoq)) +
        ggplot2::geom_hline(yintercept = 0.33, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = loqValue, linetype = "dashed") +
        ggplot2::scale_x_continuous(
            labels = round(2^(unique(x$anticipated)), 2),
            breaks = unique(x$anticipated)
        ) +
        ggplot2::ylab(expression(paste("SD (", log[10], "transformed data)"))) +
        ggplot2::xlab("Anticipated") +
        ggplot2::geom_label(
            label = round(new$sd, 2),
            nudge_y = 0.02
        ) +
        ggplot2::geom_label(
            x = - Inf, y = + Inf, hjust = -1, vjust = 2,
            label = paste("LOQ =", round(2^loqValue, 2))
        ) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::geom_label(
            x = max(x$anticipated) - 1, y = 0.33, label = "LOQ criteria limit"
        )
}
