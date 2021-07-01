# Example data =================================================================

example_verification_data <- read.table(
  "example_data_sheet.txt", header = TRUE, sep = "\t", dec = ","
)

# Custom functions =============================================================

import_verification_data <- function(file, sep = "\t", dec = ".", ...) {
  read.table(file, header = TRUE, sep = sep, dec = dec, ...)
}

geometric_mean <- function(x) {
  x <- x[x > 0]
  exp(mean(log(x), na.rm = TRUE))
}

add_anticipated <- function(x) {
  neat <- geometric_mean(x[x$Dilution == 1, ]$Obtained)
  Anticipated <- neat * x$Dilution
  cbind(x, Anticipated)
}

add_detected <- function(x, threshold = 0) {
  Detected <- x$Obtained > threshold & !is.na(x$Obtained)
  cbind(x, Detected)
}

add_detected_anticipated <- function(x) {
  x <- add_anticipated(x)
  x <- add_detected(x)
  x[c("Dilution", "Anticipated", "Obtained", "Detected")]
}

e_lod <- function(x) {
  count <- count_table(x)
  all_detected <- count$Positive / count$Total == 1
  if (!all_detected[[1]]) {
    stop("eLOD cannot be established.", call. = FALSE)
  }
  to_row <- rle(all_detected)$lengths[[1]]
  count <- count[seq_len(to_row), ]
  min(count$Anticipated)
}

count_table <- function(x) {
  Total <- tapply(x$Detected, x$Anticipated, length)
  Positive <- tapply(x$Detected, x$Anticipated, sum)
  df <- data.frame(Anticipated = names(Total), Total, Positive)
  df$Anticipated <- as.double(df$Anticipated)
  df <- df[rev(seq_len(nrow(df))), ]
  rownames(df) <- NULL
  df
}

loq <- function(x) {
    valid <- x$SD <= 0.33
    valid[is.na(valid)] <- FALSE
    if (!valid[[1]]) {
        stop("LOQ cannot be established.", call. = FALSE)
    }
    to_row <- rle(valid)$lengths[[1]]
    x <- x[seq_len(to_row), ]
    min(x$Anticipated)
}

cv <- function(x) {
    sd(x, na.rm = TRUE) /  mean(x, na.rm = TRUE) * 100
}

precision_table <- function(x) {
  data.frame(
    "Anticipated" = unique(x$Anticipated),
    "SD" = unname(
      rev(tapply(x$Obtained, x$Anticipated, sd, na.rm = TRUE))
    ),
    "Anticipated_log10" = unique(log10(x$Anticipated)),
    "SD_log10" = unname(
      rev(tapply(log10(x$Obtained), x$Anticipated, sd, na.rm = TRUE))
    )
  )
}

# Plot functions ==============================================================

plot_linearity <- function(x, threshold = e_lod(x)) {
  x <- as.data.frame(x)
  x <- x[!is.na(x$Obtained), ]
  x$Anticipated <- log2(x$Anticipated)
  x$Obtained <- log2(x$Obtained)
  minmax <- c(
    min(c(x$Obtained, x$Anticipated), na.rm = TRUE),
    max(c(x$Obtained, x$Anticipated), na.rm = TRUE)
  )
  slope <- round(lm(
    Obtained ~ Anticipated,
    subset(x, x$Anticipated >= log2(threshold) - 0.01))$coefficients[[2]], 2
  )
  above_threshold <- x$Anticipated >= log2(threshold) - 0.01
  ggplot2::ggplot(
    data = x, ggplot2::aes(x = Anticipated, y = Obtained)
  ) +
    ggplot2::geom_abline(slope = 1, col = "white", size = 1) +
    ggplot2::geom_point(ggplot2::aes(color = above_threshold)) +
    ggplot2::geom_smooth(
      method = "lm", col = "black",
      data = subset(x, Anticipated >= log2(threshold) - 0.01)
    ) +
    ggplot2::scale_x_continuous(
      labels = round(2^(unique(x$Anticipated)), 2),
      breaks = unique(x$Anticipated)
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

plot_precision <- function(x, threshold = NULL) {
  x <- as.data.frame(x)
  x$Anticipated <- log2(x$Anticipated)
  x$Obtained <- log10(x$Obtained)
  new <- data.frame(
    "Anticipated" = unique(x$Anticipated),
    "SD" = unname(rev(tapply(x$Obtained, x$Anticipated, sd, na.rm = TRUE)))
  )
  loq_value <- loq(new)
  above_loq <- new$Anticipated >= loq_value
  if (!is.null(threshold)) {
    threshold <- log2(threshold)
    if (threshold > loq_value) {
      above_loq <- new$Anticipated >= threshold - 0.01
      loq_value <- min(new$Anticipated[above_loq])
    } 
  }
  ggplot2::ggplot(data = new, ggplot2::aes(x = Anticipated, y = SD)) +
    ggplot2::geom_point(size = 2, ggplot2::aes(color = above_loq)) +
    ggplot2::geom_hline(yintercept = 0.33, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = loq_value, linetype = "dashed") +
    ggplot2::scale_x_continuous(
      labels = round(2^(unique(x$Anticipated)), 2),
      breaks = unique(x$Anticipated)
    ) +
    ggplot2::ylab(expression(paste("SD (", log[10], "transformed data)"))) +
    ggplot2::geom_label(
      label = round(new$SD, 2),
      nudge_y = 0.02
    ) +
    ggplot2::geom_label(
      x = - Inf, y = + Inf, hjust = -1, vjust = 2,
      label = paste("LOQ =", round(2^loq_value, 2))
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_label(
      x = max(x$Anticipated) - 1, y = 0.33, label = "LOQ criteria limit"
    )
}
