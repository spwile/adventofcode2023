
data <- readLines("input_11.txt") |>
    strsplit("")

# Replace # with a unique number
x <- unlist(data)
count <- 1L
for (i in seq_along(x)) {
    if (x[i] == "#") {
        x[i] <- count
        count <- sum(count, 1)
    } else {
        x[i] <- NA_integer_
    }
}

data <- lapply(x, as.numeric) |>
    sapply(unlist) |>
    matrix(nrow = length(data)) |>
    t()

# Expand columns
if (any(colSums(data, na.rm = TRUE) == 0)) {
    for (i in rev(which(colSums(data, na.rm = TRUE) == 0))) {
        data <- cbind(data[, 1:i], rep(0, length(data[, i])), data[, (i + 1):ncol(data)])
    }
}

# Expand rows
if (any(rowSums(data, na.rm = TRUE) == 0)) {
    for (i in rev(which(rowSums(data, na.rm = TRUE) == 0))) {
        data <- rbind(data[1:i, ], rep(0, length(data[i, ])), data[(i + 1):nrow(data), ])
    }
}

get_dist <- function(data, x, part2 = FALSE, multiplier = NULL) {
    nrows <- dim(data)[1]
    p1_col_idx <- ceiling(match(x[1], data) / nrows)
    p2_col_idx <- ceiling(match(x[2], data) / nrows)
    p1_row_idx <- which(data[, ceiling(match(x[1], data) / nrows)] == x[1])
    p2_row_idx <- which(data[, ceiling(match(x[2], data) / nrows)] == x[2])

    x_dist <- abs(p1_col_idx - p2_col_idx)
    y_dist <- abs(p1_row_idx - p2_row_idx)
    if (part2) {
        x_zeros <- length(which(data[p1_row_idx, p1_col_idx:p2_col_idx] == 0))
        y_zeros <- length(which(data[p1_row_idx:p2_row_idx, p1_col_idx] == 0))
        return(
            sum(x_dist - x_zeros, y_dist - y_zeros, multiplier * sum(x_zeros, y_zeros), -x_zeros, -y_zeros)
        )
    } else {
        return(
            sum(x_dist, y_dist)
        )
    }
}

c(
    part1 = sum(combn(1:(count - 1), 2, get_dist, data = data)),
    part2 = sum(combn(1:(count - 1), 2, get_dist, data = data, part2 = TRUE, multiplier = 1E6))
)
