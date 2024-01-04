
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
        data <- cbind(data[, 1:i], data[, i], data[, (i + 1):ncol(data)])
    }
}

# Expand rows
if (any(rowSums(data, na.rm = TRUE) == 0)) {
    for (i in rev(which(rowSums(data, na.rm = TRUE) == 0))) {
        data <- rbind(data[1:i, ], data[i, ], data[(i + 1):nrow(data), ])
    }
}

get_coords <- function(data, nrows, x) {
    col_idx <- ceiling(match(x, data) / nrows)
    row_idx <- which(data[, ceiling(match(x, data) / nrows)] == x)
    return(list(row_idx, col_idx))
}

get_dist <- function(data, x) {
    nrows <- dim(data)[1]
    sum(abs(unlist(get_coords(data, nrows, x[1])) - unlist(get_coords(data, nrows, x[2]))))
}

c(
    part1 = sum(combn(1:(count - 1), 2, get_dist, data = data))
)
