data <- readLines("input_09.txt") |>
    strsplit(" +") |>
    lapply(as.numeric)

get_next_value <- function(data, part2 = FALSE) {
    A <- matrix(NA, length(data), length(data), byrow = TRUE)
    A[1, ] <- data
    i <- 1L
    Row <- A[1, ]

    while (sum(abs(Row), na.rm = TRUE) != 0) {
        Row <- diff(A[i, i:dim(A)[2]])
        A[i + 1, ] <- c(rep(NA, i), Row)
        i <- sum(i, 1)
    }

    # Empty rows
    A <- A[rowSums(is.na(A)) != ncol(A), ]

    if (part2) {
        x <- diag(A)
        y <- vector(mode = "numeric", length = length(x))
        for (i in (length(y) - 1):1) {
            y[i] <- x[i] - y[i + 1]
        }
        return(y[1])
    } else {
        return(sum(A[, dim(A)[2]]))
    }
}

c(
    part1 = sum(unlist(lapply(data, get_next_value))),
    part2 = sum(unlist(lapply(data, get_next_value, part2 = TRUE)))
)
