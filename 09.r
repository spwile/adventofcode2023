data <- readLines("input_09.txt") |>
    strsplit(" +") |>
    lapply(as.numeric)

get_next_value <- function(data, verbose = FALSE) {
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
    if (verbose) print(A)

    return(sum(A[, dim(A)[2]]))
}

sum(unlist(lapply(data, get_next_value)))
