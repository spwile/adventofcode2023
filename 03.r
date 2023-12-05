data <- readLines("input_03.txt") |>
    strsplit(": |\\| +") |>
    lapply(strsplit, "\\s+")

card_wins <- function(data) {
    lapply(seq_along(data), \(x) {
        wins <- length(intersect(unlist(data[[x]][2]), unlist(data[[x]][3])))
        if (wins >= 1L) {
            points <- 2 ** (wins - 1)
        } else {
            points <- 0
        }
    })
}

c(
    part1 = sum(unlist(card_wins(data)))
)
