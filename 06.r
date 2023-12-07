n_wins <- function(t, record) {
    distance <- 1:t * (t - 1:t)
    sum(distance > record)
}

data <- readLines("input_06.txt") |>
    strsplit(" +") |>
    lapply(\(x) as.numeric(x[2:length(x)])) |>
    setNames(c("time", "record"))

library(stringr)
data2 <- readLines("input_06.txt") |>
    strsplit(": +") |>
    lapply(\(x) as.numeric(str_replace_all(x[2], " ", ""))) |>
    setNames(c("time", "record"))

c(
    part1 =
        prod(
            unlist(
                lapply(seq_along(data$time), \(x) n_wins(data[["time"]][x], data[["record"]][x]))
            )
        ),
    part2 = n_wins(data2[["time"]], data2[["record"]])
)
