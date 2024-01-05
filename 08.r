library(stringr)

instructions <- readLines("input_08.txt", n = 1) |>
    strsplit("") |>
    unlist()

data <- readLines("input_08.txt")[-(1:2)]

map <- data |>
    str_remove_all("= \\(|,|\\)") |>
    strsplit(" ") |>
    sapply(\(x) {
        out <- list(list("L" = x[2], "R" = x[3]))
        names(out) <- x[1]
        return(out)
    })

position <- "AAA"
step <- 0L
direction <- 0L

while (position != "ZZZ") {
    step <- step + 1
    direction <- direction + 1
    if (direction > length(instructions)) {
        direction <- 1
    }
    position <- map[[position]][[instructions[direction]]]
}

step
