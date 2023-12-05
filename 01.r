library(stringr)

first_last_sum <- function(data) {
    sum(
        as.integer(
            paste0(
                gsub("^(\\d)\\d*?$", "\\1", gsub("\\D", "", data), perl = TRUE),
                gsub("^\\d*?(\\d)$", "\\1", gsub("\\D", "", data), perl = TRUE)
            )
        ),
        na.rm = TRUE
    )
}

map_nums <- function(str) {
    str_replace_all(
        str,
        c(
            "one" = "one1one",
            "two" = "two2two",
            "three" = "three3three",
            "four" = "four4four",
            "five" = "five5five",
            "six" = "six6six",
            "seven" = "seven7seven",
            "eight" = "eight8eight",
            "nine" = "nine9nine"
        ),
    )
}

data <- readLines("input_01.txt")

c(
    part1 = first_last_sum(data),
    part2 = first_last_sum(map_nums(data))
)
