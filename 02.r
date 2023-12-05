library(data.table)

data <- fread("input_02.txt", header = FALSE, sep = ":")
data <- split(data$V2, data$V1)

data <- rbindlist(
    lapply(names(data), \(x) {
        y <- setDT(
            list(tstrsplit(data[[x]], ",|;"))
        )[, c("n", "colour") := tstrsplit(trimws(V1), " ")]
        y[["n"]] <- as.integer(y[["n"]])
        y[["game"]] <- as.integer(gsub("\\D", "", x))
        y
    })
)

data[, remove :=
    fifelse(
        (colour == "red" & n > 12L) |
        (colour == "green" & n > 13L) |
        (colour == "blue" & n > 14L),
        TRUE, FALSE
    )][, V1 := NULL]

c(
    part1 = sum(
        unique(
            data[which(!game %in% unique(data[which(remove)][["game"]]))][["game"]]
        )
    ),
    part2 = dcast(
        data[, .SD[which.max(n)], by = .(game, colour)][, remove := NULL],
        formula = game ~ colour, value.var = "n"
    )[, p := red * green * blue][, .(sum(p))]
)
