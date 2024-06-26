library(ggplot2)

paa <- function(x, by, ignore_remaining = TRUE) {
    if (any(is.na(x))) {
        warning("Data contains NA.")
    }

    res_size <- NA
    if (ignore_remaining) {
        res_size <- floor(length(x) / by)
    } else {
        res_size <- ceiling(length(x) / by)
    }

    res <- rep(NA, res_size)

    acc <- c()
    for (i in seq_along(x)) {
        acc <- c(acc, x[i])
        if (i %% by == 0) {
            res[i / by] <- mean(acc, na.rm = TRUE)
            acc <- c()
        }
    }

    if (!ignore_remaining) {
        rem <- length(x) %% by
        if (rem != 0) {
            res[res_size] <- mean(acc)
        }
    }

    res
}

discretize <- function(x, bp, alphabet = letters) {
    y <- rep(NA, length(x))

    for (i in seq(1, length(bp) - 1)) {
        y[bp[i] < x & x <= bp[i + 1]] <- alphabet[i]
    }

    y
}

interval_selection <- function(bp, a, b) {
    bp_ <- c()

    i <- 1

    while (i <= length(bp)) {
        if (bp[i] >= a) {
            break
        }

        bp_ <- c(bp_, bp[i])

        i <- i + 1
    }

    bp_ <- c(bp_, a)
    bp_ <- c(bp_, b)

    while (i <= length(bp)) {
        if (bp[i] > b) {
            bp_ <- c(bp_, bp[i])
        }

        i <- i + 1
    }

    bp_
}

breakpoints_sax <- function(x, n) {
    qnorm(
        seq(0, 1, 1 / n),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE)
    )
}

breakpoints_qsax <- function(x, n) {
    step <- 1 / n
    cuts <- seq(0 + step, 1 - step, step)

    bp <- quantile(x, cuts, na.rm = TRUE, names = FALSE)

    c(-Inf, bp, Inf)
}

breakpoints_dwsax <- function(x, n) {
    den <- density(x, na.rm = TRUE)
    dx <- den$x[2] - den$x[1]

    pdf <- list(x = den$x, y = cumsum(den$y * dx))

    step <- 1 / n
    cuts <- seq(0 + step, 1 - step, step)

    bp <- approx(pdf$y, pdf$x, cuts)$y

    c(-Inf, bp, Inf)
}

as_symbolic <- function(symbols, bp, values, alphabet) {
    structure(
        symbols,
        bp = bp,
        values = values,
        alphabet = alphabet,
        class = c("symbolic", "character")
    )
}

# Keeps the breakpoints of the first argument.
c.symbolic <- function(x, y) {
    as_symbolic(
        c(as.character(x), as.character(y)),
        attr(x, "bp"),
        c(attr(x, "values"), attr(y, "values")),
        attr(x, "alphabet")
    )
}

symbolize <- function(x, n, alphabet = letters, method = "qsax") {
    bp <- NA

    alphabet <- alphabet[1:n]

    if (method == "sax") {
        bp <- breakpoints_sax(x, n)
    } else if (method == "qsax") {
        bp <- breakpoints_qsax(x, n)
    } else {
        bp <- breakpoints_dwsax(x, n)
    }

    symbols <- discretize(x, bp, alphabet)

    as_symbolic(symbols, bp, x, alphabet)
}

symbolize_i <- function(x, a, b, n, alphabet = letters, method = "qsax") {
    bp <- NA

    alphabet <- alphabet[1:n]

    if (method == "sax") {
        bp <- breakpoints_sax(x, n)
    } else if (method == "qsax") {
        bp <- breakpoints_qsax(x, n)
    } else {
        bp <- breakpoints_dwsax(x, n)
    }

    bp <- interval_selection(bp, a, b)

    if (length(bp) - 1 != n) {
        n <- length(bp) - 1
        alphabet <- letters[1:n]
    } else {
        alphabet <- alphabet[1:n]
    }

    symbols <- discretize(x, bp, alphabet)

    as_symbolic(symbols, bp, x, alphabet)
}

ggsymbolic <- function(sym, time) {
    x <- NULL

    if (is.null(time)) {
        x <- seq(1, length(sym))
    } else {
        x <- time
    }

    y <- attr(sym, "values")

    bp <- attr(sym, "bp")
    bp <- bp[bp != -Inf & bp != Inf]

    ggplot() +
        geom_line(aes(x, y)) +
        geom_point(aes(x, y, color = sym)) +
        geom_hline(yintercept = bp, linetype = "dashed") +
        labs(x = "", y = "", color = "Symbol") +
        scale_y_continuous(breaks = round(bp, 2)) +
        theme(
            panel.grid.minor = element_blank(),
            axis.ticks.y = element_blank()
        )
}
