library(dplyr)
library(lubridate)
library(shiny)

date_formats <- list(
    "ymd",
    "dmy",
    "ymd HMS",
    "dmy HMS"
)

numeric_coercible <- function(x) {
    tryCatch(
        {
            as.numeric(x)
            TRUE
        },
        condition = function(cond) FALSE
    )
}

date_coercible <- function(dates) {
    tryCatch(
        {
            parse_date_time(dates, date_formats)
            TRUE
        },
        condition = function(cond) FALSE
    )
}

get_numeric_colnames <- function(data) {
    select(
        data,
        where(function(column) {
            numeric_coercible(column) && !all(is.na(column))
        })
    ) |> colnames()
}

get_date_colnames <- function(data) {
    select(
        data,
        where(function(column) {
            date_coercible(column) && !all(is.na(column))
        })
    ) |> colnames()
}

coerce_data_frame <- function(df) {
    date_colnames <- get_date_colnames(df)

    imodify(df, function(column, colname) {
        if (colname %in% date_colnames) {
            parse_date_time(column, date_formats)
        } else {
            column
        }
    })
}


try_subset <- function(data, colname) {
    tryCatch(
        data[[colname]],
        condition = function(cond) {
            NULL
        }
    )
}

mean_datetime <- function(dates) {
    mean(as.numeric(as_datetime(dates)), na.rm = TRUE) |> as_datetime()
}

# Polymorph this function.
paa_date <- function(dates, cr, ignore_remaining = TRUE) {
    res_size <- NA
    if (ignore_remaining) {
        res_size <- floor(length(dates) / cr)
    } else {
        res_size <- ceiling(length(dates) / cr)
    }

    res <- rep(NA, res_size)
    for (i in seq_len(res_size)) {
        res[i] <- mean_datetime(dates[seq((i - 1) * cr + 1, i * cr)])
    }

    as_datetime(res)
}

make_fake_time <- function(n, initial = as.Date("1000-01-01"), by = "day") {
    seq.Date(initial, by = by, length.out = n)
}

internal_bp <- function(bp) {
    bp[seq(2, length(bp) - 1)]
}

sep <- function() {
    hr(style = "color: #757575; margin-bottom: 0; margin-top: 0")
}

tab_header <- function(title, ...) {
    tagList(
        h5(title, style = "margin: 0"),
        ...,
        sep()
    )
}

set_colnames <- function(df, colnames_) {
    colnames(df) <- colnames_
    df
}

is_truthy <- function(x) {
    if (isTruthy(x)) x else NULL
}
