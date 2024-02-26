library(dplyr)
library(lubridate)

make_coercible <- function(coerce) {
    function(x) {
        tryCatch(
            {
                coerce(x)
                TRUE
            },
            condition = function(cond) FALSE
        )
    }
}

date_coercible <- make_coercible(as.Date)
numeric_coercible <- make_coercible(as.numeric)

get_numeric_colnames <- function(data) {
    select(data, where(numeric_coercible)) |> colnames()
}

get_date_colnames <- function(data) {
    select(data, where(date_coercible)) |> colnames()
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

dr_date <- function(dates, cr, ignore_remaining = TRUE) {
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
