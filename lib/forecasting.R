library(prophet)
library(magrittr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
source("lib/symbolize.R")

# Retorna um data.frame com os atributos Time, Prediction, Minimum e Maximum.
forecast <- function(df, periods, seasonality, mode) {
    model <- prophet(
        df,
        daily.seasonality = "Daily" %in% seasonality,
        weekly.seasonality = "Weekly" %in% seasonality,
        yearly.seasonality = "Yearly" %in% seasonality,
        seasonality.mode = str_to_lower(mode)
    )

    diff <- abs(as.numeric(df[["ds"]][2] - df[["ds"]][1]))

    freq <- "day"
    if (diff == 7) {
        freq <- "week"
    } else if (abs(diff - 30) < 4) {
        freq <- "month"
    } else if (abs(diff - 365) < 2) {
        freq <- "year"
    }

    future <- make_future_dataframe(
        model,
        periods = periods,
        include_history = FALSE,
        freq = freq
    )

    predict(model, future) |>
        select(
            Time = ds,
            Prediction = yhat,
            Minimum = yhat_lower,
            Maximum = yhat_upper
        ) |>
        mutate(Time = as_date(Time))
}

forecast_vector <- function(x, periods, seasonality, mode) {
    data.frame(
        ds = seq.Date(
            as_date("1000-01-01"),
            by = "day",
            length.out = length(x)
        ),
        y = x
    ) %>%
        forecast(periods, seasonality, mode) %>%
        mutate(
            Time = seq(
                length(x) + 1,
                length.out = periods
            )
        )
}

symbolize_forecast <- function(fc, sym) {
    fc_sym <- discretize(fc$Prediction, attr(sym, "bp"), letters)

    fc_symbolic <- as_symbolic(fc_sym, NULL, fc$Prediction)

    return(c(sym, fc_symbolic))
}

ggsymbolic_forecast <- function(sym, fc, time) {
    sym <- symbolize_forecast(fc, sym)

    x <- NULL
    ribbon_x <- NULL

    if (is.null(time)) {
        x <- seq_len(length(sym))
        ribbon_x <- seq(length(sym) - nrow(fc) + 1, length.out = nrow(fc))
    } else {
        x <- c(time, as.POSIXct(fc$Time))
        ribbon_x <- as.POSIXct(fc$Time)
    }

    y <- attr(sym, "values")

    bp <- attr(sym, "bp")
    bp <- bp[bp != -Inf & bp != Inf]

    ggplot() +
        geom_line(aes(x, y)) +
        geom_ribbon(
            aes(
                x = ribbon_x,
                y = fc$Prediction,
                ymin = fc$Minimum,
                ymax = fc$Maximum
            ),
            color = "gray",
            alpha = 0.2
        ) +
        geom_point(aes(x, y, color = sym)) +
        geom_hline(yintercept = bp, linetype = "dashed") +
        labs(x = "", y = "", color = "Symbol") +
        scale_y_continuous(breaks = round(bp, 2)) +
        theme(
            panel.grid.minor = element_blank(),
            axis.ticks.y = element_blank()
        )
}
