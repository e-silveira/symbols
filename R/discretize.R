library(shiny)
library(dplyr)
source("lib/forecasting.R")
source("lib/symbolize.R")

discretize_input_ui <- function(id) {
    tagList(
        tabsetPanel(
            tabPanel(
                title = "Input",
                user_input_ui(NS(id, "user_input")),
            ),
            tabPanel(
                title = "Symbolic",
                symbolic_options_ui(NS(id, "symbolic_options")),
            ),
            tabPanel(
                title = "Forecasting",
                forecasting_options_ui(NS(id, "forecasting_options")),
            ),
            header = br(),
        ),
        actionButton(
            inputId = NS(id, "apply"),
            label = "Apply",
        )
    )
}

discretize_output_ui <- function(id) {
    tagList(
        tabsetPanel(
            tabPanel(
                title = "Data",
                dataTableOutput(
                    outputId = NS(id, "table")
                ),
            ),
            tabPanel(
                title = "Plot",
                plotOutput(
                    outputId = NS(id, "plot")
                ),
            ),
            header = br(),
        )
    )
}

discretize_ui <- function(id) {
    sidebarLayout(
        sidebarPanel(
            helpText("Apply symbolization/discretization to a column."),
            discretize_input_ui(id),
        ),
        mainPanel(
            discretize_output_ui(id),
        ),
    )
}

discretize_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        selected_columns <- user_input_server("user_input")
        symbolic_options <- symbolic_options_server("symbolic_options")
        forecasting_options <- forecasting_options_server("forecasting_options")

        symbolic_df <- reactive({
            make_df_from(selected_columns()) |>
                apply_paa(symbolic_options()) |>
                apply_symbolize(symbolic_options())
        })

        forecast_df <- reactive({
            apply_forecast(symbolic_df(), forecasting_options())
        })

        output$table <- renderDataTable(
            {
                symbolic_df()
            },
            options = list(pageLength = 10, searching = FALSE)
        ) |> bindEvent(input$apply)

        output$plot <- renderPlot({
            if (forecasting_options()$apply) {
                discretize_plot_forecasting(symbolic_df(), forecast_df())
            } else {
                discretize_plot(symbolic_df())
            }
        }) |> bindEvent(input$apply)
    })
}

discretize_plot <- function(df) {
    c(time_name, attr_name, symb_name) %<-% colnames(df)
    symb <- df[[symb_name]]

    ggplot(df) +
        geom_line(aes(
            x = .data[[time_name]],
            y = .data[[attr_name]],
        )) +
        geom_point(aes(
            x = .data[[time_name]],
            y = .data[[attr_name]],
            color = .data[[symb_name]]
        )) +
        geom_hline(
            yintercept = internal_bp(attr(symb, "bp")),
            linetype = "dashed"
        )
}

discretize_plot_forecasting <- function(symbolized, forecasted) {
    c(time_name, attr_name, symb_name) %<-% colnames(symbolized)

    df <- data.frame(
        as_date(c(symbolized[[time_name]], forecasted$Time)),
        c(symbolized[[attr_name]], forecasted$Prediction),
        c(symbolized[[symb_name]], forecasted$Symbols)
    )

    colnames(df) <- c(time_name, attr_name, symb_name)

    ggplot(df) +
        geom_ribbon(
            aes(
                x = Time,
                y = Prediction,
                ymin = Minimum,
                ymax = Maximum
            ),
            data = forecasted,
            color = "gray",
            alpha = 0.2
        ) +
        geom_line(aes(
            x = .data[[time_name]],
            y = .data[[attr_name]]
        )) +
        geom_point(aes(
            x = .data[[time_name]],
            y = .data[[attr_name]],
            color = .data[[symb_name]]
        ))
}

make_df_from <- function(cols) {
    c(time, time_name, attr, attr_name) %<-% cols

    df <- NULL
    if (is.null(time)) {
        df <- data.frame(seq_along(attr), attr)
        colnames(df) <- c("Index", attr_name)
    } else {
        df <- data.frame(time, attr)
        colnames(df) <- c(time_name, attr_name)
    }

    df
}

apply_paa <- function(df, symb) {
    crate <- symb$compr

    c(time_name, attr_name) %<-% colnames(df)

    paaed_attr <- paa(df[[attr_name]], crate)

    paaed_time <- NULL
    if (is.Date(df[[time_name]])) {
        paaed_time <- paa_date(df[[time_name]], crate)
    } else {
        paaed_time <- seq_along(paaed_attr)
    }

    paaed <- data.frame(paaed_time, paaed_attr)
    colnames(paaed) <- c(time_name, attr_name)

    paaed
}

apply_symbolize <- function(df, symb) {
    c(time_name, attr_name) %<-% colnames(df)

    symbolized <- symbolize(
        df[[attr_name]],
        symb$alpha,
        method = symb$method
    )

    cbind(df, Symbols = symbolized)
}

apply_forecast <- function(symb, fore) {
    c(should_forecast, periods, seasonality, mode) %<-% fore

    if (!should_forecast) {
        return(symb)
    }

    c(time_name, attr_name, symb_name) %<-% colnames(symb)

    fc <- NULL
    if (is.instant(symb[[time_name]])) {
        fc <- forecast(
            data.frame(
                ds = as.Date(symb[[time_name]]),
                y = symb[[attr_name]]
            ),
            periods,
            seasonality,
            mode
        )
    } else {
        fc <- forecast_vector(
            symb[[attr_name]],
            periods,
            seasonality,
            mode
        )
    }

    sf <- discretize(fc$Prediction, attr(symb[["Symbols"]], "bp"))

    cbind(fc, Symbols = sf)
}