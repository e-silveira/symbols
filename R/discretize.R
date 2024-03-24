library(shiny)
library(dplyr)
library(zeallot)
library(bslib)
library(bsicons)
library(DT)
source("lib/forecasting.R")
source("lib/symbolize.R")

ui_discretize_input <- function(id) {
    list(
        div(
            accordion(
                open = FALSE,
                accordion_panel(
                    title = "Selection",
                    icon = bs_icon("filetype-csv"),
                    ui_input_(NS(id, "input")),
                ),
                accordion_panel(
                    title = "Symbolic",
                    icon = bs_icon("sort-alpha-up-alt"),
                    ui_symbolic(NS(id, "symbolic")),
                ),
                accordion_panel(
                    title = "Forecasting",
                    icon = bs_icon("magic"),
                    ui_forecasting(NS(id, "forecasting")),
                ),
                header = br(),
            )
        ),
        actionButton(
            inputId = NS(id, "apply"),
            label = "Apply",
        )
    )
}

ui_discretize_output <- function(id) {
    list(
        navset_underline(
            nav_panel(
                title = "Table",
                icon = bs_icon("table"),
                DT::dataTableOutput(
                    outputId = NS(id, "table")
                ),
            ),
            nav_panel(
                title = "Plot",
                icon = bs_icon("graph-up"),
                plotOutput(
                    outputId = NS(id, "plot")
                ),
            ),
            header = br(),
        )
    )
}

ui_discretize <- function(id) {
    layout_sidebar(
        sidebar = sidebar(
            width = "33%",
            tab_header("Discretization"),
            !!!ui_discretize_input(id),
        ),
        !!!ui_discretize_output(id),
    )
}

server_discretize <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        opt_input <- server_input_("input", data)
        opt_symbolic <- server_symbolic("symbolic")
        opt_forecasting <- server_forecasting("forecasting")

        df_symbolic <- reactive({
            make_df_from(opt_input()) |>
                apply_paa(opt_symbolic()) |>
                apply_symbolize(opt_symbolic())
        })

        df_forecasting <- reactive({
            alphabet <- letters
            if (!is.null(opt_symbolic()$classes)) {
                alphabet <- opt_symbolic()$classes
            }

            apply_forecast(
                df_symbolic(),
                opt_forecasting(),
                alphabet
            )
        })

        output$table <- DT::renderDataTable(
            {
                df_symbolic()
            },
            options = list(
                paging = FALSE,
                searching = FALSE,
                scrollX = TRUE,
                scrollY = TRUE
            )
        ) |> bindEvent(input$apply)

        output$plot <- renderPlot({
            if (opt_forecasting()$apply) {
                plot_symbolic_forecasting(
                    df_symbolic(),
                    df_forecasting()
                )
            } else {
                plot_symbolic(df_symbolic())
            }
        }) |> bindEvent(input$apply)
    })
}

plot_symbolic <- function(df) {
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

plot_symbolic_forecasting <- function(df_symbolic, df_forecasting) {
    c(time_name, attr_name, symb_name) %<-% colnames(df_symbolic)

    df <- data.frame(
        as_date(c(df_symbolic[[time_name]], df_forecasting$Time)),
        c(df_symbolic[[attr_name]], df_forecasting$Prediction),
        c(df_symbolic[[symb_name]], df_forecasting$Symbols)
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
            data = df_forecasting,
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
    c(time_name, attr_name) %<-% colnames(df)

    paaed_attr <- paa(df[[attr_name]], symb$compr)

    paaed_time <- NULL
    if (is.instant(df[[time_name]])) {
        paaed_time <- paa_date(df[[time_name]], symb$compr)
    } else {
        paaed_time <- seq_along(paaed_attr)
    }

    paaed <- data.frame(paaed_time, paaed_attr)
    colnames(paaed) <- c(time_name, attr_name)

    paaed
}

apply_symbolize <- function(df, symb) {
    c(time_name, attr_name) %<-% colnames(df)

    alphabet <- letters
    if (!is.null(symb$classes)) {
        alphabet <- symb$classes
    }

    symbolized <- symbolize(
        df[[attr_name]],
        symb$alpha,
        alphabet = alphabet,
        method = symb$method
    )

    cbind(df, Symbols = symbolized)
}

apply_forecast <- function(symb, forecasting, alphabet) {
    if (!forecasting$apply) {
        return(symb)
    }

    c(time_name, attr_name, symb_name) %<-% colnames(symb)

    df_forecasting <- NULL
    if (is.instant(symb[[time_name]])) {
        df_forecasting <- forecast(
            data.frame(
                ds = symb[[time_name]],
                y = symb[[attr_name]]
            ),
            forecasting$periods,
            forecasting$seasonality,
            forecasting$mode
        )
    } else {
        df_forecasting <- forecast_vector(
            symb[[attr_name]],
            forecasting$periods,
            forecasting$seasonality,
            forecasting$mode
        )
    }

    symbols <- discretize(
        df_forecasting$Prediction,
        attr(symb[["Symbols"]], "bp"),
        alphabet = alphabet
    )

    cbind(df_forecasting, Symbols = symbols)
}
