library(shiny)
library(dplyr)
library(zeallot)
library(bslib)
library(bsicons)
library(DT)
source("lib/symbolize.R")

ui_discretize_inputs <- function(id) {
    list(
        div(
            accordion(
                open = FALSE,
                accordion_panel(
                    title = "Selection",
                    ui_discretize_data(NS(id, "input")),
                ),
                accordion_panel(
                    title = "Discretization",
                    ui_discretize_symbolic(NS(id, "symbolic")),
                ),
            )
        ),
        actionButton(
            inputId = NS(id, "apply"),
            label = "Apply",
        )
    )
}

ui_discretize_outputs <- function(id) {
    list(
        navset_underline(
            nav_panel(
                title = "Table",
                dataTableOutput(
                    outputId = NS(id, "table")
                ),
            ),
            nav_panel(
                title = "Plot",
                plotOutput(
                    outputId = NS(id, "plot")
                ),
            )
        )
    )
}

ui_discretize <- function(id) {
    layout_sidebar(
        sidebar = sidebar(
            width = "30%",
            tab_header("Discretization"),
            !!!ui_discretize_inputs(id),
        ),
        !!!ui_discretize_outputs(id),
    )
}

server_discretize <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        opt_input <- server_discretize_data("input", data)
        opt_symbolic <- server_discretize_symbolic("symbolic")

        df_symbolic <- reactive({
            make_df_from(opt_input()) |>
                apply_paa(opt_symbolic()) |>
                apply_symbolize(opt_symbolic())
        })

        output$table <- renderDataTable(
            {
                df_symbolic()
            },
            options = list(
                lengthChange = FALSE,
                paging = TRUE, searching = FALSE,
                scrollX = TRUE, scrollY = TRUE
            )
        ) |> bindEvent(input$apply)

        output$plot <- renderPlot({
            plot_symbolic(df_symbolic()) +
                theme_minimal() +
                scale_color_viridis_d()
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
