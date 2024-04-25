library(shiny)
library(dplyr)
library(zeallot)
library(bslib)
library(bsicons)
library(DT)
library(rlang)
source("lib/symbolize.R")

ui_discretize_inputs <- function(id) {
    list(
        div(
            selectInput(
                inputId = NS(id, "attr"),
                label = "Select the column to discretize:",
                choices = NULL,
            ),
            uiOutput(
                outputId = NS(id, "compr_message")
            ),
            numericInput(
                inputId = NS(id, "compr"),
                label = "Specify the group size:",
                value = 1,
                min = 1
            ),
            accordion(
                open = FALSE,
                accordion_panel(
                    title = "Discretization",
                    ui_discretize_symbolic(NS(id, "symbolic")),
                    input_switch(
                        id = NS(id, "interval"),
                        label = "Interval selection."
                    ),
                    uiOutput(
                        outputId = NS(id, "when_interval")
                    ),
                    numericInput(
                        inputId = NS(id, "lower"),
                        label = "Specify the lower endpoint:",
                        value = NULL
                    ),
                    numericInput(
                        inputId = NS(id, "upper"),
                        label = "Specify the upper endpoint:",
                        value = NULL
                    )
                ),
            ),
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
            tab_header(
                "Discretization",
                helpText(paste0(
                    "Generate columns by discretizing ",
                    "specified attributes within the dataset."
                ))
            ),
            !!!ui_discretize_inputs(id),
        ),
        !!!ui_discretize_outputs(id),
    )
}

server_discretize <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        options <- server_discretize_symbolic("symbolic")

        observeEvent(data(), {
            updateSelectInput(
                session,
                "attr",
                selected = if (isTruthy(input$attr)) input$attr else NULL,
                choices = get_numeric_colnames(data()),
            )
        })

        observeEvent(input$interval, {
            if (!input$interval) {
                output$when_interval <- NULL
                return()
            }

            output$when_interval <- renderUI(
                helpText(
                    paste(
                        "When you specify an interval,",
                        "the dwSAX and qSAX can no longer guarantee",
                        "the equal distribution of classes.",
                        "The interval can also change the number of classes.",
                        "In that case, letters will be used as the alphabet."
                    )
                )
            )
        })

        observeEvent(input$compr, {
            if (input$compr != 1) {
                output$compr_message <- renderUI({
                    helpText(paste0(
                        "When you group of observations, ",
                        "no column will be generated. "
                    ))
                })
            } else {
                output$compr_message <- NULL
            }
        })

        symbolic <- reactive({
            attr <- data()[[input$attr]]

            if (isTruthy(input$compr)) {
                attr <- paa(attr, input$compr)
            }

            if (input$interval) {
                if (!isTruthy(input$lower) || !isTruthy(input$upper)) {
                    validate(
                        "You have to specify the endpoints of the interval."
                    )
                }
                symb <- exec(
                    symbolize_i, attr,
                    input$lower, input$upper, !!!options()
                )
            } else {
                symb <- exec(symbolize, attr, !!!options())
            }

            data.frame(attr, symb) |> set_colnames(c(input$attr, "Symbols"))
        }) |> bindEvent(input$apply)

        observeEvent(symbolic(), {
            if (input$compr != 1) {
                return()
            }

            df <- data()
            colname <- paste0(input$attr, "_symbolic")
            column <- symbolic()[["Symbols"]]

            if (colname %in% colnames(df)) {
                df <- select(df, -all_of(colname))
            }

            data(cbind(df, data.frame(column) |> set_colnames(colname)))
        })

        output$table <- renderDataTable(
            {
                symbolic()
            },
            options = list(
                lengthChange = FALSE,
                paging = TRUE, searching = FALSE,
                scrollX = TRUE, scrollY = TRUE
            )
        ) |> bindEvent(input$apply)

        output$plot <- renderPlot({
            symb <- symbolic()[["Symbols"]]

            bp <- attr(symb, "bp")

            ggplot(
                symbolic(),
                aes(
                    x = seq_along(.data[[input$attr]]),
                    y = .data[[input$attr]]
                )
            ) +
                geom_line() +
                geom_point(aes(colour = Symbols), size = 2) +
                geom_hline(yintercept = internal_bp(bp), linetype = "dashed") +
                labs(x = "Index") +
                theme_minimal() +
                scale_y_continuous(breaks = round(internal_bp(bp), 2))
        }) |> bindEvent(input$apply)
    })
}
