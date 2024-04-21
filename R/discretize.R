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
            numericInput(
                inputId = NS(id, "compr"),
                label = "Specify the group size:",
                value = 1,
            ),
            accordion(
                open = FALSE,
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
        options <- server_discretize_symbolic("symbolic")

        observeEvent(data(), {
            updateSelectInput(
                session,
                "attr",
                choices = get_numeric_colnames(data()),
            )
        })

        symbolic <- reactive({
            attr <- data()[[input$attr]]

            if (isTruthy(input$compr)) {
                attr <- paa(attr, input$compr)
            }

            symb <- exec(symbolize, attr, !!!options())

            data.frame(attr, symb) |> set_colnames(c(input$attr, "Symbols"))
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
            ggplot(
                symbolic(),
                aes(
                    x = seq_along(.data[[input$attr]]),
                    y = .data[[input$attr]]
                )
            ) +
                geom_line() +
                geom_point(aes(colour = Symbols), size = 2) +
                labs(x = "Index") +
                theme_minimal() +
                scale_colour_viridis_d()
        }) |> bindEvent(input$apply)
    })
}
