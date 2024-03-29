library(shiny)
library(bslib)
library(data.table)
library(purrr)
library(vroom)
library(ggplot2)

ui_input <- function(id) {
    layout_sidebar(
        border = TRUE,
        border_radius = TRUE,
        sidebar = sidebar(
            width = "33%",
            tab_header("Input and Visualization"),
            fileInput(
                inputId = NS(id, "file"),
                label = "Select a data file:",
                accept = ".csv",
                placeholder = "iris.csv"
            ),
            selectizeInput(
                inputId = NS(id, "colnames"),
                label = "Select columns to inspect:",
                choices = colnames(iris),
                multiple = TRUE,
                options = list(plugins = "remove_button")
            ),
            selectizeInput(
                inputId = NS(id, "colnumber"),
                label = "Select the grid width:",
                choices = c(1, 2)
            ),
            actionButton(
                inputId = NS(id, "visualize"),
                label = "Visualize"
            ),
            hr(),
            selectizeInput(
                inputId = NS(id, "colname_reset"),
                label = "Select column to reset:",
                choices = colnames(iris),
            ),
            actionButton(
                inputId = NS(id, "reset"),
                label = "Reset"
            )
        ),
        uiOutput(NS(id, "summary"))
    )
}

server_input <- function(id) {
    moduleServer(id, function(input, output, session) {
        original <- reactiveVal(iris)
        modified <- reactiveVal(iris)

        observeEvent(input$file, {
            original(vroom(input$file$datapath))
            modified(original())

            updateSelectizeInput(
                inputId = "colnames",
                choices = colnames(original())
            )
        })

        observeEvent(input$reset, {
            df <- modified()
            df[[input$colname_reset]] <- original()[[input$colname_reset]]
            modified(df)
        })

        output$summary <- renderUI({
            n <- as.numeric(input$colnumber)
            layout_column_wrap(
                width = 1 / n,
                heights_equal = "row",
                !!!map(input$colnames, \(colname) {
                    navset_card_underline(
                        title = colname,
                        nav_panel(
                            title = "Statistics",
                            info(modified()[[colname]])
                        ),
                        nav_panel(
                            title = "Graph",
                            renderPlot(info_plot(modified()[[colname]]))
                        ),
                        nav_panel(
                            title = "Density",
                            renderPlot(
                                info_density(modified()[[colname]])
                            )
                        )
                    )
                })
            )
        }) |> bindEvent(input$visualize)

        list(
            original = original,
            modified = modified
        )
    })
}

info <- function(data) {
    card()
}

info_plot <- function(column) {
    data <- data.frame(x = seq_along(column), y = column)
    ggplot(data) +
        geom_point(aes(x, y))
}

info_density <- function(column) {
    data <- data.frame(y = column)
    ggplot(data) +
        geom_density(aes(y))
}
