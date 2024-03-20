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
            )
        ),
        uiOutput(NS(id, "summary"))
    )
}

server_input <- function(id) {
    moduleServer(id, function(input, output, session) {
        data <- reactiveVal(iris)

        observeEvent(input$file, {
            data(vroom(input$file$datapath))

            updateSelectizeInput(
                inputId = "colnames",
                choices = colnames(data())
            )
        })

        output$summary <- renderUI({
            map(input$colnames, \(colname) {
                navset_card_underline(
                    title = colname,
                    nav_panel(
                        title = "Statistics",
                        mean(data()[[colname]])
                    ),
                    nav_panel(
                        title = "Graph",
                        renderPlot(plot(data()[[colname]]))
                    ),
                    nav_panel(
                        title = "Density",
                        renderPlot(
                            density(data()[[colname]]) |> plot()
                        )
                    )
                )
            })
        }) |> bindEvent(input$colnames)

        reactive({
            data()
        })
    })
}
