library(shiny)
library(bslib)
library(data.table)
library(purrr)
library(dplyr)
library(vroom)
library(ggplot2)
library(nycflights13)

default_dataframe <- fread("./data/santa_maria_dengue.csv")

ui_input <- function(id) {
    layout_sidebar(
        border = TRUE,
        border_radius = TRUE,
        sidebar = sidebar(
            width = "30%",
            tab_header("Input and Visualization"),
            fileInput(
                inputId = NS(id, "file"),
                label = "Select a data file:",
                accept = ".csv",
                placeholder = "default.csv"
            ),
            navset_hidden(
                id = NS(id, "dynamic_selection"),
                nav_panel_hidden(
                    "Table",
                    NULL
                ),
                nav_panel_hidden(
                    "Boxplot",
                    ui_input_boxplot(NS(id, "boxplot"))
                ),
                nav_panel_hidden(
                    "Scatterplot",
                    ui_input_scatterplot(NS(id, "scatterplot"))
                ),
                nav_panel_hidden(
                    "Histogram",
                    ui_input_histogram(NS(id, "histogram"))
                ),
                nav_panel_hidden(
                    "Distribution",
                    ui_input_distribution(NS(id, "distribution"))
                )
            )
        ),
        navset_underline(
            id = NS(id, "current_tab"),
            nav_panel(
                title = "Table",
                dataTableOutput(outputId = NS(id, "table"))
            ),
            nav_panel(
                title = "Boxplot",
                plotOutput(outputId = NS(id, "boxplot"))
            ),
            nav_panel(
                title = "Scatterplot",
                plotOutput(outputId = NS(id, "scatterplot"))
            ),
            nav_panel(
                title = "Histogram",
                plotOutput(outputId = NS(id, "histogram"))
            ),
            nav_panel(
                title = "Distribution",
                plotOutput(outputId = NS(id, "distribution"))
            )
        )
    )
}

server_input <- function(id) {
    moduleServer(id, function(input, output, session) {
        data <- reactiveVal(default_dataframe)

        observeEvent(input$file, {
            data(fread(input$file$datapath) |> coerce_data_frame())
        })

        observeEvent(input$current_tab, {
            nav_select("dynamic_selection", input$current_tab)
        })

        output$table <- renderDataTable(
            data(),
            options = list(
                lengthChange = FALSE,
                paging = TRUE, searching = FALSE,
                scrollX = TRUE, scrollY = TRUE
            )
        ) |> bindEvent(data())

        boxplot <- server_input_boxplot("boxplot", data)
        output$boxplot <- renderPlot(boxplot())

        scatterplot <- server_input_scatterplot("scatterplot", data)
        output$scatterplot <- renderPlot(scatterplot())

        distribution <- server_input_distribution("distribution", data)
        output$distribution <- renderPlot(distribution())

        histogram <- server_input_histogram("histogram", data)
        output$histogram <- renderPlot(histogram())

        data
    })
}
