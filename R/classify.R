library(shiny)

ui_classify <- function(id) {
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel(
                    title = "Data",
                    ui_classify_data(NS(id, "data")),
                ),
                tabPanel(
                    title = "Tree",
                    ui_classify_tree(NS(id, "tree"))
                ),
                header = br(),
            ),
            actionButton(
                inputId = NS(id, "apply"),
                label = "Apply",
                icon = icon("circle-chevron-right")
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Data",
                    icon = icon("table"),
                    dataTableOutput(
                        outputId = NS(id, "table")
                    )
                ),
                tabPanel(
                    title = "Tree",
                    icon = icon("tree"),
                    plotOutput(
                        outputId = NS(id, "tree")
                    )
                ),
                header = br()
            )
        )
    )
}

server_classify <- function(id) {
    moduleServer(id, function(input, output, session) {
        data <- server_classify_data("data")
        tree <- server_classify_tree("tree", data)

        output$table <- renderDataTable(
            {
                data()
            },
            options = list(
                pageLength = 10,
                searching = FALSE,
                scrollX = TRUE
            )
        ) |> bindEvent(input$apply)

        output$tree <- renderPlot(
            tree()
        ) |> bindEvent(input$apply)
    })
}
