library(shiny)

ui_classify <- function(id) {
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel(
                    title = "Data",
                    ui_classify_data(NS(id, "data")),
                ),
                header = br(),
            ),
            actionButton(
                inputId = NS(id, "apply"),
                label = "Apply"
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Data",
                    dataTableOutput(
                        outputId = NS(id, "table")
                    )
                ),
                tabPanel(
                    title = "Tree",
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
