ui_download <- function(id) {
    tagList(
        numericInput(
            inputId = NS(id, "width"),
            label = "Specify the width of the image:",
            value = 7
        ),
        numericInput(
            inputId = NS(id, "height"),
            label = "Specify the height of the image:",
            value = 7
        ),
        selectInput(
            inputId = NS(id, "device"),
            label = "Select the file type:",
            choices = c("pdf", "png")
        ),
        downloadButton(
            outputId = NS(id, "download"),
            label = "Download"
        )
    )
}

server_download <- function(id, filename, p) {
    moduleServer(id, function(input, output, session) {
        output$download <- downloadHandler(
            filename = function() {
                paste0(filename, ".", input$device)
            },
            content = function(file) {
                ggsave(file, p(),
                       device = input$device,
                       width = input$width, height = input$height)
            }
        )
    })
}
