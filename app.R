library(shiny)

ui <- fluidPage(
    titlePanel("Symbolize"),
    sidebarLayout(
        sidebarPanel(
            user_input_ui("user_input"),
            selection_ui("selection"),
            symbolic_options_ui("symbolic_options"),
            forecasting_options_ui("forecasting_options"),
        ),
        mainPanel(
            plotOutput(outputId = "plot")
        ),
    ),
)

server <- function(input, output, session) {
    data <- user_input_server("user_input")

    columns <- selection_server("selection", data)
    sym_opt <- symbolic_options_server("symbolic_options")
    fc_opt <- forecasting_options_server("forecasting_options")

    output$plot <- renderPlot({
        columns()
        sym_opt()
        fc_opt()

        c(sym, fc, time) %<-% rd_symb_fc(columns, sym_opt, fc_opt)

        if (is.null(fc)) {
            ggsymbolic(sym, time)
        } else {
            ggsymbolic_forecast(sym, fc, time)
        }
    })
}

rd_symb_fc <- function(columns, sym_opt, fc_opt) {
    c(time, attr) %<-% columns()
    c(alphabet_size, crate, method) %<-% sym_opt()

    c(time_rd, attr_rd) %<-% reduce_dimension(time, attr, crate)

    list(
        symbolize(attr_rd, alphabet_size, method = method),
        apply_forecast(time_rd, attr_rd, sym, fc_opt),
        if (is.null(time)) NULL else time_rd
    )
}

reduce_dimension <- function(time, attr, crate) {
    if (is.null(time)) {
        time <- make_fake_time(length(attr))
    }

    list(
        paa_date(time, crate),
        paa(attr, crate)
    )
}

apply_forecast <- function(time, attr, sym, fc_opt) {
    c(should_forecast, periods) %<-% fc_opt()

    if (should_forecast) {
        forecast(data.frame(ds = time, y = attr), periods)
    } else {
        NULL
    }
}

shinyApp(ui, server)
