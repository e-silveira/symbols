library(shiny)

symbolize_ui <- function(id) {
    tagList(
        plotOutput(outputId = NS(id, "plot"))
    )
}

symbolize_server <-
    function(id, selected_columns, symbolic_options, forecasting_options) {
        moduleServer(id, function(input, output, session) {
            output$plot <- renderPlot({
                c(time, attribute) %<-% selected_columns()
                c(alphabet_size, crate, method) %<-% symbolic_options()
                c(apply, periods) %<-% forecasting_options()

                c(time_, attribute_) %<-% dim_reduction(time, attribute, crate)

                sym <- symbolize(attribute_, alphabet_size, method = method)

                if (apply) {
                    fc <- forecast_vector(attribute_, periods)
                    sym_ <- symbolize_forecast(fc, sym)
                    ggsymbolic_forecast(sym_, fc)
                } else {
                    ggsymbolic(sym)
                }
            })
        })
    }

dim_reduction <- function(time, attribute, compression_rate) {
    if (is.null(time)) {
        time <- seq.Date(
            as.Date("1000-01-01"),
            by = "day",
            length.out = length(attribute)
        )
    }

    time <- dr_date(time, compression_rate)
    attribute <- paa(attribute, compression_rate)

    list(time, attribute)
}
