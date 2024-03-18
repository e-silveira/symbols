helper_ <- function(shiny_tag,
                    content,
                    icon = "circle-info",
                    type = "markdown",
                    colour = "#878787",
                    ...) {
    helper(
        shiny_tag,
        content = content,
        icon = icon,
        type = type,
        colour = colour,
        ...
    )
}
