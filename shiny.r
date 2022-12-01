# library(shiny)
source("setup.r")
source("plots/killer.r")

coloredBox <- function(color) {
    div(style = paste0("background-color:", color, "; width: 20px; height: 20px"))
}

palette <- function(colors) {
    do.call(div, c(style = "display: flex; flex-direction: row", lapply(colors, coloredBox)))
}

palettes <- list(
    default = person_colors,
    option1 = c("goldenrod", "dimgrey", "blue", "maroon", "lightblue", "red")
)

choices <- unname(do.call(tagList, lapply(palettes, palette)))
values <- names(palettes)

ui <- fluidPage(
    verticalLayout(
        titlePanel("Mortality Killer"),
        plotOutput("killerPlot", height = "500px"),
        wellPanel(
            sliderInput(
                "scale",
                label = "Scale all:",
                min = 0.1, value = 1, max = 3
            ),
            radioButtons(
                "palette",
                "Color Palette",
                choiceNames = choices,
                choiceValues = values,
                inline = TRUE
            )
        )
    )
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput(
    #             "scale",
    #             label = "Scale all:",
    #             min = 0.1, value = 1, max = 3
    #         ),
    #         radioButtons(
    #             "palette",
    #             "Color Palette",
    #             choiceNames = choices,
    #             choiceValues = values,
    #             inline = TRUE
    #         )
    #     ),
    #     mainPanel(
    #         plotOutput("killerPlot")
    #     )
    # )
)

server <- function(input, output) {
    output$killerPlot <- renderPlot({
        print("Killer Plot Server Start")
        draw_killer(palettes[input$palette][[1]], input$scale)
        print("Killer Plot Server Done")
    })
}

# print("Killer Plot UI Start")
shinyApp(ui = ui, server = server, options = list(port = 3000))
