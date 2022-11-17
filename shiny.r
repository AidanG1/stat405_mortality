library(shiny)

ui <- fluidPage(
    titlePanel("Mortality Killer"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "scale",
                label = "Scale all:",
                min = 0.1, value = 1, max = 3
            )
        ),
        mainPanel(
            plotOutput("killerPlot")
        )
    )
)

server <- function(input, output) {
    output$killerPlot <- renderPlot({
        print("Killer Plot Server Start")
        draw_killer(person_colors, input$scale)
        print("Killer Plot Server Done")
    })
}

print("Killer Plot UI Start")
shinyApp(ui = ui, server = server)
