
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("R Colors and R symbols"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sym",
                        "Symbols:",
                        min = 0,
                        max = 25,
                        value = 1),
            sliderInput("coloR",
                        "Red scale:",
                        min = 0,
                        max = 100,
                        value = 50),
            sliderInput("coloG",
                        "Green scale:",
                        min = 0,
                        max = 100,
                        value = 50),
            sliderInput("coloB",
                        "Blue scale:",
                        min = 0,
                        max = 100,
                        value = 50)),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        sym = input$sym
        r = input$coloR/100
        g = input$coloG/100
        b = input$coloB/100

        # draw the histogram with the specified number of bins
        plot(1,1,col=rgb(r,g,b,1),pch=sym, cex=5, axes=FALSE, frame.plot=FALSE, xaxt='n', yaxt='n', xlab=c(), ylab=c(), ann=FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

##Prefentially, run the shinny app with runApp(host="0.0.0.0",port=PORT)
