#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Boxplot iris dataset"),

    # Select variable
    selectInput(inputId = "numvar", 
                label = "Select variable to use for boxplot", 
                choices = colnames(iris[,c(1:4)])),
    
    # Output
    plotOutput("bPlot")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$bPlot <- renderPlot({
        # draw boxplot
        boxplot(iris[,input$numvar] ~ iris$Species, 
                main = paste0("Boxplot of ",input$numvar), 
                pch = 19, 
                cex.main = 1.6,
                cex.lab = 1.4,
                xlab = "Iris species",
                ylab = input$numvar)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
