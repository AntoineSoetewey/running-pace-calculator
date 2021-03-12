library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Running pace calculator"),
    h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tags$div(
                tags$p("Type in the distance you plan to run and the time in which you would like to run that distance to find your necessary pace."),
                hr()
            ),
            radioButtons(
                inputId = "units",
                label = "Units:",
                choices = c(
                    "Kilometers" = "km",
                    "Miles" = "mile"
                )
            ),
            conditionalPanel(
                condition = "input.units == 'km'",
                numericInput(
                    "distance_km",
                    "Distance (kms):",
                    10,
                    min = 1,
                    max = 1000
                )
            ),
            conditionalPanel(
                condition = "input.units == 'mile'",
                numericInput(
                    "distance_mile",
                    "Distance (miles):",
                    10,
                    min = 1,
                    max = 1000
                )
            ),
            hr(),
            tags$b("Time"),
            numericInput(
                "hours",
                "Hours:",
                1,
                min = 0,
                max = 1000
            ),
            numericInput(
                "minutes",
                "Minutes:",
                0,
                min = 0,
                max = 59
            ),
            numericInput(
                "seconds",
                "Seconds:",
                0,
                min = 0,
                max = 59
            ),
            hr(),
            HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/running-pace-calculator/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/running-pace-calculator">code</a>. Back to <a href="https://www.antoinesoetewey.com/">antoinesoetewey.com</a> or <a href="https://statsandr.com/">statsandr.com</a>.</p>'),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            conditionalPanel(
                condition = "input.units == 'km'",
                uiOutput("results_km")
            ),
            conditionalPanel(
                condition = "input.units == 'mile'",
                uiOutput("results_mile")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$results_km <- renderUI({
        
        # compute pace
        time_total <- sum(c(input$hours * 3600, input$minutes * 60, input$seconds), na.rm = TRUE)
        time <- time_total / input$distance_km
        
        # display results
        paste0("You need to run ", time, " per kilometer.")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
