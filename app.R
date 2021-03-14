library(shiny)
library(DT)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Running pace calculator"),
  h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$div(
        tags$p("Type in the distance and time you plan to run below to find your necessary pace and the splits."),
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
      tags$h4("Pace"),
      tags$text("You need to run:"),
      tags$ul(
        tags$li(uiOutput("results")),
        tags$li(uiOutput("results2")),
      ),
      br(),
      tags$h4("Splits"),
      DT::dataTableOutput("tbl")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$results <- renderUI({

    # compute pace
    time_total <- sum(c(input$hours * 3600, input$minutes * 60, input$seconds), na.rm = TRUE)
    time <- time_total / ifelse(input$units == "km", input$distance_km, input$distance_mile)
    pace <- paste0(time %/% 60, ":", sprintf("%02.0f", time %% 60))

    # display results
    paste0(pace, ifelse(input$units == "km", " minutes/kilometer", " minutes/mile"))
  })

  output$results2 <- renderUI({

    # compute pace
    time_total <- sum(c(input$hours * 3600, input$minutes * 60, input$seconds), na.rm = TRUE)
    time_h <- time_total / 3600
    pace <- ifelse(input$units == "km", input$distance_km, input$distance_mile) / time_h

    # display results
    paste0(round(pace, 2), ifelse(input$units == "km", " kilometers/hour", " miles/hour"))
  })

  # Data output
  output$tbl <- DT::renderDataTable({
    distance <- 1:ifelse(input$units == "km", input$distance_km, input$distance_mile)
    time_total <- sum(c(input$hours * 3600, input$minutes * 60, input$seconds), na.rm = TRUE)
    time <- time_total / ifelse(input$units == "km", input$distance_km, input$distance_mile) * distance
    td <- seconds_to_period(time)

    dat <- data.frame(
      Distance = distance,
      Time = ifelse(
        day(td) > 0,
        sprintf("%02.0f %02.0f:%02.0f:%02.0f", day(td), td@hour, minute(td), second(td)),
        sprintf("%02.0f:%02.0f:%02.0f", td@hour, minute(td), second(td))
      )
    )

    DT::datatable(dat,
      extensions = "Buttons",
      options = list(
        lengthChange = TRUE,
        dom = "Blrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),

        lengthMenu = list(c(-1, 10, 20, 50, 100), c("All", "10", "20", "50", "100"))
      ),
      rownames = FALSE,
      colnames = c(paste0("Distance ", ifelse(input$units == "km", "(kms)", "(miles)")), "Time (hh:mm:ss)")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
