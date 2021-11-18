verifyUI <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        shinydashboard::box(
            title = "Linearity",
            shiny::plotOutput(ns("linearityPlot"), height = 300, width = 400),
            width = 6
        ),
        shinydashboard::box(
            title = "Precision and LOQ",
            shiny::plotOutput(ns("precisionPlot"), height = 300, width = 400),
            witdh = 6
        ),
        shinydashboard::box(
            title = "Lowest value for linearity and LOQ",
            shiny::uiOutput(ns("from")),
            width = 6
        ),
        shinydashboard::box(
            title = "Table",
            shiny::downloadLink(ns("downloadPrecision"), "Download"),
            shiny::br(),
            shiny::br(),
            shiny::tableOutput(ns("precisionTable")),
            width = 6
        )
    )
}

verifyServer <- function(id, data) {
    shiny::moduleServer(id, function(input, output, session) {

        verificationData <- shiny::reactive({
            shiny::req(data())
            addAnticipatedDetected(data())
            })

        output$from <- shiny::renderUI({
            shiny::req(verificationData())
            ns <- session$ns
            shiny::selectInput(
                ns("from"),
                label = "",
                choices = round(unique(verificationData()$anticipated), 2),
                selected = round(eLod(verificationData()), 2),
                width = 400
            )
        })

        output$downloadPrecision <- shiny::downloadHandler(
            filename = function() {
                paste0("precision-", Sys.Date(), ".csv")
            },
            content = function(file) {
                utils::write.csv(precisionTable(verificationData()), file)
            })


        output$precisionTable <- shiny::renderTable({
            shiny::req(verificationData())
            x <- precisionTable(verificationData())
            names(x) <- c(
                "Anticipated", "SD", "%CV", "Anticipated, log10", "SD, log10"
            )
            x
        })

        output$linearityPlot <- shiny::renderPlot({
            shiny::req(verificationData())
            shiny::req(input$from)
            plotLinearity(verificationData(), as.numeric(input$from))
        })

        output$precisionPlot <- shiny::renderPlot({
            shiny::req(verificationData())
            shiny::req(input$from)
            plotPrecision(verificationData(), as.numeric(input$from))
        })

    })
}
