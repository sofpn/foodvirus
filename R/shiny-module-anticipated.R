anticipatedUI <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        shinydashboard::box(
            title = "Summary",
            shiny::downloadLink(ns("downloadQualitative"), "Download"),
            shiny::br(),
            shiny::br(),
            shiny::tableOutput(ns("qualitativeData")),
            width = 6
        ),
        shinydashboard::box(
            title = "All observations",
            shiny::downloadLink(ns("downloadQuantitative"), "Download"),
            shiny::br(),
            shiny::br(),
            shiny::tableOutput(ns("quantitativeData")),
            width = 6
        )
    )
}

anticipatedServer <- function(id, data) {
    shiny::moduleServer(id, function(input, output, session) {
        processedData <- shiny::reactive({
            shiny::req(data())
            addAnticipatedDetected(data())
        })

        output$qualitativeData <- shiny::renderTable({
            shiny::req(processedData())
            x <- processedData()
            x <- countTable(x)
            names(x) <- c(
                "Anticipated conc.",
                "Number of observations",
                "Number of positive observations",
                "Proportion of positive observations"
            )
            x
        })

        output$quantitativeData <- shiny::renderTable({
            shiny::req(processedData())
            x <- as.data.frame(processedData())
            names(x) <- c(
                "Dilution level",
                "Anticipated conc.",
                "Obtained conc.",
                "Detected"
            )
            x
        })

        output$downloadQualtiative <- shiny::downloadHandler(
            filename = function() {
                paste0("summary-", Sys.Date(), ".csv")
            },
            content = function(file) {
                utils::write.csv(countTable(processedData()), file)
            })

        output$downloadQuantitative <- shiny::downloadHandler(
            filename = function() {
                paste0("all_observations-", Sys.Date(), ".csv")
            },
            content = function(file) {
                utils::write.csv(processedData(), file)
            })
    })

}
