dataUI <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        shinydashboard::box(
            title = "Data import",
            width = 12,
            shiny::radioButtons(
                ns("dataSelection"),
                label = "Selection",
                choices = c(
                    "Upload dataset", "Use example data"
                ), selected = "Upload dataset"
            ),
            shiny::conditionalPanel(
                ns = shiny::NS(id),
                condition = "input.dataSelection == 'Upload dataset'",
                shiny::fileInput(
                    ns("file"), "",
                    multiple = FALSE,
                    accept = c("text", ".csv")
                ),
                shiny::htmlOutput(ns("filename"))
            ),
            shiny::conditionalPanel(
                ns = shiny::NS(id),
                condition = "input.dataSelection == 'Use example data'",
                shiny::h5("Oyster data")
            )
        ),
        shinydashboard::box(
            title = "Summary",
            shiny::htmlOutput(ns("nObservations")),
            shiny::htmlOutput(ns("nDilutions")),
            shiny::htmlOutput(ns("dilutions"))
        ),
        shinydashboard::box(
            title = "Input data",
            shiny::tableOutput(ns("indataTable"))
        )
    )
}

dataServer <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        inputData <- shiny::reactive({
            if (input$dataSelection == "Upload dataset") {
                shiny::req(input$file)
                tryCatch(
                    {
                        readFoodVirusData(
                            input$file$datapath
                        )
                    },
                    error = function(cond) {
                        shiny::showNotification(
                            "Failed to import the data from the file correctly.\n
                        Please check that the file format is correct (.txt) and that
                            the data is correctly formatted, with '.' as decimal separator.\n",
                            type = "error",
                            duration = NULL
                        )
                    },
                    silent = TRUE
                )
            } else if (input$dataSelection == "Use example data") {
                utils::data("oyster", package = "foodvirus")
                oyster
            }
        })

        output$filename <- shiny::renderText({
            shiny::req(is(inputData(), "FoodVirusData"))
            if (input$dataSelection == "Upload dataset") {
                paste0(
                    "'", input$file[[1]], "' was successfully uploaded! "
                )
            } else {
                NULL
            }
        })

        output$nObservations <- shiny::renderText({
            shiny::req(is(inputData(), "FoodVirusData"))
            paste("Total number of observations:", nrow(inputData()))
        })

        output$nDilutions <- shiny::renderText({
            shiny::req(is(inputData(), "FoodVirusData"))
            paste(
                "Number of dilution levels:",
                length(unique(inputData()$dilution))
            )
        })

        output$indataTable <- shiny::renderTable({
            shiny::req(is(inputData(), "FoodVirusData"))
            x <- as.data.frame(inputData())
            names(x) <- c("Dilution", "Obtained")
            x
        })

        list(data = shiny::reactive(inputData()))
    })
}
