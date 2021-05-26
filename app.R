library(shiny)
library(shinydashboard)
source("functions.R")

# Conditional panels ===========================================================

upload <- conditionalPanel(
    condition = "input.fileInput == 'Upload file'",
    fileInput(
        "file1", "", multiple = FALSE, accept = c("text")
    )
)
useExample <- conditionalPanel(
    condition = "input.fileInput == 'Use example data'",
)

# UI ===========================================================================

ui <- dashboardPage(
    dashboardHeader(title = "EURL verguide"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Welcome!", tabName = "instructions"),
            menuItem("Data import", tabName = "import"),
            menuItem("Performance characteristics", tabName = "verify")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "instructions",
                    fluidRow(
                        box(
                            title = ("Instructions for use"),
                            h5(
                                "This application is under development, and 
                                the output and format may change over time."
                            ),
                            h5(
                                "If you experience any problems or want to 
                                report bugs, please contact us at 
                                eurlfoodvirus@slv.se."
                            ),
                            br(),
                            uiOutput("codelink"),
                            width = 12
                        )
                    )
            ),
            tabItem(tabName = "import",
                    fluidRow(
                        box(
                            radioButtons(
                                "fileInput",
                                label = NULL,
                                choices = c("Upload file", "Use example data"),
                                selected = "Upload file"
                            ),
                            upload,
                            useExample
                        ),
                        box(
                            title = "Qualitative data",
                            downloadLink("download1", "Download"),
                            br(),
                            br(),
                            tableOutput("table2"),
                            width = 12
                        ),
                        box(
                            title = "Quantitative data",
                            downloadLink("download2", "Download"),
                            br(),
                            br(),
                            tableOutput("table1"),
                            width = 12
                        ),
                    )
            ),
            tabItem(tabName = "verify",
                    fluidRow(
                        box(
                            title = "Lowest value for linearity and LOQ",
                            uiOutput("from"),
                            width = 12
                        ),
                        box(
                            title = "Linearity",
                            plotOutput("plot2", height = 300, width = 400),
                            width = 12
                        ),
                        box(
                            title = "Precision and LOQ",
                            plotOutput("plot3", height = 300, width = 400),
                            br(),
                            hr(),
                            br(),
                            downloadLink("download3", "Download"),
                            br(),
                            br(),
                            tableOutput("table3"),
                            width = 12
                        )
                    )
            )
        )
        
    )
)

# Server =======================================================================

server <- function(input, output) {
    
    mydata <- reactive({
        if (input$fileInput == "Upload file") {
            req(input$file1)
            x <- import_verification_data(input$file1$datapath)
            add_detected_anticipated(x)
        } else if (input$fileInput == "Use example data") {
            data("example_verification_data")
            x <- example_verification_data
            add_detected_anticipated(x)
        }
    })
    
    output$table1 <- renderTable({
        req(mydata())
        mydata()
    })
    
    output$table2 <- renderTable({
        req(mydata())
        count_table(mydata())
    })
    
    output$table3 <- renderTable({
        req(mydata())
        precision_table(mydata())
    })
    
    output$download1 <- downloadHandler(
        filename = function() {
            paste0("qualitative_data-", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(count_table(mydata()), file)
        })
    
    output$download2 <- downloadHandler(
        filename = function() {
            paste0("quantitative_data-", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(mydata(), file)
        })
    
    output$download3 <- downloadHandler(
        filename = function() {
            paste0("sd-", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(precision_table(mydata()), file)
        })
    
    output$from <- renderUI({
        req(mydata())
        selectInput(
            "from", 
            label = "",
            choices = round(unique(mydata()$Anticipated), 2), 
            selected = round(e_lod(mydata()), 2),
            width = 400
        )
    })
    
    output$plot2 <- renderPlot({
        req(mydata())
        req(input$from)
        plot_linearity(mydata(), as.numeric(input$from))
    })
    
    output$plot3 <- renderPlot({
        req(mydata())
        req(input$from)
        plot_precision(mydata(), as.numeric(input$from))
    })
    
    url <- a("Source code", href="https://github.com/sofpn/EURL-verguide")
    output$codelink <- renderUI({
        tagList(url)
    })
    
    
}

# Run app ======================================================================

shinyApp(ui, server)
