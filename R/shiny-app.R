#' Open the EURLVerguide application
#'
#' @return Opens a Shiny application.
#'
#' @export
#'
#' @examples
#' ## Only run this in interactive R sessions:
#' if (interactive()) {
#'     runEURLVerguide()
#' }
runEURLVerguide <- function() {
    ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "EURLVerguide"),
        shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
                shinydashboard::menuItem("Welcome", tabName = "welcome"),
                shinydashboard::menuItem("Data import", tabName = "data"),
                shinydashboard::menuItem("Anticipated and detected", tabName = "anticipated"),
                shinydashboard::menuItem(
                    "Performance characteristics", tabName = "verify"
                )
            )
        ),
        shinydashboard::dashboardBody(
            shinydashboard::tabItems(
                shinydashboard::tabItem(
                    tabName = "welcome",
                    shiny::fluidRow(
                        welcomeUI("welcome")
                    )
                ),
                shinydashboard::tabItem(
                    tabName = "data",
                    shiny::fluidRow(
                        dataUI("data")
                    )
                ),
                shinydashboard::tabItem(
                    tabName = "anticipated",
                    shiny::fluidRow(
                        anticipatedUI("anticipated")
                    )
                ),
                shinydashboard::tabItem(
                    tabName = "verify",
                    shiny::fluidRow(
                        verifyUI("verify")
                    )
                )
            )
        )
    )

    server <- function(input, output, session) {
        inputData <- dataServer("data")
        anticipatedServer("anticipated", data = inputData$data)
        verifyServer("verify", data = inputData$data)
    }

    shiny::shinyApp(ui, server)
}
