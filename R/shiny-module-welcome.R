welcomeUI <- function(id) {
    shiny::tagList(
        shinydashboard::box(
            title = ("Welcome to the EURLVerguide-application!"),
            shiny::h5(shiny::tags$b(
                "This application helps to determine performance
                characteristics during verification and in-house validation
                of quantitative PCR-based methods in food virology."
            )),
            shiny::h5(
                "For detailed instructions on how to use the application,
                please see this document (ADD LINK)"
            ),
            shiny::hr(),
            shiny::h5("R-package foodvirus, version 0.1.0"),
            shiny::tags$h5(shiny::tags$a(
                href = "https://github.com/sofpn/foodvirus",
                "View source code", shiny::icon("github")
            )),
            shiny::tags$h5(shiny::tags$a(
                href = "https://github.com/sofpn/foodvirus/issues",
                "Report bugs", shiny::icon("github")
            )),
            shiny::h5("Contact us: eurlfoodvirus@slv.se"),
            shiny::h5(
                "Disclaimer: this application is under development.
                The format may change over time and we leave no
                guarantee for its correctness."
            ),
            width = 12
        )
    )
}
