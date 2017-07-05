library(shiny)
library(shinydashboard)


info.tab <- fluidRow(
    box(title = "Introduction", h4("This app is created for exploration of IRT models."))
)

ui <- dashboardPage(
  dashboardHeader(title = "mirtExplorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information",
          tabName = "info", icon = icon("dashboard")),
      menuItem("Comparsion with raw score",
          tabName = "rawscorecmp", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "info", info.tab),
      tabItem(tabName = "rawscorecmp",
              h2("Widgets tab content"),
              shinydashboard::box(width = 6,
                plotly::plotlyOutput(outputId = "corrPlot")),
              shinydashboard::box(width = 6,
                shiny::plotOutput(outputId = "plot"))
      )
    )
  )
)
