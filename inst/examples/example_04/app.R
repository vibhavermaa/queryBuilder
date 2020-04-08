library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(htmlwidgets)
library(dplyr)
library(rlang)
library(bupaR)
library(edeaR)
library(customQueryBuilder)
library(eventdataR)

df <- readRDS("/Users/vibha_verma/Documents/mckpi_doc/data/veeva_vault_eventlog.rds")
# df <- eventdataR::sepsis
df <- head(df,100)

ui <- dashboardPagePlus(
    header = dashboardHeaderPlus
    (
        title               = "Testing",
        enable_rightsidebar = TRUE,
        rightSidebarIcon    = "filter"
    ),

    sidebar = dashboardSidebar(
        tags$head(tags$style(HTML(".content { height: 99vh; overflow-y: auto; }" ))),
        tags$head(
            tags$script(src="https://d3js.org/d3.v5.min.js")
        ),
        sidebarMenu(
            menuItem
            (
                "Adv Analytics",                tabName = "adv_analytics", icon = icon("compass", lib = "font-awesome"),
                menuSubItem("Advanced Filters", tabName = "adv_filters",   icon = icon("compass", lib = "font-awesome"))
            )
        )
    ),

    body = dashboardBody
    (
        tabItem
        (
            tabName = "adv_filters", h2("Advanced Filters"),
            fluidRow(
                column(
                    12,
                    box(
                        title = "Enter advanced filters here:",
                        status = "info",
                        width = NULL,
                        queryBuilderOutput('queryBuilder', width = "100%", height = 200)
                    ),
                )
            ),
            fluidRow(
                column(
                    12,
                    style = "overflow-y: auto",
                    box(
                        title  = "Eventlog preview:",
                        status = "info",
                        width  = "100%",
                        height = "100%",
                        div(
                            style = "overflow-y: auto; overflow-x: auto",
                            DT::dataTableOutput("adv_filters_preview")
                        )
                    )
                )
            )
        )
    )
)

server <- function(input,output,session){
    output$queryBuilder <- renderQueryBuilder({
        req(df)
        queryBuilder(data = df,
                     autoassign = TRUE,
                     allow_empty = TRUE)

    })

    # render advanced filter preview
    adv_filters_preview <- reactive({
        x <- input$queryBuilder_out
        if(!is.null(input$queryBuilder_validate) && (input$queryBuilder_validate == TRUE))
        {
            results <- filterBupaRTable(input$queryBuilder_out, data = df)
            return(results)
        }
        else
        {
            results <- df
            return(results)
        }
    })


    # adv_filters_preview
    output$adv_filters_preview = DT::renderDataTable({
        adv_filters_preview()
    })
}

shinyApp(ui, server)
