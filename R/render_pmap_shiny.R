#' @title Render process map as a web app
#' @description Show the given process map in a Shiny server web application, with ability to adjust the nodes and edges precision in the real time.
#' @param p The process map created by `create_pmap()`
#' @param title The title you want to display on the web page
#' @param options The Shiny server options, such as binding address or listening port.
#' @examples
#' # We can use the real life event log from 'eventdataR' package to do the shiny app demo
#' library(eventdataR)
#' library(dplyr)
#' eventlog <- eventdataR::sepsis %>%
#'   rename(
#'     timestamp = Complete_Timestamp,
#'     customer_id = Case_ID,
#'     event_name = Activity
#'   ) %>%
#'   mutate(
#'     event_type = event_name
#'   ) %>%
#'   select(timestamp, customer_id, event_name, event_type) %>%
#'   filter(!is.na(customer_id))
#' app <- eventlog %>% create_pmap() %>% render_pmap_shiny()
#' # To run the app, just print(app)
#' @importFrom shiny        fluidPage
#' @importFrom shiny        titlePanel
#' @importFrom shiny        sidebarLayout
#' @importFrom shiny        sidebarPanel
#' @importFrom shiny        sliderInput
#' @importFrom shiny        mainPanel
#' @importFrom shiny        shinyApp
#' @importFrom DiagrammeR   grVizOutput
#' @importFrom DiagrammeR   renderGrViz
#' @importFrom dplyr        %>%
#' @export
render_pmap_shiny <- function(p, title = "Process Map", options = NULL) {
  processmap <- p

  ui <- fluidPage(
    # Application title
    titlePanel(title),

    # Sidebar with a slider input for graph precision
    sidebarLayout(
      sidebarPanel(
        sliderInput("keep_edges_size",
                    "Edges precision:",
                    min = 1,
                    max = 100,
                    value = 80),
        sliderInput("keep_nodes_size",
                    "Nodes precision:",
                    min = 1,
                    max = 100,
                    value = 80),
        width = 2
      ),

      # Show Diagram
      mainPanel(
        DiagrammeR::grVizOutput("diagram", width = "100%", height = "800px"),
        width = 10
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {
    # print("[server] str(processmap):")
    # print(str(processmap))
    output$diagram <- DiagrammeR::renderGrViz({
      render_pmap(
        processmap %>%
          prune_nodes(1 - (input$keep_nodes_size / 100)) %>%
          prune_edges(1 - (input$keep_edges_size / 100))
      )
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server, options = options)
}
