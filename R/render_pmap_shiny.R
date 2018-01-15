#' @title Render process map as a web app
#' @description Show the given process map in a Shiny server web application, with ability to adjust the nodes and edges precision in the real time.
#' @param p The process map created by `create_pmap()`
#' @param title The title you want to display on the web page
#' @param options The Shiny server options, such as binding address or listening port.
#' @examples
#' library(pmap)
#' # Generate a random eventlog
#' eventlog <- generate_eventlog()
#' # Create the process map from the event log
#' p <- create_pmap(eventlog)
#' # Render process map as Shiny app
#' # render_pmap_shiny(p)
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

  ui <- shiny::fluidPage(
    # Application title
    shiny::titlePanel(title),

    # Sidebar with a slider input for graph precision
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput("keep_edges_size",
                    "Edges precision:",
                    min = 1,
                    max = 100,
                    value = 80),
        shiny::sliderInput("keep_nodes_size",
                    "Nodes precision:",
                    min = 1,
                    max = 100,
                    value = 80),
        width = 2
      ),

      # Show Diagram
      shiny::mainPanel(
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
  shiny::shinyApp(ui = ui, server = server, options = options)
}
