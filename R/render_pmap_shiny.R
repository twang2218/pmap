#' @title Render process map as a web app
#' @description Show the given process map in a Shiny server web application, with ability to adjust the nodes and edges precision in the real time.
#' @usage render_pmap_shiny(
#'    p,
#'    title = "Process Map",
#'    nodes_prune_percentage = 0.5,
#'    edges_prune_percentage = 0.5,
#'    options = NULL)
#' @param p The process map created by `create_pmap()`
#' @param title The title you want to display on the web page
#' @param nodes_prune_percentage How many percentage of nodes should be pruned. Default is `0.5`
#' @param edges_prune_percentage How many percentage of edges should be pruned. Default is `0.5`
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
render_pmap_shiny <- function(p, title = "Process Map", nodes_prune_percentage = 0.5, edges_prune_percentage = 0.5, options = NULL) {
  processmap <- p

  ui <- shiny::fluidPage(
    # Application title
    shiny::titlePanel(title),

    # Sidebar with a slider input for graph precision
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput("nodes_prune_percentage",
                    "Nodes Prune Percentage:",
                    min = 1,
                    max = 100,
                    value = nodes_prune_percentage * 100),
        shiny::sliderInput("edges_prune_percentage",
                    "Edges Prune Percentage:",
                    min = 1,
                    max = 100,
                    value = edges_prune_percentage * 100),
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
      processmap %>%
        prune_nodes(input$nodes_prune_percentage / 100) %>%
        prune_edges(input$edges_prune_percentage / 100) %>%
        render_pmap()
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server, options = options)
}
