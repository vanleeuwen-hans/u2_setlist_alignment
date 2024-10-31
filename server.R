server <- function(input, output, session) {
  
  max_shows <- reactive({
    as.numeric(input$max_shows)
  })
  

  output$setlistPlot <- renderPlot({
    req(input$tour)
    tryCatch({
      create_setlist_alignment(u2data, input$tour, max_shows = max_shows())
    }, error = function(e) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, paste("Error:", e$message), cex = 1.5)
    })
  })
}