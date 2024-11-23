if (!require("concertData")) {
  remotes::install_github("https://github.com/vanleeuwen-hans/concertData")
  library(concertData)
}

server <- function(input, output, session) {
  tour_data <- reactive({
    req(input$selected_tour)
    tour_data_list[[input$selected_tour]]
  })
  
  tour_song_codes <- reactive({
    req(input$selected_tour)
    tour_song_codes_list[[input$selected_tour]]
  })
  
  alignment_data <- reactive({
    req(input$selected_tour)
    read_mafft_clustal_alignment(get_alignment_filename(input$selected_tour))
  })
  
  output$tour_visualization <- renderPlot({
    req(tour_data(), tour_song_codes(), alignment_data())
    prepare_and_create_visualization(
      tour_data(),
      tour_song_codes(),
      alignment_data(),
      input$selected_tour
    )
  })
}