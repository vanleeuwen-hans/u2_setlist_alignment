ui <- fluidPage(
  
  # Application title
  titlePanel("U2 Setlist Alignment"),
  
  # Short description and links
  HTML("<small><small>This U2 Setlist Alignment visualization was created as part of the Capstone Project for my Google Data Analytics certification. The use of the data was with kind permission from Matt @ <a href='https://www.u2gigs.com'>u2gigs.com</a>. The code for this app can be found in my GitHub repository at: <a href='https://github.com/vanleeuwen-hans/u2_setlist_alignment'>https://github.com/vanleeuwen-hans/u2_setlist_alignment</a>. The explanation and methods for the multiple setlist alignment can be read at <a href='https://vanleeuwen-hans.github.io/u2_data_analytics/multiple-setlist-alignment.html#multiple-setlist-alignment-with-mafft'>U2 Data Analytics > Multiple Setlist Alignment</a>.Hans van Leeuwen - November 2024.</small></small></br></br>"),
  
  # tour selection and max shows input
  fluidRow(
    column(
      width = 5,
      selectInput(
        "selected_tour", 
        "Choose a Tour:", 
        choices = unique_tours
      )
    ),
    # Show the plot output 
    plotOutput("tour_visualization")
  )
)




