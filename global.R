# load libraries
# First, load all required libraries
library(shiny)
library(dplyr)
library(stringr)
library(shinycssloaders)
library(pryr)
library(ggplot2)
library(gridExtra)

# Use custom library functions
# Note: using my concertData package from GitHub 
# resulted in compiling errors during shinyapps.io deployment,
# this is a workaround
source("packages/concertData/R/tourSetlistAlignment.R")
source("packages/concertData/R/utils.R")
source("packages/concertData/R/tourStatistics.R")
source("packages/concertData/R/tourSetlistDistanceTree.R")
source("packages/concertData/R/tourWorldMaps.R")
source("packages/concertData/R/tourSetlistVariability.R")



# Define helper functions first
get_alignment_filename <- function(tour_name) {
  paste0(
    "data/alignments/u2_setlists_mafft_alignment_",
    gsub("[^[:alnum:]]", "_", tour_name),
    ".ASCII"
  )
}

get_codes_filename <-function(tour_name) {
  paste0(
    "data/codes/u2_setlists_alignments_codes_",
    gsub("[^[:alnum:]]", "_", tour_name),
    ".rds")
}

create_visualization <- function(viz_data, tour_name, num_setlists) {
  # Create legend
  legend_data <- viz_data[!viz_data$is_gap, c("song_title", "four_letter_code")]
  legend_data <- unique(legend_data[order(legend_data$song_title), ])
  legend_text <- paste(legend_data$four_letter_code, "=", legend_data$song_title, collapse = ", ")
  
  # Create color scale
  n_songs <- length(unique(viz_data$song_title[!viz_data$is_gap]))
  song_colors <- create_distinct_palette(n_songs)
  names(song_colors) <- unique(viz_data$song_title[!viz_data$is_gap])
  song_colors <- c(song_colors, GAP = "white")
  
  # Prepare data
  viz_data$city_date <- paste(viz_data$city, "-", as.character(viz_data$date))
  viz_data$show_id <- seq_len(nrow(viz_data))
  viz_data$city_date <- factor(viz_data$city_date, 
                               levels = unique(viz_data$city_date[viz_data$show_id]))
  
  # Create main plot
  main_plot <- ggplot(viz_data, aes(x = position, y = city_date)) +
    geom_tile(aes(fill = song_title), color = "grey90", linewidth = 0.5) +
    geom_text(aes(label = ifelse(is_gap, "", four_letter_code)), 
              size = 3,
              color = "white") +
    scale_fill_manual(values = song_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          legend.position = "none",
          panel.grid = element_blank()) +
    labs(x = "Song Position", 
         y = "City - Show Date",
         title = paste("Setlist Alignment for", tour_name),
         subtitle = paste("Showing", num_setlists, 
                          "most representative setlists. Empty white cells indicate inserted/skipped songs"))
  
  # Create legend plot
  legend_plot <- ggplot() +
    geom_text(aes(x = 0.5, y = 0.5, label = str_wrap(legend_text, width = 250)), 
              size = 3, hjust = 0.5) +
    theme_void() + 
    theme(plot.margin = margin(t = 10))
  
  # Return arranged plots
  grid.arrange(main_plot,
               legend_plot,
               ncol = 1,
               heights = c(4, 1))
}

prepare_and_create_visualization <- function(tour_data, tour_song_codes, alignment_data, tour_name) {
  # Prepare song code lookup
  song_code_lookup <- tour_song_codes[, c("hex_char", "four_letter_code", "song_title")]
  
  # Create visualization data
  viz_data <- create_setlist_viz_data(
    alignment_data, 
    song_code_lookup, 
    tour_data
  )
  
  # Get number of setlists
  num_setlists <- length(unique(alignment_data$showID))
  
  # Create and return visualization
  create_visualization(viz_data, tour_name, num_setlists)
}

# Read and prepare data with error handling
tryCatch({
  # Define excluded tours
  excluded_tours <- c(
    "U2 Stories of Surrender Tour",
    "U2 Songs Of Experience Promo Tour",
    "U2 Songs Of Innocence Promo Tour",
    "U2 No Line On The Horizon Promo Tour",
    "U2 How To Dismantle An Atomic Bomb Promo Tour",
    "U2 All That You Can't Leave Behind Promo Tour",
    "U2 Conspiracy Of Hope",
    "U2 11 O'Clock Tick Tock Tour",
    "U2 Early Days",
    "U2 Various Dates"
  )
  
  # Read data
  u2data <- read_concertData_csv('data/u2data_all_shows_clean_final.csv')
  
  # Process data
  no_snippets_data <- concertData_remove_snippets(u2data)
  selected_tours_data <- concertData_remove_tours(no_snippets_data, excluded_tours)
  setlists_shows_data <- concertData_remove_showsNoSetlist(selected_tours_data)
  

  # Prepare tour list for dropdown
  unique_tours <- unique(setlists_shows_data$tour)

  # Pre-calculate tour data and song codes 
  tour_data_list <- list()
  tour_song_codes_list <- list()
  
  for(tour_name in unique_tours) {
    tour_data_list[[tour_name]] <- filter(setlists_shows_data, tour == tour_name)
    # read file with tour song codes
    tour_song_codes_list[[tour_name]] <- readRDS(get_codes_filename(tour_name))
  }
}, error = function(e) {
  stop("Error in data preparation: ", e$message)
})

