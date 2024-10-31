# Load necessary libraries
library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringdist)
library(stringr)
library(gridExtra)
library(shinycssloaders)

# Improved function to create acronyms or shortened titles
create_short_title <- function(title, use_acronyms = TRUE) {
  if (is.na(title) || title == "") return("")
  
  # Remove special characters at the start of each word
  words <- str_split(title, "\\s+")[[1]]  # Split by spaces
  words <- str_replace_all(words, "^\\W+", "")  # Remove leading special characters from each word
  
  if (use_acronyms) {
    if (length(words) == 1) {
      return(toupper(substr(title, 1, 4)))  # First 4 letters for single-word titles
    } else {
      return(toupper(paste(substr(words, 1, 1), collapse = "")))  # Acronym for multi-word titles
    }
  } else {
    return(substr(title, 1, 4))  # First 4 letters for all titles if acronyms are not used
  }
}

# Function to align sequences
align_sequences <- function(seq1, seq2) {
  max_length <- max(length(seq1), length(seq2))
  seq1 <- c(seq1, rep(NA, max_length - length(seq1)))
  seq2 <- c(seq2, rep(NA, max_length - length(seq2)))
  
  alignment <- stringdist::stringdist(seq1, seq2, method = "lv")
  similarity <- 1 - (alignment / max_length)
  list(alignment = alignment, similarity = similarity)
}

# Main function
create_setlist_alignment <- function(data, tour_name, max_shows = 20, use_acronyms = TRUE) {
  # Filter data for the specified tour and exclude snippets
  tour_data <- data %>%
    filter(tour == tour_name, !snippet) %>%
    filter(!is.na(song_position) & !is.na(song_title)) %>%
    arrange(date, song_position)
  
  # Create sequence of songs for each show
  show_sequences <- tour_data %>%
    group_by(date) %>%
    summarise(sequence = list(song_title), .groups = "drop")
  
  # Perform pairwise alignments
  alignments <- tryCatch({
    combn(show_sequences$date, 2, function(pair) {
      seq1 <- show_sequences$sequence[[which(show_sequences$date == pair[1])]]
      seq2 <- show_sequences$sequence[[which(show_sequences$date == pair[2])]]
      result <- align_sequences(seq1, seq2)
      data.frame(date1 = pair[1], date2 = pair[2], 
                 similarity = result$similarity, 
                 alignment = result$alignment)
    }, simplify = FALSE) %>% bind_rows()
  }, error = function(e) {
    warning("Error in alignment calculation: ", e$message)
    return(NULL)
  })
  
  if (is.null(alignments)) {
    stop("Unable to calculate alignments. Check your data for inconsistencies.")
  }
  
  # Select representative setlists
  representative_dates <- alignments %>%
    group_by(date1) %>%
    summarise(avg_similarity = mean(similarity, na.rm = TRUE)) %>%
    arrange(desc(avg_similarity)) %>%
    head(max_shows) %>%
    pull(date1)
  
  # Prepare data for visualization
  viz_data <- tour_data %>%
    filter(date %in% representative_dates) %>%
    group_by(date) %>%
    mutate(position = row_number(),
           short_title = sapply(song_title, create_short_title, use_acronyms = use_acronyms)) %>%
    ungroup()
  
  # Create legend for acronyms/short titles
  legend_data <- viz_data %>%
    distinct(song_title, short_title) %>%
    arrange(song_title)
  
  legend_text <- paste(legend_data$short_title, "=", legend_data$song_title, collapse = ", ")
  
  # Create the main plot
  main_plot <- ggplot(viz_data, aes(x = position, y = factor(date))) +
    geom_tile(aes(fill = song_title), color = "white", linewidth = 0.5) +
    geom_text(aes(label = short_title), size = 2.5) +
    scale_fill_viridis_d(option = "viridis") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 13),
          legend.position = "none") +
    labs(x = "Song Position", y = "Show Date",
         title = paste("Setlist Alignment for", tour_name),
         subtitle = paste("Showing", max_shows, "most representative setlists"))
  
  # Create a separate plot for the legend that explains song acronyms
  legend_plot <- ggplot() +
    geom_text(aes(x=0.5, y=0.5, label=str_wrap(legend_text, width=200)), 
              size=3.5, hjust=0.5) +
    theme_void() + 
    theme(plot.margin=margin(t=10))
  
  # Combine plots using gridExtra
  combined_plot <- grid.arrange(main_plot,
                                legend_plot,
                                ncol=1,
                                heights=c(4, 1)) 
  return(combined_plot)
}

# Function to safely read data with error handling
safe_read_data <- function(file_path) {
  tryCatch({
    read_csv(file_path, show_col_types = FALSE)
  }, error = function(e) {
    stop(paste("Error reading data file:", e$message))
  })
}

# Load data
u2data <- safe_read_data('u2data_all_shows_clean_final.csv')