
#' @title Find Longest Setlist by Tour
#' @description Calculates the maximum number of songs played in a single show for each tour.
#'
#' @param data A data frame in concertData format, containing at least 'tour' and 'song_position' columns.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item tour: Name of the tour
#'     \item max_songs: Maximum number of songs played in a single show for that tour
#'   }
#'
#' @examples
#' \dontrun{
#' longest_setlists <- longest_setlist_by_tour(concert_data)
#' print(longest_setlists)
#' }
#'
#' @export
longest_setlist_by_tour <- function(data) {
  # Check if required columns exist
  if (!all(c("tour", "song_position") %in% names(data))) {
    stop("Input data frame must contain 'tour' and 'song_position' columns")
  }

  # Calculate max songs for each tour
  max_songs <- tapply(data$song_position, data$tour, max, na.rm = TRUE)

  # Convert to data frame
  max_songs_by_tour <- data.frame(
    tour = names(max_songs),
    max_songs = as.vector(max_songs)
  )

  # Sort by tour name (optional, remove if not needed)
  max_songs_by_tour <- max_songs_by_tour[order(max_songs_by_tour$tour), ]

  # Reset row names
  rownames(max_songs_by_tour) <- NULL

  return(max_songs_by_tour)
}


#' @title Generate Setlist Data for Each Show
#' @description Creates a summary of setlist data for each show, including the number of unique songs and a list of all songs played.
#'
#' @param data A data frame in concertData format, containing at least 'showID', 'tour', 'date', and 'song_title' columns.
#' @param tour_order A data frame with a 'tour' column specifying the desired order of tours (optional).
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item showID: Unique identifier for each show
#'     \item tour: Name of the tour (as a factor if tour_order is provided)
#'     \item date: Date of the show
#'     \item songs: Number of songs played in the show
#'     \item song_list: Semicolon-separated list of all songs played in the show
#'   }
#'
#' @examples
#' \dontrun{
#' setlist_summary <- generate_setlist_data(concert_data)
#' setlist_summary_ordered <- generate_setlist_data(concert_data, tour_order)
#' }
#'
#' @export
generate_setlist_data <- function(data, tour_order = NULL) {
  # Check if required columns exist
  required_cols <- c("showID", "tour", "date", "song_title")
  if (!all(required_cols %in% names(data))) {
    stop("Input data frame must contain 'showID', 'tour', 'date', and 'song_title' columns")
  }

  # Remove rows with NA song_title
  data <- data[!is.na(data$song_title), ]

  # Create a list to store results for each show
  results <- list()

  # Process each unique show
  unique_shows <- unique(data$showID)
  for (show in unique_shows) {
    show_data <- data[data$showID == show, ]

    results[[show]] <- data.frame(
      showID = show,
      tour = show_data$tour[1],
      date = show_data$date[1],
      songs = length(unique(show_data$song_title)),
      song_list = paste(show_data$song_title, collapse = "; ")
    )
  }

  # Combine results into a single data frame
  setlist_data <- do.call(rbind, results)

  # Order tours if tour_order is provided
  if (!is.null(tour_order) && "tour" %in% names(tour_order)) {
    setlist_data$tour <- factor(setlist_data$tour, levels = tour_order$tour)
  }

  # Ensure date is in Date format
  setlist_data$date <- as.Date(setlist_data$date)

  # Sort by date
  setlist_data <- setlist_data[order(setlist_data$date), ]

  # Reset row names
  rownames(setlist_data) <- NULL

  return(setlist_data)
}


#' @title Analyze Song Positions in Setlists
#' @description Performs an analysis of song positions within setlists across different tours.
#'
#' @param data A data frame in concertData format, containing at least 'tour', 'showID', 'song_title', and 'song_position' columns.
#' @param max_songs_by_tour A data frame with 'tour' and 'max_songs' columns, typically output from longest_setlist_by_tour function.
#' @param tour_order A data frame with a 'tour' column specifying the desired order of tours (optional).
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item tour: Name of the tour (as a factor if tour_order is provided)
#'     \item position_bin: The song position in the setlist
#'     \item unique_songs: Number of unique songs played in this position
#'     \item total_occurrences: Total number of songs played in this position
#'     \item most_common_song: The most frequently played song in this position
#'     \item most_common_freq: Frequency of the most common song
#'     \item predictability: Ratio of most_common_freq to total_occurrences
#'     \item variability: 1 - predictability
#'   }
#'
#' @examples
#' \dontrun{
#' max_songs <- longest_setlist_by_tour(concert_data)
#' tour_order <- tour_order_date(concert_data)
#' position_analysis <- analyze_song_positions(concert_data, max_songs, tour_order)
#' }
#'
#' @export
analyze_song_positions <- function(data, max_songs_by_tour, tour_order = NULL) {
  # Check if required columns exist
  required_cols <- c("tour", "showID", "song_title", "song_position")
  if (!all(required_cols %in% names(data))) {
    stop("Input data frame must contain 'tour', 'showID', 'song_title', and 'song_position' columns")
  }

  # Remove rows with NA song_title
  data <- data[!is.na(data$song_title), ]

  # Create normalized_position (equivalent to row_number() within each tour and showID)
  data <- data[order(data$tour, data$showID, data$song_position), ]
  data$normalized_position <- ave(data$song_position, data$tour, data$showID, FUN = seq_along)

  # Merge with max_songs_by_tour
  data <- merge(data, max_songs_by_tour, by = "tour", all.x = TRUE)

  # Create position_bin (use normalized_position as is)
  data$position_bin <- data$normalized_position

  # Perform analysis
  result <- by(data, list(data$tour, data$position_bin), function(x) {
    unique_songs <- length(unique(x$song_title))
    total_occurrences <- nrow(x)
    song_table <- table(x$song_title)
    most_common_song <- names(which.max(song_table))
    most_common_freq <- max(song_table)
    predictability <- most_common_freq / total_occurrences
    variability <- 1 - predictability

    data.frame(
      tour = x$tour[1],
      position_bin = x$position_bin[1],
      unique_songs = unique_songs,
      total_occurrences = total_occurrences,
      most_common_song = most_common_song,
      most_common_freq = most_common_freq,
      predictability = predictability,
      variability = variability
    )
  })

  # Combine results
  position_analysis <- do.call(rbind, result)

  # Order tours if tour_order is provided
  if (!is.null(tour_order) && "tour" %in% names(tour_order)) {
    position_analysis$tour <- factor(position_analysis$tour, levels = tour_order$tour)
  }

  # Sort by tour and position_bin
  position_analysis <- position_analysis[order(position_analysis$tour, position_analysis$position_bin), ]

  # Reset row names
  rownames(position_analysis) <- NULL

  return(position_analysis)
}

#' @title Calculate Comprehensive Tour Statistics
#' @description Computes various statistics for each tour, including show counts, song counts, and variability metrics.
#'
#' @param data A data frame in concertData format, containing at least 'tour', 'showID', 'song_title', and 'song_position' columns.
#' @param data_inc_snippets A data frame similar to 'data' but including snippet performances.
#' @param tour_order A data frame with a 'tour' column specifying the desired order of tours (optional).
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item tour: Name of the tour (as a factor if tour_order is provided)
#'     \item total_shows: Total number of shows in the tour
#'     \item avg_songs_per_show: Average number of songs per show
#'     \item total_unique_songs: Total number of unique songs played (excluding snippets)
#'     \item total_unique_snippets: Total number of unique snippets played
#'     \item overall_variability: Ratio of unique songs to average songs per show
#'   }
#'
#' @examples
#' \dontrun{
#' tour_statistics <- calculate_comprehensive_tour_statistics(concert_data, concert_data_with_snippets, tour_order)
#' }
#'
#' @export
calculate_comprehensive_tour_statistics <- function(data_inc_snippets, tour_order = NULL) {
  # Check if required columns exist
  required_cols <- c("tour", "showID", "song_title", "song_position")
  if (!all(required_cols %in% names(data_inc_snippets))) {
    stop("Input data frames must contain 'tour', 'showID', 'song_title', and 'song_position' columns")
  }

  # remove snippets
  data_no_snippets <- concertData_remove_snippets(data_inc_snippets)

  # Calculate songs in each show
  songs_per_show <- aggregate(song_position ~ tour + showID, data_no_snippets, max, na.rm = TRUE)
  names(songs_per_show)[3] <- "songs_in_show"

  # Calculate tour-level statistics
  tour_stats <- aggregate(songs_in_show ~ tour, songs_per_show,
                          FUN = function(x) c(total_shows = length(x),
                                              avg_songs_per_show = mean(x, na.rm = TRUE)))
  tour_stats <- do.call(data.frame, tour_stats)
  names(tour_stats)[2:3] <- c("total_shows", "avg_songs_per_show")

  # Calculate unique songs and snippets
  unique_songs <- tapply(data_no_snippets$song_title, data_no_snippets$tour, function(x) length(unique(x)))
  unique_snippets <- tapply(data_inc_snippets$song_title[data_inc_snippets$snippet],
                            data_inc_snippets$tour[data_inc_snippets$snippet],
                            function(x) length(unique(x)))

  # Combine all statistics
  tour_stats$total_unique_songs <- unique_songs[tour_stats$tour]
  tour_stats$total_unique_snippets <- unique_snippets[tour_stats$tour]
  tour_stats$overall_variability <- tour_stats$total_unique_songs / tour_stats$avg_songs_per_show

  # Order tours if tour_order is provided
  if (!is.null(tour_order) && "tour" %in% names(tour_order)) {
    tour_stats$tour <- factor(tour_stats$tour, levels = tour_order$tour)
  }

  # Sort by tour
  tour_stats <- tour_stats[order(tour_stats$tour), ]

  # Reset row names
  rownames(tour_stats) <- NULL

  return(tour_stats)
}

#' @title Calculate Song Frequency Across Tours
#' @description Computes the frequency of each song played across different tours and includes additional tour statistics.
#'
#' @param data A data frame in concertData format, containing at least 'tour', 'song_title', and 'showID' columns.
#' @param tour_stats A data frame containing tour statistics, including 'tour', 'total_shows', 'avg_songs_per_show', 'overall_variability', 'total_unique_snippets', and 'total_unique_songs' columns.
#' @param tour_order A data frame with a 'tour' column specifying the desired order of tours (optional).
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item tour: Name of the tour (as a factor)
#'     \item song_title: Title of the song
#'     \item song_count: Number of shows in which the song was played
#'     \item avg_songs_per_show: Average number of songs per show for the tour
#'     \item overall_variability: Overall variability metric for the tour
#'     \item total_shows: Total number of shows in the tour
#'     \item total_unique_snippets: Total number of unique snippets in the tour
#'     \item total_unique_songs: Total number of unique songs in the tour
#'     \item song_frequency: Proportion of shows in which the song was played
#'   }
#'
#' @examples
#' \dontrun{
#' song_freq <- calculate_song_frequency_across_tours(concert_data, tour_statistics, tour_order)
#' }
#'
#' @export
calculate_song_frequency_across_tours <- function(data, tour_stats, tour_order = NULL) {
  # Check if required columns exist
  required_data_cols <- c("tour", "song_title", "showID")
  required_stats_cols <- c("tour", "total_shows", "avg_songs_per_show", "overall_variability", "total_unique_snippets", "total_unique_songs")

  if (!all(required_data_cols %in% names(data))) {
    stop("Input data frame must contain 'tour', 'song_title', and 'showID' columns")
  }
  if (!all(required_stats_cols %in% names(tour_stats))) {
    stop("tour_stats must contain all required columns")
  }

  # Calculate song count for each tour and song
  song_count <- aggregate(showID ~ tour + song_title, data, function(x) length(unique(x)))
  names(song_count)[3] <- "song_count"

  # Merge with tour statistics
  song_frequency <- merge(song_count, tour_stats[, required_stats_cols], by = "tour", all.x = TRUE)

  # Calculate song frequency
  song_frequency$song_frequency <- song_frequency$song_count / song_frequency$total_shows

  # Order tours if tour_order is provided
  if (!is.null(tour_order) && "tour" %in% names(tour_order)) {
    song_frequency$tour <- factor(song_frequency$tour, levels = tour_order$tour)
  } else {
    song_frequency$tour <- factor(song_frequency$tour)
  }

  # Sort by tour and song frequency
  song_frequency <- song_frequency[order(song_frequency$tour, -song_frequency$song_frequency), ]

  # Reorder columns to match original output
  song_frequency <- song_frequency[, c("tour", "song_title", "song_count", "avg_songs_per_show",
                                       "overall_variability", "total_shows", "total_unique_snippets",
                                       "total_unique_songs", "song_frequency")]

  return(song_frequency)
}



#' @title Calculate Tour Correlation Data Number Shows vs Unique Songs and Snippets
#' @description This function transforms tour statistics into a long-format dataset
#'              for correlation analysis between unique songs, unique snippets, and the number of shows.
#'
#' @param tour_stats A data frame containing tour statistics with at least
#'                   'total_unique_songs', 'total_unique_snippets', and 'total_shows' columns.
#'
#' @return A data frame in long format with columns:
#'   \itemize{
#'     \item metric_type: Type of metric ("Unique Songs" or "Unique Snippets").
#'     \item unique_count: Count of unique songs or snippets.
#'     \item total_shows: Number of shows corresponding to each metric type.
#'   }
#'
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr case_when
#'
#' @examples
#' \dontrun{
#' correlation_data <- calculate_tour_correlation_data(tour_stats)
#' }
#'
#' @export
calculate_tour_correlation_data <- function(tour_stats) {
  library(tidyr)  # Load tidyr for pivot_longer
  library(dplyr)  # load dplyr for case_when

  # Check if required columns exist
  required_cols <- c("total_unique_songs", "total_unique_snippets", "total_shows")
  if (!all(required_cols %in% names(tour_stats))) {
    stop("Input data frame must contain 'total_unique_songs', 'total_unique_snippets', and 'total_shows' columns.")
  }

  # Create long-format dataset using pivot_longer
  long_format_data <- pivot_longer(
    data = tour_stats,
    cols = c(total_unique_songs, total_unique_snippets),
    names_to = "metric_type",
    values_to = "unique_count"
  )

  # Mutate metric_type to descriptive names
  long_format_data$metric_type <- case_when(
    long_format_data$metric_type == "total_unique_songs" ~ "Unique Songs",
    long_format_data$metric_type == "total_unique_snippets" ~ "Unique Snippets"
  )

  # Return the modified dataset including total_shows
  return(long_format_data)
}
