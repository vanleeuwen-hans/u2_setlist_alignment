#' @title Read concertData CSV File
#' @description Reads a CSV file in the concertData format with error handling, providing informative error messages if the file cannot be read or does not conform to the expected format.
#'
#' @param file_path A string specifying the path to the CSV file to be read.
#'
#' @return A data frame containing the concert data from the CSV file.
#'
#' @details The input CSV file must conform to the concertData format, which includes the following required columns:
#'   \itemize{
#'     \item showID: Unique identifier for each show
#'     \item date: Date of the concert (YYYY-MM-DD format)
#'     \item tour: Name of the tour
#'     \item city: City where the concert took place
#'     \item country: Country where the concert took place
#'     \item venue: Name of the venue
#'     \item song_position: Position of the song in the setlist
#'     \item song_title: Title of the song
#'     \item snippet: Boolean indicating if the song was a snippet (TRUE) or full performance (FALSE)
#'   }
#'   Additional columns that may be present include:
#'   \itemize{
#'     \item encore: Boolean indicating if a song was an encore performance (TRUE) or in the main set (FALSE)
#'     \item state: State where the concert took place
#'     \item show_url: URL for more information about the show
#'     \item song_url: URL for more information about the song
#'     \item song_lyrics: Lyrics of the song performed
#'   }
#'
#' @examples
#' \dontrun{
#' concert_data <- read_concertData_csv("path/to/your/concertdata.csv")
#' }
#'
#' @importFrom readr read_csv
#' @export
read_concertData_csv <- function(file_path) {
  # Check if file_path is provided and is a string
  if (missing(file_path) || !is.character(file_path)) {
    stop("file_path must be provided as a string")
  }

  # Attempt to read the CSV file
  tryCatch({
    data <- readr::read_csv(file_path, show_col_types = FALSE)

    # Check for required columns
    required_cols <- c("showID", "date", "tour", "city", "country", "venue", "song_position", "song_title", "snippet")
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
      stop(paste("The following required columns are missing from the CSV file:",
                 paste(missing_cols, collapse = ", ")))
    }

    # Additional checks could be added here (e.g., date format, data types)

    return(data)
  }, error = function(e) {
    stop(paste("Error reading concertData CSV file:", e$message))
  })
}


#' @title Remove Specified Tours from concertData
#' @description Filters out specified tours from a concertData data frame.
#'
#' @param data A data frame in concertData format.
#' @param excluded_tours A character vector of tour names to be excluded.
#'
#' @return A filtered concertData data frame with specified tours removed.
#'
#' @examples
#' \dontrun{
#' filtered_data <- concertData_remove_tours(concert_data, c("Tour1", "Tour2"))
#' }
#'
#' @export
concertData_remove_tours <- function(data, excluded_tours) {
  # Check if required columns exist
  if (!all(c("tour") %in% names(data))) {
    stop("Input data frame must contain 'tour' column")
  }

  # Filter out excluded tours
  filtered_data <- data[!data$tour %in% excluded_tours, ]

  # Reset factor levels
  filtered_data <- droplevels(filtered_data)

  return(filtered_data)
}

#' @title Remove Shows with No Setlist from concertData
#' @description Filters out shows with no setlist (no songs) from a concertData data frame.
#'
#' @param data A data frame in concertData format.
#'
#' @return A filtered concertData data frame with shows having no setlist removed.
#'
#' @examples
#' \dontrun{
#' filtered_data <- concertData_remove_showsNoSetlist(concert_data)
#' }
#'
#' @export
concertData_remove_showsNoSetlist <- function(data) {
  # Check if required columns exist
  if (!all(c("showID", "song_position") %in% names(data))) {
    stop("Input data frame must contain 'showID' and 'song_position' columns")
  }

  # Filter out shows with no setlist
  filtered_data <- data[data$song_position > 0, ]

  # Remove shows with no songs
  show_counts <- table(filtered_data$showID)
  shows_with_songs <- names(show_counts[show_counts > 0])
  filtered_data <- filtered_data[filtered_data$showID %in% shows_with_songs, ]

  # Reset factor levels
  filtered_data <- droplevels(filtered_data)

  return(filtered_data)
}

#' @title Remove Snippets from concertData
#' @description Filters out snippet performances from a concertData data frame.
#'
#' @param data A data frame in concertData format.
#'
#' @return A filtered concertData data frame with snippet performances removed.
#'
#' @examples
#' \dontrun{
#' filtered_data <- concertData_remove_snippets(concert_data)
#' }
#'
#' @export
concertData_remove_snippets <- function(data) {
  # Check if required columns exist
  if (!all(c("snippet") %in% names(data))) {
    stop("Input data frame must contain 'snippet' column")
  }

  # Filter out snippets
  filtered_data <- data[!data$snippet, ]

  # Reset factor levels
  filtered_data <- droplevels(filtered_data)

  return(filtered_data)
}


#' @title Order Tours by Date
#' @description This function orders tours based on their median date, allowing for either
#'              ascending or descending chronological order.
#'
#' @param data A data frame in concertData format, containing at least the following columns:
#'        - `tour`: The name of the tour.
#'        - `date`: The date of each concert in the tour.
#' @param ascending A logical value. If TRUE, tours are ordered from earliest to latest;
#'                  if FALSE (default), they are ordered from latest to earliest.
#'
#' @return A data frame with the following columns:
#'   - `tour`: A factor of tour names, ordered chronologically.
#'   - `median_date`: The median date of each tour.
#'
#' @examples
#' \dontrun{
#' tour_order <- tour_order_by_date(concert_data)
#' tour_order_asc <- tour_order_by_date(concert_data, ascending = TRUE)
#' print(tour_order)
#' }
#'
#' @export
tour_order_by_date <- function(data, ascending = FALSE) {
  # Check if required columns exist
  if (!all(c("tour", "date") %in% names(data))) {
    stop("Input data frame must contain 'tour' and 'date' columns")
  }

  # Ensure date is in Date format
  if (!inherits(data$date, "Date")) {
    data$date <- as.Date(data$date)
  }

  # Calculate median date for each tour
  tour_dates <- aggregate(date ~ tour, data, FUN = function(x) as.Date(median(x)))
  names(tour_dates)[2] <- "median_date"

  # Sort tours based on median date
  if (ascending) {
    tour_dates <- tour_dates[order(tour_dates$median_date), ]
  } else {
    tour_dates <- tour_dates[order(tour_dates$median_date, decreasing = TRUE), ]
  }

  # Create ordered factor for tour names
  tour_dates$tour <- factor(tour_dates$tour, levels = tour_dates$tour)

  return(tour_dates)
}
