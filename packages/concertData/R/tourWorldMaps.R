#' @title Extract Unique Shows from Concert Data
#' @description This function processes concert data to extract unique shows,
#'
#' @param concert_data A concertData frame containing concert data with at least 'showID',
#' 'venue', 'city', 'country', 'date', and 'tour' columns.
#' @return A data frame with unique shows, sorted by date.
#' @importFrom dplyr select distinct arrange n_distinct
#' @examples
#' concert_data <- data.frame(
#'   showID = c(1, 1, 2),
#'   venue = c("Venue A", "Venue A", "Venue B"),
#'   city = c("City X", "City X", "City Y"),
#'   country = c("Country A", "Country A", "Country B"),
#'   date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02")),
#'   tour = c("Tour 1", "Tour 1", "Tour 1"),
#'   song_title = c("Song A", "Song B", "Song C")
#' )
#' unique_shows <- extract_unique_shows(concert_data)
#' @export
extract_unique_shows <- function(concert_data) {
  tryCatch({
    # Select relevant columns and get distinct shows
    shows_df <- dplyr::select(concert_data, showID, venue, city, country, date, tour)
    shows_df <- dplyr::distinct(shows_df)
    shows_df <- dplyr::arrange(shows_df, date)

    # Validation check for unique shows
    expected_shows <- dplyr::n_distinct(concert_data$showID)
    actual_shows <- nrow(shows_df)

    if (actual_shows != expected_shows) {
      warning(sprintf("Warning: Found %d shows but expected %d based on unique showIDs",
                      actual_shows, expected_shows))
    }

    return(shows_df)  # Return the processed data
  }, error = function(e) {
    stop(paste("Error processing unique shows:", e$message))
  })
}



#' @title Get City - Country Locations from Concert Data
#' @description This function extracts city and country data from a data frame with concert info,
#' creating a unique list of locations that can for example be used to look up geographical coordinates
#'
#' @param data A data frame containing concert info with 'city' and 'country' columns.
#' @return A data frame with unique city-country pairs and a combined 'location' column.
#' @importFrom dplyr select distinct mutate filter if_else
#' @examples
#' city_data <- data.frame(
#'   city = c("New York", "London", ""),
#'   country = c("USA", "UK", "France")
#' )
#' unique_locations <- get_concert_locations(unique_shows)
#' @export
get_concert_locations <- function(data) {
  tryCatch({
    # Select relevant columns
    selected_data <- dplyr::select(data, city, country)

    # Get distinct pairs
    distinct_data <- dplyr::distinct(selected_data)

    # Create location and handle empty strings
    processed_data <- dplyr::mutate(distinct_data,
                                    location = paste(city, country, sep = ", "),
                                    city = dplyr::if_else(city == "", NA_character_, city),
                                    country = dplyr::if_else(country == "", NA_character_, country)
    )

    # Filter out NA and empty values
    filtered_data <- dplyr::filter(processed_data,
                                   !is.na(city) & !is.na(country) & city != "" & country != ""
    )

    return(filtered_data)
  }, error = function(e) {
    stop(paste("Error processing cities:", e$message))
  })
}

#' @title Geocode Cities
#' @description This function geocodes unique locations, either by reading from an existing file
#' or by performing geocoding and saving the results to a file.
#'
#' @param geocoded_file_path A string specifying the file path for geocoded cities data.
#' @param unique_locations A data frame containing unique locations to be geocoded.
#' @return A data frame with geocoded city information.
#' @importFrom readr read_csv write_csv
#' @importFrom tidygeocoder geocode
#' @examples
#' geocoded_file_path <- "path/to/geocoded_cities.csv"
#' unique_locations <- data.frame(
#'   location = c("New York, USA", "London, UK", "Paris, France")
#' )
#' geocoded_cities <- geocode_cities(geocoded_file_path, unique_locations)
#' @export
geocode_cities <- function(geocoded_file_path, unique_locations) {
  # Check if file with geocodes exists and read it if it does
  if (file.exists(geocoded_file_path)) {
    geocoded_cities <- tryCatch({
      readr::read_csv(geocoded_file_path)
    }, error = function(e) {
      stop(paste("Error reading file:", e$message))
    })
  } else {
    # Perform geocoding with error handling
    geocoded_cities <- tryCatch({
      result <- tidygeocoder::geocode(
        unique_locations,
        address = location,
        method = 'osm',
        limit = 1,
        min_time = 0.5
      )

      if (is.null(result) || nrow(result) == 0) {
        stop("Geocoding returned no results")
      }

      result
    }, error = function(e) {
      stop(paste("Geocoding error:", e$message))
    })

    # Write results to file
    readr::write_csv(geocoded_cities, geocoded_file_path)
  }

  return(geocoded_cities)
}


#' @title Create City Frequency Counts
#' @description This function creates frequency counts for cities based on unique shows
#' and joins the result with geocoded city data.
#'
#' @param unique_shows A data frame containing unique show information.
#' @param geocoded_cities A data frame containing geocoded city information.
#' @return A data frame with city frequency counts and geocoded information.
#' @importFrom dplyr group_by summarise n_distinct left_join
#' @importFrom tidyr drop_na
#' @examples
#' unique_shows <- data.frame(
#'   showID = 1:3,
#'   city = c("New York", "London", "New York"),
#'   country = c("USA", "UK", "USA"),
#'   date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
#'   venue = c("Madison Square Garden", "Wembley Stadium", "Yankee Stadium"),
#'   tour = c("Tour A", "Tour B", "Tour A")
#' )
#' geocoded_cities <- data.frame(
#'   city = c("New York", "London"),
#'   country = c("USA", "UK"),
#'   lat = c(40.7128, 51.5074),
#'   lon = c(-74.0060, -0.1278)
#' )
#' city_freq <- create_city_frequency(unique_shows, geocoded_cities)
#' @export
create_city_frequency <- function(unique_shows, geocoded_cities) {
  tryCatch({
    # Group by city and country
    grouped_data <- dplyr::group_by(unique_shows, city, country)

    # Summarize the data
    summarized_data <- dplyr::summarise(grouped_data,
                                        show_count = dplyr::n_distinct(showID),
                                        first_show = min(date, na.rm = TRUE),
                                        last_show = max(date, na.rm = TRUE),
                                        venues = list(unique(na.omit(venue))),
                                        tours = list(unique(na.omit(tour))),
                                        .groups = 'drop'
    )

    # Join with geocoded cities
    city_frequency <- dplyr::left_join(summarized_data, geocoded_cities, by = c("city", "country"))

    return(city_frequency)
  }, error = function(e) {
    stop(paste("Error creating frequency counts:", e$message))
  })
}



#' @title Transform City Frequency Data
#' @description This function transforms the city frequency data by adding a log-transformed
#' show count and categorizing cities based on their show counts using custom breaks.
#'
#' @param city_frequency A data frame containing city frequency information, including a 'show_count' column.
#' @param breaks A numeric vector of break points for categorizing show counts.
#' @return A data frame with additional columns for log-transformed show counts and show count categories.
#' @importFrom dplyr mutate
#' @examples
#' city_frequency <- data.frame(
#'   city = c("New York", "London", "Paris", "Berlin"),
#'   country = c("USA", "UK", "France", "Germany"),
#'   show_count = c(50, 25, 10, 1)
#' )
#' breaks <- c(0, 1, 5, 10, 25, 50, 100, Inf)
#' transformed_data <- transform_city_frequency(city_frequency, breaks)
#' @export
transform_city_frequency <- function(city_frequency, breaks) {
  tryCatch({
    # Validate breaks
    if (!is.numeric(breaks) || length(breaks) < 2) {
      stop("breaks must be a numeric vector with at least 2 elements")
    }

    # Generate labels based on breaks
    labels <- character(length(breaks) - 1)
    for (i in 1:(length(breaks) - 1)) {
      if (i == 1 && breaks[i] == 0 && breaks[i+1] == 1) {
        labels[i] <- "1"
      } else if (is.infinite(breaks[i+1])) {
        labels[i] <- paste(breaks[i], "+", sep="")
      } else {
        labels[i] <- paste(breaks[i] + 1, "-", breaks[i+1], sep="")
      }
    }

    city_frequency_transformed <- dplyr::mutate(city_frequency,
                                                log_shows = log(show_count + 1),
                                                show_category = cut(
                                                  show_count,
                                                  breaks = breaks,
                                                  labels = labels,
                                                  include.lowest = TRUE,
                                                  right = TRUE
                                                )
    )

    return(city_frequency_transformed)
  }, error = function(e) {
    stop(paste("Error transforming city frequency data:", e$message))
  })
}


#' Create Decade-Based City Frequency
#'
#' This function creates a decade-based frequency count of shows for each city,
#' and joins this information with geocoded city data.
#'
#' @param unique_shows A data frame containing unique show information, including 'showID', 'city', 'country', and 'date' columns.
#' @param geocoded_cities A data frame containing geocoded information for cities.
#' @return A data frame with decade-based show counts for each city, including geocoded information.
#' @importFrom dplyr mutate filter group_by summarise n_distinct left_join
#' @examples
#' unique_shows <- data.frame(
#'   showID = 1:5,
#'   city = c("New York", "London", "Paris", "New York", "Tokyo"),
#'   country = c("USA", "UK", "France", "USA", "Japan"),
#'   date = as.Date(c("1990-01-01", "2000-01-01", "2010-01-01", "1995-01-01", "2020-01-01"))
#' )
#' geocoded_cities <- data.frame(
#'   city = c("New York", "London", "Paris", "Tokyo"),
#'   country = c("USA", "UK", "France", "Japan"),
#'   lat = c(40.7128, 51.5074, 48.8566, 35.6762),
#'   lon = c(-74.0060, -0.1278, 2.3522, 139.6503)
#' )
#' decade_frequency <- create_decade_frequency(unique_shows, geocoded_cities)
#' @export
create_decade_frequency <- function(unique_shows, geocoded_cities) {
  tryCatch({
    # Ensure date is character and handle potential NA values
    unique_shows$decade <- ifelse(
      !is.na(unique_shows$date),
      paste0(substr(as.character(unique_shows$date), 1, 3), "0s"),
      NA_character_
    )

    # Remove any records with NA decades
    unique_shows <- dplyr::filter(unique_shows, !is.na(decade))

    # Group by city, country, and decade
    grouped_data <- dplyr::group_by(unique_shows, city, country, decade)

    # Summarise to get show counts
    summarized_data <- dplyr::summarise(grouped_data,
                                        show_count = dplyr::n_distinct(showID),
                                        .groups = 'drop'
    )

    # Join with geocoded cities
    city_by_decade <- dplyr::left_join(summarized_data, geocoded_cities, by = c("city", "country"))

    return(city_by_decade)
  }, error = function(e) {
    stop(paste("Error creating decade-based frequency:", e$message))
  })
}
