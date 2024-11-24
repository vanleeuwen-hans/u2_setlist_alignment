#' @title Identify Concerts with the Longest Setlists
#' @description This function identifies concerts that feature the highest number of songs played
#' from a given concert dataset. It aggregates concerts by their date, city, country, and venue
#' to determine which had the most songs performed.
#'
#' @param concert_data A data frame containing concert information with the following columns:
#'        - `song_position`: The position of each song in the setlist.
#'        - `date`: The date of the concert.
#'        - `city`: The city where the concert took place.
#'        - `country`: The country where the concert took place.
#'        - `venue`: The venue of the concert.
#'
#' @return A data frame containing details of concerts with the maximum number of songs played,
#'         including columns for `date`, `city`, `country`, `venue`, and `Number of Songs Played`.
#'
#' @examples
#' \dontrun{
#' data <- read.csv("u2concerts.csv")
#' longest_setlists <- find_longest_setlists(data)
#' print(longest_setlists)
#' }
#'
#' @export
find_longest_setlists <- function(concert_data) {
  # Check if required columns exist
  required_cols <- c("date", "city", "country", "venue", "song_position")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  max_song_position <- max(concert_data$song_position, na.rm = TRUE)

  longest_setlists <- aggregate(
    data = concert_data[concert_data$song_position == max_song_position, ],
    song_position ~ date + city + country + venue,
    FUN = max
  )

  setNames(
    longest_setlists,
    c("date", "city", "country", "venue", "Number of songs played")
  )
}

#' @title Find Shows with Most Snippets
#' @description Identifies concerts with the highest number of song snippets played.
#' A snippet is a short piece of song (either from another artist or from U2 themselves)
#' played as part of a regular song in the show.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        date, city, country, venue, snippet (logical)
#'
#' @return A data frame sorted by number of snippets (descending) and date (descending),
#'         containing columns: date, city, country, venue, and number of snippets played
#'
#' @examples
#' \dontrun{
#' shows_with_snippets <- find_shows_with_most_snippets(concert_data)
#' head(shows_with_snippets)
#' }
#'
#' @importFrom dplyr select group_by summarise arrange desc
#' @export
find_shows_with_most_snippets <- function(concert_data) {
  # Check if required columns exist
  required_cols <- c("date", "city", "country", "venue", "snippet")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Step 1: Select the required columns
  selected_data <- dplyr::select(concert_data, date, city, country, venue, snippet)

  # Step 2: Group the data
  grouped_data <- dplyr::group_by(selected_data, date, city, country, venue)

  # Step 3: Summarize the data
  summarized_data <- dplyr::summarise(grouped_data,
                               number_snippets_played = sum(snippet == TRUE),
                               .groups = 'drop')

  # Step 4: Arrange the results
  final_result <- dplyr::arrange(summarized_data,
                                 dplyr::desc(number_snippets_played),
                                 dplyr::desc(date))

}

#' @title Find Most Frequently Played Cities
#' @description Identifies cities where the most shows were performed, based on unique show IDs.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        showID and city
#' @param n Integer specifying how many top cities to return (default is 15)
#'
#' @return A data frame sorted by number of shows (descending), containing columns:
#'         city and number of shows
#'
#' @examples
#' \dontrun{
#' top_cities <- find_most_played_cities(concert_data)
#' top_cities <- find_most_played_cities(concert_data, n = 10)  # Show only top 10
#' }
#'
#' @importFrom dplyr arrange desc
#' @export
find_most_played_cities <- function(concert_data, n = 15) {
  # Check if required columns exist
  required_cols <- c("showID", "city")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate n is a positive integer
  if (!is.numeric(n) || n < 1 || n != round(n)) {
    stop("'n' must be a positive integer")
  }

  # Calculate shows per city
  cities_summary_temp <- aggregate(
    data = concert_data,
    showID ~ city,
    FUN = function(x) length(unique(x))
  )
  cities_summary <- dplyr::arrange(cities_summary_temp, dplyr::desc(showID))

  # Rename the count column to be more descriptive
  names(cities_summary)[names(cities_summary) == "showID"] <- "number_of_shows"

  # Return top n cities
  head(cities_summary, n = n)
}


#' @title Find Most Frequently Played Venues
#' @description Identifies venues where the most shows were performed, based on unique show IDs.
#' Includes both venue and city information.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        showID, venue, and city
#' @param n Integer specifying how many top venues to return (default is 10)
#'
#' @return A data frame sorted by number of shows (descending), containing columns:
#'         venue, city, and number of shows
#'
#' @examples
#' \dontrun{
#' top_venues <- find_most_played_venues(concert_data)
#' top_venues <- find_most_played_venues(concert_data, n = 15)  # Show top 15
#' }
#'
#' @importFrom dplyr arrange desc
#' @export
find_most_played_venues <- function(concert_data, n = 10) {
  # Check if required columns exist
  required_cols <- c("showID", "venue", "city")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate n is a positive integer
  if (!is.numeric(n) || n < 1 || n != round(n)) {
    stop("'n' must be a positive integer")
  }

  # Calculate shows per venue
  venues_summary_temp <- aggregate(
    data = concert_data,
    showID ~ venue + city,
    FUN = function(x) length(unique(x))
  )
  venues_summary <- dplyr::arrange(venues_summary_temp, dplyr::desc(showID))

  # Rename the count column to be more descriptive
  names(venues_summary)[names(venues_summary) == "showID"] <- "number_of_shows"

  # Return top n venues
  head(venues_summary, n = n)
}



#' @title Find Most Frequently Played Countries
#' @description Identifies countries where the most shows were performed, based on unique show IDs.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        showID and country
#' @param n Integer specifying how many top countries to return (default is 10)
#'
#' @return A data frame sorted by number of shows (descending), containing columns:
#'         country and number of shows
#'
#' @examples
#' \dontrun{
#' top_countries <- find_most_played_countries(concert_data)
#' top_countries <- find_most_played_countries(concert_data, n = 15)  # Show top 15
#' }
#'
#' @importFrom dplyr arrange desc
#' @export
find_most_played_countries <- function(concert_data, n = 10) {
  # Check if required columns exist
  required_cols <- c("showID", "country")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate n is a positive integer
  if (!is.numeric(n) || n < 1 || n != round(n)) {
    stop("'n' must be a positive integer")
  }

  # Calculate shows per country
  countries_summary_temp <- aggregate(
    data = concert_data,
    showID ~ country,
    FUN = function(x) length(unique(x))
  )

  countries_summary <- dplyr::arrange(countries_summary_temp, dplyr::desc(showID))

  # Rename the count column to be more descriptive
  names(countries_summary)[names(countries_summary) == "showID"] <- "number_of_shows"

  # Return top n countries
  head(countries_summary, n = n)
}


#' @title Find Most Played Songs
#'
#' @description This function calculates and returns the most frequently played songs from a dataset,
#' including total plays, regular plays, and snippet plays.
#'
#' @param data A concertData data frame containing song play data.
#' @param n An integer specifying the number of top played songs to return. Default is 10.
#'
#' @return A data frame with four columns: song_title, times_played_total, times_played_regular,
#' times_played_snippet, sorted in descending order by times_played_total.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' top_songs <- find_most_played_songs(concert_data)
#' top_songs <- find_most_played_songs(concert_data, n = 5)
#' }
#'
#' @importFrom dplyr count arrange desc left_join
#' @importFrom utils head
find_most_played_songs <- function(concert_data, n = 10) {
  # Check if required columns exist
  required_cols <- c("song_title", "snippet")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate n is a positive integer
  if (!is.numeric(n) || n < 1 || n != round(n)) {
    stop("'n' must be a positive integer")
  }

  # Calculate play counts
  total_plays <- dplyr::count(concert_data, song_title, name = "times_played_total")

  filtered_regular <- dplyr::filter(concert_data, snippet == FALSE)
  regular_plays <- dplyr::count(filtered_regular, song_title, name = "times_played_regular")

  filtered_snippet <- dplyr::filter(concert_data, snippet == TRUE)
  snippet_plays <- dplyr::count(filtered_snippet, song_title, name = "times_played_snippet")

  # Combine all play counts
  result_temp1 <- dplyr::left_join(total_plays, regular_plays, by = "song_title")
  result_temp2 <- dplyr::left_join(result_temp1, snippet_plays, by = "song_title")
  result_temp3 <- dplyr::arrange(result_temp2, dplyr::desc(times_played_total))
  result <- head(result_temp3, n)

  # Replace NA values with 0
  result[is.na(result)] <- 0

  return(result)
}


#' @title Find Most Played Opening Songs
#' @description Identifies songs that were most frequently played as opening songs in concerts,
#' excluding appearances as snippet as opening.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        date, city, song_title, song_position, and snippet
#' @param n Integer specifying how many top opening songs to return (default is all)
#'
#' @return A data frame sorted by frequency (descending), containing columns:
#'         song_title and times_played (number of times played as opening song)
#'
#' @examples
#' \dontrun{
#' top_openers <- find_most_played_opening_songs(concert_data)
#' top_openers <- find_most_played_opening_songs(concert_data, n = 10)  # Show top 10
#' }
#'
#' @importFrom dplyr count arrange desc
#' @export
find_most_played_opening_songs <- function(concert_data, n = NULL) {
  # Check if required columns exist
  required_cols <- c("date", "city", "song_title", "song_position", "snippet")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate n if provided
  if (!is.null(n)) {
    if (!is.numeric(n) || n < 1 || n != round(n)) {
      stop("'n' must be a positive integer")
    }
  }

  # Find opening songs
  subset_data <- subset(concert_data,
                        song_position == 1 & snippet == FALSE,
                        select = c("date", "city", "song_title"))

  opening_songs <- dplyr::count(subset_data,
                         song_title,
                         sort = TRUE,
                         name = "times_played")

  # Return results
  if (is.null(n)) {
    return(opening_songs)
  } else {
    return(head(opening_songs, n = n))
  }
}


#' @title Find Most Played Closing Songs
#' @description Identifies the most frequently played closing songs in concerts.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        showID, song_title, song_position, and snippet
#' @param n Integer specifying how many top closing songs to return (default is 10)
#'
#' @return A data frame sorted by frequency (descending), containing columns:
#'         song_title and times_played (number of times played as closing song)
#'
#' @examples
#' \dontrun{
#' top_closers <- find_most_played_closing_songs(concert_data)
#' top_closers <- find_most_played_closing_songs(concert_data, n = 15)  # Show top 15
#' }
#'
#' @export
find_most_played_closing_songs <- function(concert_data, n = 10) {
  # Check if required columns exist
  required_cols <- c("showID", "song_title", "song_position", "snippet")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate n is a positive integer
  if (!is.numeric(n) || n < 1 || n != round(n)) {
    stop("'n' must be a positive integer")
  }

  # Find the last song for each show
  closing_songs <- vector("list", length(unique(concert_data$showID)))
  for (i in seq_along(closing_songs)) {
    show_data <- concert_data[concert_data$showID == unique(concert_data$showID)[i], ]
    max_position <- max(show_data$song_position, na.rm = TRUE)
    last_song <- show_data[show_data$song_position == max_position & !show_data$snippet, ]
    closing_songs[[i]] <- last_song
  }

  # Combine all closing songs
  closing_songs <- do.call(rbind, closing_songs)

  # Count occurrences of each song
  song_counts <- table(closing_songs$song_title)

  # Convert to data frame and sort
  closing_songs_df <- data.frame(
    song_title = names(song_counts),
    times_played = as.vector(song_counts)
  )
  closing_songs_df <- closing_songs_df[order(closing_songs_df$times_played, decreasing = TRUE), ]

  # Select top n songs
  top_closing_songs <- head(closing_songs_df, n)

  # Reset row names
  rownames(top_closing_songs) <- NULL

  return(top_closing_songs)
}

#' @title Find Most Played Opening Songs by Tour
#' @description This function identifies the most frequently played opening songs for each tour,
#'              returning a specified number of top songs based on their play counts.
#'
#' @param concert_data A data frame containing concert information with the following columns:
#'        - `tour`: The name of the tour.
#'        - `date`: The date of the concert.
#'        - `song_title`: The title of the song performed.
#'        - `song_position`: The position of the song in the setlist.
#'        - `snippet`: Logical indicating whether the song was played as a snippet.
#' @param top_n An integer specifying the maximum number of top opening songs to return per tour
#'               (default is 3).
#'
#' @return A data frame sorted by tour start date (descending), containing columns:
#'         - `tour`: The name of the tour.
#'         - `song_title`: The title of the most played opening song.
#'         - `times_played`: The number of times the song was played as an opening song.
#'         For each tour, it returns up to `top_n` most played opening songs, or fewer if
#'         the tour has less unique opening songs.
#'
#' @examples
#' \dontrun{
#' top_openers_by_tour <- find_most_played_opening_songs_by_tour(concert_data)
#' top_5_openers_by_tour <- find_most_played_opening_songs_by_tour(concert_data, top_n = 5)
#' print(top_openers_by_tour)
#' }
#'
#' @export
find_most_played_opening_songs_by_tour <- function(concert_data, top_n = 3) {
  # Check if required columns exist
  required_cols <- c("tour", "date", "song_title", "song_position", "snippet")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate top_n is a positive integer
  if (!is.numeric(top_n) || top_n < 1 || top_n != round(top_n)) {
    stop("'top_n' must be a positive integer")
  }

  # Get the earliest date for each tour
  tour_start_dates <- tapply(concert_data$date, concert_data$tour, min)

  # Filter for opening songs
  opening_songs <- concert_data[concert_data$song_position == 1 & concert_data$snippet == FALSE, ]

  # Count occurrences of each song by tour
  song_counts <- table(opening_songs$tour, opening_songs$song_title)

  # Convert to data frame
  song_counts_df <- as.data.frame.table(song_counts)
  names(song_counts_df) <- c("tour", "song_title", "times_played")

  # Remove rows where times_played is 0
  song_counts_df <- song_counts_df[song_counts_df$times_played > 0, ]

  # Add tour start date
  song_counts_df$tour_start_date <- tour_start_dates[song_counts_df$tour]

  # Function to get top songs for a tour
  get_top_n <- function(tour_data, n) {
    tour_data <- tour_data[order(tour_data$times_played, decreasing = TRUE), ]
    head(tour_data, min(n, nrow(tour_data)))
  }

  # Apply get_top_n to each tour
  top_songs_list <- by(song_counts_df, song_counts_df$tour, get_top_n, n = top_n)

  # Combine results
  top_songs <- do.call(rbind, top_songs_list)

  # Sort by tour start date
  top_songs <- top_songs[order(top_songs$tour_start_date, decreasing = TRUE), ]

  # Select and reorder columns
  result <- top_songs[, c("tour", "song_title", "times_played")]

  # Reset row names
  rownames(result) <- NULL

  return(result)
}


#' @title Find Most Played Opening Songs by Year
#' @description Identifies the most frequently played opening song for each year.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        date, song_title, song_position, and snippet
#'
#' @return A data frame sorted by year (ascending), containing columns:
#'         year, song_title, and times_played (number of times the song was played as opener that year)
#'
#' @examples
#' \dontrun{
#' opening_timeline <- find_most_played_opening_song_by_year(concert_data)
#' print(opening_timeline, n = nrow(opening_timeline))
#' }
#'
#' @importFrom lubridate year
#' @export
find_most_played_opening_song_by_year <- function(concert_data) {
  # Check if required columns exist
  required_cols <- c("date", "song_title", "song_position", "snippet")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Filter for opening songs
  opening_songs <- concert_data[concert_data$song_position == 1 & concert_data$snippet == FALSE, ]

  # Extract year from date
  opening_songs$year <- lubridate::year(opening_songs$date)

  # Count occurrences of each song by year
  song_counts <- table(opening_songs$year, opening_songs$song_title)

  # Convert to data frame
  song_counts_df <- as.data.frame.table(song_counts)
  names(song_counts_df) <- c("year", "song_title", "times_played")

  # Convert year to numeric
  song_counts_df$year <- as.numeric(as.character(song_counts_df$year))

  # Function to get the most played song(s) for a year
  get_max_songs <- function(year_data) {
    max_count <- max(year_data$times_played)
    year_data[year_data$times_played == max_count, ]
  }

  # Apply get_max_songs to each year
  max_songs_list <- by(song_counts_df, song_counts_df$year, get_max_songs)

  # Combine results
  opening_timeline <- do.call(rbind, max_songs_list)

  # Sort by year
  opening_timeline <- opening_timeline[order(opening_timeline$year), ]

  # Reset row names
  rownames(opening_timeline) <- NULL

  return(opening_timeline)
}


#' @title Count Shows by Weekday
#' @description Calculates the number of shows performed on each day of the week.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        showID (or a unique identifier for each show) and date (in Date format)
#'
#' @return A data frame with columns:
#'         weekday (factor with levels ordered from Monday to Sunday) and
#'         count (number of shows on that weekday)
#'
#' @examples
#' \dontrun{
#' weekday_counts <- count_shows_by_weekday(concert_data)
#' print(weekday_counts)
#' }
#'
#' @importFrom dplyr distinct
#' @export
count_shows_by_weekday <- function(concert_data) {
  # Check if required columns exist
  required_cols <- c("showID", "date")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if date is in Date format
  if (!inherits(concert_data$date, "Date")) {
    stop("The 'date' column must be in Date format")
  }

  # Get subset so that we have one row per show
  concert_data_shows <- dplyr::distinct(concert_data, showID, .keep_all = TRUE)

  # Define weekdays
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  # Get counts per weekday
  weekday_counts <- sapply(weekdays, function(day) sum(weekdays(concert_data_shows$date) == day))

  # Prepare data frame for output
  weekdays_count <- data.frame(
    weekday = factor(weekdays, levels = weekdays),
    count = weekday_counts
  )

  return(weekdays_count)
}



#' @title Count Shows by Month
#' @description Calculates the number of shows performed in each month.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        showID (or a unique identifier for each show) and date (in Date format)
#'
#' @return A data frame with columns:
#'         month (factor with levels ordered from Jan to Dec) and
#'         count (number of shows in that month)
#'
#' @examples
#' \dontrun{
#' month_counts <- count_shows_by_month(concert_data)
#' print(month_counts)
#' }
#'
#' @importFrom dplyr distinct
#' @export
count_shows_by_month <- function(concert_data) {
  # Check if required columns exist
  required_cols <- c("showID", "date")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if date is in Date format
  if (!inherits(concert_data$date, "Date")) {
    stop("The 'date' column must be in Date format")
  }

  # Get subset so that we have one row per show
  concert_data_shows <- dplyr::distinct(concert_data, showID, .keep_all = TRUE)

  # Define months
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  # Get counts per month
  month_counts <- sapply(1:12, function(m) sum(format(concert_data_shows$date, "%m") == sprintf("%02d", m)))

  # Prepare data frame for output
  months_count <- data.frame(
    month = factor(months, levels = months),
    count = month_counts
  )

  return(months_count)
}


#' @title Count Shows by Decade
#' @description Calculates the number of shows performed in each decade based on the input data.
#'
#' @param concert_data A data frame containing concert information with columns:
#'        showID (or a unique identifier for each show) and date (in Date format)
#'
#' @return A data frame with columns:
#'         decade (factor with levels ordered chronologically) and
#'         count (number of shows in that decade)
#'
#' @examples
#' \dontrun{
#' decade_counts <- count_shows_by_decade(concert_data)
#' print(decade_counts)
#' }
#'
#' @importFrom dplyr distinct
#' @export
count_shows_by_decade <- function(concert_data) {
  # Check if required columns exist
  required_cols <- c("showID", "date")
  missing_cols <- setdiff(required_cols, names(concert_data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check if date is in Date format
  if (!inherits(concert_data$date, "Date")) {
    stop("The 'date' column must be in Date format")
  }

  # Get subset so that we have one row per show
  u2data_shows <- dplyr::distinct(concert_data, showID, .keep_all = TRUE)

  # Extract years from dates
  years <- as.integer(format(u2data_shows$date, "%Y"))

  # Determine the range of decades present in the data
  min_decade <- floor(min(years) / 10) * 10
  max_decade <- floor(max(years) / 10) * 10

  # Create a sequence of decades
  decades <- seq(min_decade, max_decade, by = 10)

  # Function to create decade label
  decade_label <- function(d) {
    paste0(substr(as.character(d), 3, 3), "0s")
  }

  # Count shows for each decade
  decade_counts <- sapply(decades, function(d) {
    sum(years >= d & years < (d + 10))
  })

  # Prepare data frame for output
  decades_count <- data.frame(
    decade = factor(sapply(decades, decade_label), levels = sapply(decades, decade_label)),
    count = decade_counts
  )

  return(decades_count)
}
