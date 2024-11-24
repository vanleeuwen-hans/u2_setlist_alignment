#' @title Generate a Unique 4-Letter Code from a Song Title
#'
#' @description This function creates a unique 4-letter code for each song title.
#' The generated code is intended for use in displaying song titles in multiple setlist alignment plots,
#' ensuring that each song can be uniquely identified with a concise code.
#'
#' @param title A character string representing the song title.
#' @param used_codes A character vector of already used codes to ensure uniqueness.
#'
#' @return A character string of length 4 representing the unique code for the song.
#'
#' @importFrom stringr str_sub
#'
#' @examples
#' \dontrun{
#' code <- create_four_letter_code("With or Without You", c())
#' print(code)  # Output: "WOWY"
#' }
#'
#' @export
create_four_letter_code <- function(title, used_codes) {
  # Remove special characters at the beginning of words
  clean_title <- gsub("[^A-Za-z0-9 ]", "", title)

  # Split title into words
  words <- strsplit(clean_title, "\\s+")[[1]]

  # Generate initial 4-letter code
  if (length(words) == 1) {
    # Take first 4 letters of the title for titles with one word
    code <- substr(words[1], 1, 4)
  } else if (length(words) == 2) {
    # Take first letter of the first word
    # and the first three letters of the second word
    # for titles with 2 words
    code <- paste0(substr(words[1], 1, 1), substr(words[2], 1, 3))
  } else if (length(words) == 3){
    # Take the first letter of the first word,
    # the first letter of the second word,
    # and the first two letters of the third word,
    # for titles with 3 words
    code <- paste0(
      substr(words[1], 1, 1),
      substr(words[2], 1, 1),
      substr(words[3], 1, 2)
    )
  } else {
    # Take first letter of first 4 words
    # for titles with 4 or more words
    code <- paste0(
      substr(words[1], 1, 1),
      substr(words[2], 1, 1),
      substr(words[3], 1, 1),
      substr(words[4], 1, 1)
    )
  }

  # Make sure code is exactly 4 characters, pad with X if needed
  code <- substr(paste0(code, "XXXX"), 1, 4)

  # Check if code is already used, if so, append a number
  unique_code <- code
  i <- 1
  while (!is.null(used_codes) && unique_code %in% used_codes) {
    unique_code <- paste0(substr(code, 1, 3), i)
    i <- i + 1
  }

  # Ensure uppercase output
  return(toupper(unique_code))
}




#' @title Convert a 4-Letter Code to a Unique Hex Character
#'
#' @description This function converts a 4-letter song code into a unique hex character.
#' It checks against a list of already used hex codes to ensure that the generated hex character
#' is unique. The function returns both the hex character and its numeric value.
#'
#' @param code A character string of length 4 representing the song code.
#' @param used_hex_codes A character vector of already used hex codes to ensure uniqueness.
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item char: A single character representing the unique hex character.
#'     \item value: The numeric value of the hex character.
#'   }
#'
#' @importFrom stringr str_sub
#'
#' @examples
#' \dontrun{
#' hex_info <- convert_code_to_hex("WOWY", c("0x41", "0x42"))
#' print(hex_info)  # Output: List with char and value
#' }
#'
#' @export
four_letter_code_to_hex <- function(code, used_hex_codes) {
  # Ensure code is a single string
  if (length(code) != 1) {
    warning("Multiple codes provided; using only the first one")
    code <- code[1]
  }

  # Define excluded hex values
  excluded_hex <- c(
    0x00:0x1F,  # Control characters
    0x3E, 0x3D, 0x3C, 0x2D, 0x20, 0x0D, 0x0A,  # MAFFT required exclusions
    0x7F  # DEL character
  )

  # Define valid hex values
  valid_hex <- setdiff(0x20:0x7E, excluded_hex)

  # Convert to character vector and get ASCII values
  code_chars <- strsplit(code, "")[[1]]
  ascii_values <- sapply(code_chars, function(x) utf8ToInt(x))
  ascii_sum <- sum(ascii_values) %% length(valid_hex)

  # Select initial hex value from valid set
  hex_value <- valid_hex[ascii_sum + 1]

  # Ensure uniqueness
  while (intToUtf8(hex_value) %in% used_hex_codes) {
    index <- which(valid_hex == hex_value)
    hex_value <- valid_hex[(index %% length(valid_hex)) + 1]
  }

  # Convert to hex character
  return(list(
    char = intToUtf8(hex_value),
    value = hex_value
  ))
}



#' @title Codify Tour Song Titles
#'
#' @description This function takes a concertData data frame, and for all
#' unique songs in the data set creates a mapping
#' with 4 columns: song title, 4-letter code, hex character, and hex value.
#'
#' @param concert_data A data frame in concertData format.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item song_title: The title of the song.
#'     \item four_letter_code: A unique 4-letter code for the song.
#'     \item hex_char: The unique hex character representation of the 4-letter code.
#'     \item hex_value: The unique hex value of the character.
#'   }
#'   Returns NULL if no songs are found in the dataset.
#'
#' @importFrom stringr str_trim
#' @importFrom dplyr distinct arrange
#'
#' @examples
#' \dontrun{
#' codified_songs <- codify_tour_song_titles(concert_data)
#' print(codified_songs)
#' }
#'
#' @export
codify_tour_song_titles <- function(concert_data) {
  # Check if required columns exist
  required_cols <- c("tour", "song_title", "song_position", "snippet")
  if (!all(required_cols %in% names(concert_data))) {
    stop("Input data frame must contain 'tour', 'song_title', 'song_position', and 'snippet' columns")
  }

  # Get unique songs and sort alphabetically
  unique_songs <- unique(concert_data$song_title)

  # If no songs found, return NULL with a warning
  if (length(unique_songs) == 0) {
    warning("No songs found in this data set")
    return(NULL)
  }

  # Sort unique songs
  sorted_unique_songs <- sort(unique_songs)

  # Create a data frame with sorted unique songs
  result <- data.frame(song_title = sorted_unique_songs, stringsAsFactors = FALSE)

  # Create empty vector to store used codes
  used_codes <- character(0)
  used_hex_codes <- character(0)

  # Generate unique 4-letter codes for all unique songs
  four_letter_codes <- character(length(sorted_unique_songs))
  hex_results <- vector("list", length(sorted_unique_songs))

  for(i in seq_along(sorted_unique_songs)) {
    four_letter_codes[i] <- create_four_letter_code(sorted_unique_songs[i], used_codes)
    used_codes <- c(used_codes, four_letter_codes[i])

    hex_results[[i]] <- four_letter_code_to_hex(four_letter_codes[i], used_hex_codes)
    used_hex_codes <- c(used_hex_codes, hex_results[[i]]$char)
  }

  result$four_letter_code <- four_letter_codes
  result$hex_char <- sapply(hex_results, function(x) x$char)
  result$hex_value <- sprintf("0x%02X", sapply(hex_results, function(x) x$value))

  return(result)
}



#' @title Prepare Setlist Sequences from Concert Data
#'
#' @description This function prepares setlist sequences from concert data and song codes.
#' It creates a data frame that associates show IDs with their corresponding encoded setlist sequences,
#' ensuring that each song title is mapped to its unique hexadecimal code.
#'
#' @param concert_data A data frame containing concert information with at least the following columns:
#'   \itemize{
#'     \item showID: Numeric identifier for each show.
#'     \item song_position: Numeric position of each song in the setlist.
#'     \item song_title: Character string of the song title.
#'   }
#' @param codes_tour_songs A data frame containing song codes with at least the following columns:
#'   \itemize{
#'     \item song_title: Character string of the song title.
#'     \item hex_value: Character string of the hexadecimal code for each song.
#'   }
#'
#' @return A data frame with two columns:
#'   \itemize{
#'     \item showID: Character vector of show identifiers.
#'     \item sequence: Character vector of encoded setlist sequences.
#'   }
#'
#' @importFrom base order
#' @importFrom base tapply
#' @importFrom base unname
#'
#' @examples
#' \dontrun{
#' data(concert_data)
#' data(codes_tour_songs)
#' setlist_sequences <- prepare_setlist_sequences(concert_data, codes_tour_songs)
#' head(setlist_sequences)
#' }
#'
#' @export
prepare_setlist_sequences <- function(concert_data, codes_tour_songs) {
  # Check for required columns
  required_cols_concert <- c("showID", "song_position", "song_title")
  required_cols_codes <- c("song_title", "hex_value")

  if (!all(required_cols_concert %in% names(concert_data))) {
    stop("concert_data must contain columns: ", paste(required_cols_concert, collapse = ", "))
  }
  if (!all(required_cols_codes %in% names(codes_tour_songs))) {
    stop("codes_tour_songs must contain columns: ", paste(required_cols_codes, collapse = ", "))
  }

  # Order by showID and song position
  setlist_data <- concert_data[order(concert_data$showID, concert_data$song_position), ]

  # Create lookup dictionary for song codes
  codes_tour_songs$hex_clean <- gsub("0x", "", codes_tour_songs$hex_value)
  song_codes <- setNames(codes_tour_songs$hex_clean, codes_tour_songs$song_title)

  # Group by show and create the hex values to be printed in the output file
  show_setlist_sequences_all <- tapply(setlist_data$song_title, setlist_data$showID,
                                       function(x) paste(song_codes[x], collapse = " "))
  show_setlist_sequences_all <- data.frame(
    showID = names(show_setlist_sequences_all),
    sequence = unname(show_setlist_sequences_all),
    stringsAsFactors = FALSE
  )

  return(show_setlist_sequences_all)
}




#' Find Most Representative Setlists
#'
#' This function identifies a specified number of shows that have the highest average
#' similarity to other shows' setlists, effectively finding the most representative
#' setlists in the dataset.
#'
#' @param show_sequences_all A data frame containing at least two columns:
#'   \itemize{
#'     \item sequence: A character vector of setlist sequences.
#'     \item Other columns containing show information.
#'   }
#' @param n_representatives An integer specifying the number of representative setlists to find. Default is 30.
#'
#' @return A data frame containing the most representative setlists,
#'         with the same structure as the input data frame.
#'
#' @importFrom stringdist stringdistmatrix
#'
#' @examples
#' \dontrun{
#' # Assuming show_sequences_all is your dataset
#' representative_shows <- find_most_representative_setlists(show_sequences_all, n_representatives = 30)
#' print(representative_shows)
#' }
#'
#' @export
find_most_representative_setlists <- function(show_sequences_all, n_representatives = 30) {
  # Check if required package is available
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package 'stringdist' is required but not installed. Please install it.")
  }

  # Calculate distance matrix using Levenshtein distance
  dist_matrix <- stringdist::stringdistmatrix(show_sequences_all$sequence,
                                              show_sequences_all$sequence,
                                              method = "lv")

  # Calculate average distance for each setlist
  avg_distances <- rowMeans(dist_matrix)

  # Find indices of n setlists with lowest average distance
  representative_indices <- order(avg_distances)[1:n_representatives]

  # Return the most representative setlists
  return(show_sequences_all[representative_indices, ])
}



#' Find Most Representative Setlists using K-means Clustering
#'
#' This function identifies a specified number of shows that represent the variety
#' of setlists in the dataset, using k-means clustering.
#'
#' @param show_sequences_all A data frame containing at least two columns:
#'   \itemize{
#'     \item sequence: A character vector of setlist sequences.
#'     \item Other columns containing show information.
#'   }
#' @param n_representatives An integer specifying the number of representative setlists to find. Default is 30.
#'
#' @return A data frame containing the most representative setlists,
#'         with the same structure as the input data frame.
#'
#' @importFrom stringdist stringdistmatrix
#' @importFrom stats kmeans
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' # Assuming show_sequences_all is your dataset
#' representative_shows <- find_representative_setlists_kmeans(show_sequences_all, n_representatives = 30)
#' print(representative_shows)
#' }
#'
#' @export
find_representative_setlists_kmeans <- function(show_sequences_all, n_representatives = 30) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package 'stringdist' is required but not installed. Please install it.")
  }

  dist_matrix <- stringdist::stringdistmatrix(show_sequences_all$sequence,
                                              show_sequences_all$sequence,
                                              method = "lv")

  similarity_matrix <- 1 / (1 + dist_matrix)

  set.seed(123)
  kmeans_result <- kmeans(similarity_matrix, centers = n_representatives)

  representative_indices <- sapply(1:n_representatives, function(i) {
    cluster_members <- which(kmeans_result$cluster == i)
    if (length(cluster_members) == 1) {
      return(cluster_members)
    }
    cluster_center <- kmeans_result$centers[i, cluster_members]
    closest_to_center <- which.max(colSums(similarity_matrix[cluster_members, cluster_members, drop = FALSE] * cluster_center))
    return(cluster_members[closest_to_center])
  })

  return(show_sequences_all[representative_indices, ])
}


#' @title Create FASTA Format for MAFFT Alignment
#'
#' @description This function creates a FASTA format output from representative setlists
#' for use in MAFFT multiple sequence alignment.
#'
#' @param representative_setlists A data frame containing two columns:
#'   \itemize{
#'     \item showID: A character vector of show identifiers
#'     \item sequence: A character vector of encoded setlist sequences
#'   }
#'
#' @return A character vector containing the FASTA format output, where each entry
#' consists of a header line (>showID) followed by the corresponding sequence.
#'
#' @examples
#' \dontrun{
#' representative_setlists <- data.frame(
#'   showID = c("1408", "1320"),
#'   sequence = c("35 66 44 55 65 45 34 4F 59 48 5D 43 4A 57 68 51 6C 61 6D 42 6B 27",
#'                "35 66 44 5F 62 45 34 4F 59 48 5D 43 4A 57 68 51 60 69 6D 42 6B 27")
#' )
#' fasta_output <- create_setlist_fasta_mafft(representative_setlists)
#' print(fasta_output)
#' }
#'
#' @export
create_setlist_fasta_mafft <- function(representative_setlists) {
  # Create FASTA format output
  fasta_output <- character()
  for(i in seq_len(nrow(representative_setlists))) {
    fasta_output <- c(
      fasta_output,
      sprintf(">showID%s", representative_setlists$showID[i]),
      representative_setlists$sequence[i]
    )
  }

  return(fasta_output)
}


#' Parse MAFFT Clustal Alignment File
#'
#' @title Parse MAFFT Clustal Alignment File
#' @description This function reads and parses a multiple sequence alignment file
#' produced by the MAFFT --text algorithm. It extracts showID and sequence
#' information from the file and returns it as a data frame.
#'
#' @param file_path A character string specifying the path to the MAFFT Clustal
#' alignment file.
#'
#' @return A data frame with two columns:
#'   \itemize{
#'     \item showID: An integer vector of show identifiers.
#'     \item sequence: A character vector of aligned sequences.
#'   }
#'
#' @importFrom utils read.table
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' alignment_data <- read_mafft_clustal_alignment("path/to/mafft_alignment.txt")
#' head(alignment_data)
#' }
#'
#' @export
read_mafft_clustal_alignment <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Read the file lines
  lines <- readLines(file_path)

  # Initialize empty vectors for showID and sequence
  showIDs <- integer()
  sequences <- character()

  # Process each line
  for (line in lines) {
    # Check if the line starts with "showID"
    if (grepl("^showID", line)) {
      parts <- strsplit(trimws(line), "\\s+")[[1]]
      showIDs <- c(showIDs, as.numeric(sub("showID", "", parts[1])))
      sequences <- c(sequences, parts[2])
    }
  }

  # Create data frame
  result <- data.frame(showID = showIDs, sequence = sequences, stringsAsFactors = FALSE)

  return(result)
}



#' Create Visualization Data for Setlist Alignment
#'
#' @title Create Visualization Data for Setlist Alignment
#' @description This function processes alignment data and creates a detailed dataset
#' for visualizing setlist alignments. It expands sequences into individual positions,
#' matches song codes to titles, incorporates date information, and includes city names.
#'
#' @param alignment_data A data frame containing alignment data with columns:
#'   showID and sequence.
#' @param song_code_lookup A data frame with song code information, including columns:
#'   hex_char, song_title, and four_letter_code.
#' @param concert_data A data frame containing show information, including columns:
#'   showID, date, and city.
#'
#' @return A data frame with columns:
#'   date, position, song_title, four_letter_code, is_gap, city
#'
#' @importFrom dplyr mutate group_by ungroup row_number case_when left_join distinct select arrange
#' @importFrom tidyr unnest
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' viz_data <- create_setlist_viz_data(alignment_data, song_code_lookup, concert_data)
#' head(viz_data)
#' }
#'
#' @export
create_setlist_viz_data <- function(alignment_data, song_code_lookup, concert_data) {
  # Split each sequence into individual characters
  expanded_data <- strsplit(alignment_data$sequence, "")
  names(expanded_data) <- alignment_data$showID
  expanded_df <- data.frame(
    showID = rep(names(expanded_data), sapply(expanded_data, length)),
    sequence = unlist(expanded_data),
    stringsAsFactors = FALSE
  )

  # Create position column and ensure it's numeric
  expanded_df$position <- as.numeric(ave(expanded_df$showID, expanded_df$showID, FUN = seq_along))

  # Match song codes to titles and short titles, handling gaps correctly
  expanded_df$is_gap <- expanded_df$sequence == "-"
  expanded_df$song_title <- ifelse(expanded_df$is_gap, "GAP",
                                   song_code_lookup$song_title[match(expanded_df$sequence, song_code_lookup$hex_char)])
  expanded_df$four_letter_code <- ifelse(expanded_df$is_gap, "GAP",
                                    song_code_lookup$four_letter_code[match(expanded_df$sequence, song_code_lookup$hex_char)])

  # Join with date and city information
  date_info <- unique(concert_data[, c("showID", "date", "city")])
  viz_data <- merge(expanded_df, date_info, by = "showID", all.x = TRUE)

  # Ensure date is in proper format
  viz_data$date <- as.Date(viz_data$date)

  # Select columns including city
  viz_data <- viz_data[, c("date", "position", "song_title", "four_letter_code", "is_gap", "city")]

  # Ensure position is numeric before sorting
  viz_data$position <- as.numeric(viz_data$position)

  # Sort the data frame by date and position
  viz_data <- viz_data[order(viz_data$date, viz_data$position), ]

  # Reset row names to ensure order is preserved
  rownames(viz_data) <- NULL

  return(viz_data)
}


#' Create a Palette of Distinct Colors
#'
#' @title Create a Palette of Distinct Colors
#' @description This function generates a palette of distinct colors that maximizes
#' differences between adjacent colors. It uses the HSV color space to create a
#' visually appealing and distinguishable set of colors.
#'
#' @param n An integer specifying the number of distinct colors to generate.
#'
#' @return A character vector of hex color codes.
#'
#' @details
#' The function creates colors by:
#' \itemize{
#'   \item Using the full range of hues (0-1)
#'   \item Keeping saturation relatively high (0.7-1)
#'   \item Alternating value (brightness) between higher and lower values (0.6-0.9)
#' }
#' Hues are shuffled to avoid similar adjacent colors.
#'
#' @examples
#' \dontrun{
#' # Generate a palette of 10 distinct colors
#' colors <- create_distinct_palette(10)
#' # Use the colors in a plot
#' plot(1:10, 1:10, col = colors, pch = 19, cex = 3)
#' }
#'
#' @importFrom grDevices hsv
#'
#' @export
create_distinct_palette <- function(n) {
  # Generate hues
  hues <- seq(0, 1, length.out = n)

  # Shuffle hues to avoid similar adjacent colors
  hues <- hues[c(seq(1, n, 2), seq(2, n, 2))]

  # Create colors with adjusted brightness for better contrast
  colors <- sapply(seq_len(n), function(i) {
    saturation <- 0.8 + (i %% 2) * 0.2  # Alternate between 0.8 and 1.0
    value <- 0.4 + (i %% 2) * 0.3       # Lower brightness for better contrast (range: 0.4-0.7)
    hsv(hues[i], saturation, value)
  })

  return(colors)
}



#' Perform Pairwise Sequence Alignment for Setlists
#'
#' @title Pairwise Sequence Alignment for Setlists
#' @description This function performs pairwise sequence alignment for setlists using
#' dynamic programming. It calculates the optimal alignment between two sequences
#' and returns the aligned sequences along with a similarity score.
#'
#' @param seq1 A character vector representing the first setlist sequence.
#' @param seq2 A character vector representing the second setlist sequence.
#' @param gap_penalty Numeric value for the gap penalty (default: -1).
#' @param match_score Numeric value for the match score (default: 2).
#' @param mismatch_penalty Numeric value for the mismatch penalty (default: -1).
#'
#' @return A list containing:
#'   \item{seq1_aligned}{The aligned sequence for seq1}
#'   \item{seq2_aligned}{The aligned sequence for seq2}
#'   \item{similarity}{A numeric value representing the similarity between the sequences}
#'
#' @details
#' The function uses a scoring system defined by the input parameters:
#' - Match score (default: 2)
#' - Mismatch penalty (default: -1)
#' - Gap penalty (default: -1)
#'
#' The alignment is performed using a dynamic programming approach with a scoring matrix
#' and a traceback matrix to determine the optimal alignment.
#'
#' @examples
#' seq1 <- c("A", "B", "C", "D")
#' seq2 <- c("A", "C", "D")
#' result <- align_pairwise_setlists(seq1, seq2)
#' print(result)
#'
#' # Using custom scoring parameters
#' result_custom <- align_pairwise_setlists(seq1, seq2, gap_penalty = -2, match_score = 3, mismatch_penalty = -2)
#' print(result_custom)
#'
#' @export

align_pairwise_setlists <- function(seq1, seq2, gap_penalty = -1, match_score = 2, mismatch_penalty = -1) {
  # Initialize scoring matrix
  n <- length(seq1)
  m <- length(seq2)
  score_matrix <- matrix(0, nrow = n + 1, ncol = m + 1)

  # Initialize traceback matrix
  # 1 = diagonal (match/mismatch), 2 = up (gap in seq2), 3 = left (gap in seq1)
  traceback <- matrix(0, nrow = n + 1, ncol = m + 1)

  # Initialize first row and column with gap penalties
  score_matrix[1, ] <- seq(0, m) * gap_penalty
  score_matrix[, 1] <- seq(0, n) * gap_penalty
  traceback[1, -1] <- 3
  traceback[-1, 1] <- 2

  # Fill the matrices
  for (i in 2:(n + 1)) {
    for (j in 2:(m + 1)) {
      # Calculate scores for all possible moves
      current_match_score <- ifelse(seq1[i-1] == seq2[j-1], match_score, mismatch_penalty)
      diagonal_score <- score_matrix[i-1, j-1] + current_match_score
      up_score <- score_matrix[i-1, j] + gap_penalty
      left_score <- score_matrix[i, j-1] + gap_penalty

      # Choose the best score
      score_matrix[i, j] <- max(diagonal_score, up_score, left_score)

      # Record the move in traceback matrix
      traceback[i, j] <- which.max(c(diagonal_score, up_score, left_score))
    }
  }

  # Traceback to get alignment
  aligned1 <- character()
  aligned2 <- character()

  i <- n + 1
  j <- m + 1

  while (i > 1 || j > 1) {
    if (i == 1) {
      # Must go left
      aligned1 <- c(NA, aligned1)
      aligned2 <- c(seq2[j-1], aligned2)
      j <- j - 1
    } else if (j == 1) {
      # Must go up
      aligned1 <- c(seq1[i-1], aligned1)
      aligned2 <- c(NA, aligned2)
      i <- i - 1
    } else {
      move <- traceback[i, j]
      if (move == 1) {
        # Diagonal
        aligned1 <- c(seq1[i-1], aligned1)
        aligned2 <- c(seq2[j-1], aligned2)
        i <- i - 1
        j <- j - 1
      } else if (move == 2) {
        # Up
        aligned1 <- c(seq1[i-1], aligned1)
        aligned2 <- c(NA, aligned2)
        i <- i - 1
      } else {
        # Left
        aligned1 <- c(NA, aligned1)
        aligned2 <- c(seq2[j-1], aligned2)
        j <- j - 1
      }
    }
  }

  # Calculate similarity score
  matches <- sum(aligned1 == aligned2, na.rm = TRUE)
  total_positions <- length(aligned1)
  similarity <- matches / total_positions

  result <- list(
    seq1_aligned = aligned1,
    seq2_aligned = aligned2,
    similarity = similarity
  )

  return(result)
}


#' @title Create Multiple Setlist Alignment
#'
#' @description This function creates a multiple setlist alignment using a custom
#' pairwise alignment approach followed the Needleman-Wunsch algorithm approach.
#'
#' @param data A data frame containing at least the following columns:
#'   \itemize{
#'     \item date: Date of the show
#'     \item song_title: Title of the song
#'   }
#' @param max_shows Integer. Maximum number of representative shows to include in the alignment (default: 20)
#'
#' @return A data frame with aligned setlists, containing the following columns:
#'   \itemize{
#'     \item date: Date of the show
#'     \item position: Position of the song in the aligned setlist
#'     \item song_title: Title of the song (or NA for gaps in the alignment)
#'   }
#'
#' @examples
#' \dontrun{
#' data(concert_data)
#' aligned_setlists <- create_setlist_alignment(concert_data, max_shows = 10)
#' head(aligned_setlists)
#' }
#'
#' @export
create_setlist_alignment <- function(data, max_shows = 20) {
  # Create sequence of songs for each show
  show_sequences <- aggregate(list(sequence = data$song_title),
                              by = list(date = data$date),
                              FUN = function(x) list(x))

  # Perform pairwise alignments using the new alignment function
  alignments <- tryCatch({
    all_pairs <- combn(show_sequences$date, 2, simplify = FALSE)
    result <- lapply(all_pairs, function(pair) {
      seq1 <- show_sequences$sequence[[which(show_sequences$date == pair[1])]]
      seq2 <- show_sequences$sequence[[which(show_sequences$date == pair[2])]]
      result <- align_pairwise_setlists(seq1, seq2)
      data.frame(date1 = pair[1], date2 = pair[2],
                 similarity = result$similarity)
    })
    do.call(rbind, result)
  }, error = function(e) {
    warning("Error in alignment calculation: ", e$message)
    return(NULL)
  })

  if (is.null(alignments)) {
    stop("Unable to calculate alignments. Check your data for inconsistencies.")
  }

  # Select representative setlists
  avg_similarities <- aggregate(similarity ~ date1, data = alignments, FUN = mean)
  representative_dates <- avg_similarities$date1[order(-avg_similarities$similarity)][1:min(max_shows, nrow(avg_similarities))]

  # For visualization, align all setlists to the most representative one
  most_representative_date <- representative_dates[1]
  reference_sequence <- show_sequences$sequence[[which(show_sequences$date == most_representative_date)]]

  # Align all setlists to the reference
  aligned_setlists <- lapply(representative_dates, function(date) {
    seq <- show_sequences$sequence[[which(show_sequences$date == date)]]
    alignment <- align_pairwise_setlists(reference_sequence, seq)
    data.frame(
      date = rep(date, length(alignment$seq2_aligned)),
      position = seq_along(alignment$seq2_aligned),
      song_title = alignment$seq2_aligned,
      stringsAsFactors = FALSE
    )
  })

  # Combine all aligned setlists into a single data frame
  result <- do.call(rbind, aligned_setlists)
  rownames(result) <- NULL
  return(result)
}



#' Format Alignment Data
#'
#' This function processes aligned setlist data and formats it into an alignment data frame
#' with show IDs and encoded song sequences, including dashes for skipped songs.
#'
#' @param aligned_setlists A data frame containing aligned setlist information.
#' @param codes_tour_songs A data frame containing song codes and hex characters.
#' @param concert_data A data frame containing concert information including show IDs.
#'
#' @return A data frame with columns:
#'   \item{showID}{Numeric identifier for each show}
#'   \item{sequence}{Character string representing the encoded song sequence}
#'
#' @export
format_alignment_data <- function(aligned_setlists, codes_tour_songs, concert_data) {
  # Create lookup tables
  showID_lookup <- unique(concert_data[, c("date", "showID")])
  hex_char_lookup <- setNames(codes_tour_songs$hex_char, codes_tour_songs$song_title)

  # Function to get hex char for a song
  get_hex_char <- function(song) {
    if (is.na(song)) return("-")
    hex_char <- hex_char_lookup[song]
    if (is.na(hex_char)) return("-")
    return(hex_char)
  }

  # Process aligned_setlists
  result <- data.frame()
  for (date in unique(aligned_setlists$date)) {
    show_data <- aligned_setlists[aligned_setlists$date == date, ]
    showID <- showID_lookup$showID[showID_lookup$date == date]

    if (length(showID) == 0) {
      warning(paste("No showID found for date:", date))
      next
    }

    sequence <- sapply(show_data$song_title, get_hex_char)
    sequence <- paste(sequence, collapse = "")

    result <- rbind(result, data.frame(showID = showID, sequence = sequence))
  }

  # Ensure showID is numeric
  result$showID <- as.numeric(result$showID)

  return(result)
}
