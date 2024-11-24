#' @title Calculate Distance Matrix from Sequences
#'
#' @description This function calculates a distance matrix based on the pairwise differences between sequences.
#'
#' @param sequences A list of character strings representing concert setlist sequences.
#' @return A distance matrix where each entry (i, j) represents the distance between sequences i and j.
#' @examples
#' seq_list <- list(showID1408 = "-5fD-UeE4OY-H]CJWhQla-mBk", showID1320 = "-5fD-^_E4OY-H]CJWhQeU-mBk", showID1336 = "5fD-UeE4OY-H]CJWhQei-mBk")
#' dist_matrix <- calculate_distance_matrix(seq_list)
#' @export
calculate_distance_matrix <- function(sequences) {
  n <- length(sequences)
  seq_names <- names(sequences)

  # Initialize distance matrix
  dist_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(dist_matrix) <- colnames(dist_matrix) <- seq_names

  # Calculate distances
  for (i in 1:n) {
    for (j in 1:n) {
      dist_matrix[i, j] <- sum(strsplit(sequences[[i]], '')[[1]] != strsplit(sequences[[j]], '')[[1]], na.rm = TRUE) / length(sequences[[i]])
    }
  }

  return(dist_matrix)
}

#' @title reate City Date Tree Labels
#'
#' @description This function generates tree labels based on concert data, creating labels that combine city and date information.
#' The input data must contain showIDs of the shows for which the distance tree is being created.
#'
#' @param concert_data A data frame containing concert information, including showID, city, date, country, and leg.
#' @param setlist_tree A tree object containing tip labels corresponding to showIDs.
#' @param show_info A data frame containing show information, including city-date, country, and leg.
#' @return A character vector of labels for the tree tips, formatted as "city - YYYY-MM-DD".
#' @examples
#' concert_data <- data.frame(showID = c(1, 2), city = c("New York", "Los Angeles"),
#'                            date = as.Date(c("2023-10-01", "2023-10-02")),
#'                            country = c("USA", "USA"), leg = c("Leg 1", "Leg 1"))
#' setlist_tree <- list(tip.label = c("showID1", "showID2")) # Example tree structure
#' show_info <- data.frame(city_date = paste("City", 1:10),
#'                          country = sample(c("USA", "Canada"), 10, replace = TRUE),
#'                          leg = sample(c("Leg 1", "Leg 2"), 10, replace = TRUE))
#' labels <- create_city_date_tree_labels(concert_data, setlist_tree, show_info)
#' @export
create_city_date_tree_labels <- function(concert_data, setlist_tree, show_info) {

  # Convert showID to character to match the tree labels
  show_info$showID <- paste0("showID", show_info$showID)

  # Function to get city data based on show ID
  get_city_date <- function(id, info_df) {
    row <- info_df[info_df$showID == id, ]
    if (nrow(row) > 0) {
      as.character(row$city_date)
    } else {
      id  # Return just the ID if no match found
    }
  }

  # Create tree labels using the provided setlist_tree
  new_labels <- sapply(setlist_tree$tip.label, get_city_date, info_df = show_info)

  return(new_labels)
}



#' @title Generate a ggtree Plot from a Distance Tree
#'
#' @description This function generates a ggtree plot from a given distance tree object.
#' It utilizes the `ggtree` package to create a visual representation of the phylogenetic tree,
#' including tips and customized themes for better visualization.
#'
#' @param setlist_tree A distance tree object (of class `phylo` or `hclust`) to be plotted.
#'
#' @return A `ggtree` plot object that can be printed or further customized.
#'
#' @importFrom ggtree ggtree
#' @importFrom ggtree geom_tiplab
#' @importFrom ggtree theme_tree2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 scale_x_continuous
#'
#' @examples
#' library(ggtree)
#' # Example tree structure (replace with actual tree object)
#' setlist_tree <- rtree(10)  # Generate a random tree with 10 tips
#' plot <- create_ggtree_plot(setlist_tree)
#' print(plot)
#' @export
create_ggtree_plot <- function(setlist_tree) {

  # Create the ggtree plot
  tree_plot <- ggtree(setlist_tree) +
    geom_tiplab(size = 2.5, hjust = -0.1) +
    theme_tree2() +
    theme(
      plot.margin = margin(r = 20, l = 10, b = 10, t = 10)
    ) +
    coord_cartesian(clip = 'off') +  # Prevent clipping of labels
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.3)))  # Add padding

  return(tree_plot)
}


#' @title Generate a ggtree Plot with Colored Tips
#'
#' @description This function generates a ggtree plot from a given concert setlists distance tree.
#' The tree tips are colored based on either country or leg, allowing for visual differentiation
#' of the tips in the plot.
#'
#' @param setlist_tree A concert setlists distance tree object (of class `phylo` or `hclust`) to be plotted.
#' @param show_info A data frame containing show information, including city-date, country, and leg.
#' @param color_by A string indicating whether to color tips by "country" or "leg". Default is "country".
#'
#' @return A `ggtree` plot object with colored tips based on the specified grouping.
#'
#' @importFrom ggtree ggtree
#' @importFrom ggtree geom_tiplab
#' @importFrom ggtree geom_tippoint
#' @importFrom ggtree theme_tree2
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 scale_x_continuous
#'
#' @examples
#' library(ggtree)
#' # Example tree structure (replace with actual tree object)
#' setlist_tree <- rtree(10)  # Generate a random tree with 10 tips
#' show_info <- data.frame(city_date = paste("City", 1:10),
#'                          country = sample(c("USA", "Canada"), 10, replace = TRUE),
#'                          leg = sample(c("Leg 1", "Leg 2"), 10, replace = TRUE))
#' plot <- create_ggtree_plot_colored(setlist_tree, show_info, color_by = "country")
#' print(plot)
#' @export
create_ggtree_plot_colored <- function(setlist_tree, show_info, color_by = c("country", "leg")) {

  # Match the input for color_by
  color_by <- match.arg(color_by)

  if (color_by == "country") {
    # Create a color mapping based on unique countries present in show_info
    unique_countries <- unique(show_info$country)
    country_colors <- setNames(rainbow(length(unique_countries)), unique_countries)
    # Create a mapping for city-date to country
    city_date_to_country <- setNames(show_info$country, show_info$city_date)

    # Create the tree plot with colors based on country
    tree_plot <- ggtree(setlist_tree) +
      geom_tiplab(size = 2.5, hjust = -0.1) +
      geom_tippoint(aes(color = city_date_to_country[label]), size = 3) +  # Map colors to aesthetic
      scale_color_manual(
        values = country_colors,
        name = "Country",
        breaks = names(country_colors)
      )

  } else if (color_by == "leg") {
    # Create a color mapping based on unique legs present in show_info
    unique_legs <- unique(show_info$leg)
    leg_colors <- setNames(rainbow(length(unique_legs)), unique_legs)
    # Create a mapping for city-date to leg
    city_date_to_leg <- setNames(show_info$leg, show_info$city_date)

    # Create the tree plot with colors based on leg
    tree_plot <- ggtree(setlist_tree) +
      geom_tiplab(size = 2.5, hjust = -0.1) +  # Adjust hjust to move labels right
      geom_tippoint(aes(color = city_date_to_leg[label]), size = 3) +  # Map colors to aesthetic
      scale_color_manual(
        values = leg_colors,
        name = "Tour Leg",
        breaks = names(leg_colors)
      )
  }

  # Common theme settings for both plots
  tree_plot <- tree_plot +
    theme_tree2() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(r = 20, l = 10, b = 30, t = 10)
    ) +
    coord_cartesian(clip = 'off') +  # Prevent clipping of labels
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.3)))  # Add padding

  return(tree_plot)
}
