# generate new similar colors for the countries
generate_similar_colors <- function(base_color, n, variation=5) {
  colors <- c()
  while (length(colors) < n) {
    new_color <- col2rgb(base_color) + sample(-variation:variation, 3, replace = TRUE)
    new_color <- pmin(pmax(new_color, 0), 255)
    hex_color <- rgb(new_color[1], new_color[2], new_color[3], maxColorValue = 255)
    if (!hex_color %in% colors) {
      colors <- c(colors, hex_color)
    }
  }
  return(colors)
}

# ensure new colors are unique
generate_unique_colors <- function(existing_colors, base_color, n, variation=5) {
  unique_colors <- c()
  while (length(unique_colors) < n) {
    candidate_colors <- generate_similar_colors(base_color, n * 2, variation)
    candidate_colors <- setdiff(candidate_colors, existing_colors)
    unique_colors <- unique(c(unique_colors, candidate_colors))
    unique_colors <- unique_colors[1:min(length(unique_colors), n)]
  }
  return(unique_colors)
}


