#' @title generate_clusters 
#' 
#' @description Brief description of what the function does.
#' 
#' @param param1 Description of the first parameter, including its type (e.g., numeric, character).
#' @param param2 Description of the second parameter. Mention any special behavior, such as default values.
#' @param ... Description of additional arguments (if applicable). 
#' 
#' @return Description of the output value or object the function returns.
#' 
#' @details Additional details about the function, such as implementation notes or edge cases.
#' 
#' @examples
#' # Example usage
#' example_result <- example_function(10, 20)
#' print(example_result)
#'
#' @export
generate_clusters <- function(J = 100, njrange = c(50, 100), seed) {
    # library(tidyverse)
    # Generate random cluster sizes within the specified range
    nj_sizes <- runif(J, njrange[1], njrange[2]) |> round()
    
    # Total number of individuals
    N <- sum(nj_sizes)
    
    # Create the initial data frame with individual IDs and cluster IDs
    data <- data.frame(id = 1:N,
                       school = unlist(map(1:J, ~ rep(.x, each = nj_sizes[.x])))) |> 
        group_by(school) |> 
        # Create a standardized cluster size variable 'W_nj'
        mutate(W_nj = (n() - njrange[1]) / (njrange[2] - njrange[1])) |> # NOTE: maybe reconsider 
        ungroup()
    
    return(list(
        data = data,
        nj_sizes = nj_sizes,
        njrange = njrange
    ))
}

