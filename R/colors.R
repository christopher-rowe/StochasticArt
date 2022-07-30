#' Get a random color pool
#'
#' @param num_colors number of colors desired
#'
#' @export
#'
#' @return a vector of adjacent hex codes
#'

get_random_color_pool = function(num_colors){

  # randomly pick a color
  main_color = rgb(
    red = sample(0:255, 1), blue = sample(0:255, 1), green = sample(0:255, 1),
    maxColorValue = 255
  )
  color_pool = main_color

  # grow the color pool as specified
  if(num_colors>1){
    while(length(color_pool)<num_colors){
      color_pool = color_pool %>%
        purrr::map(function(x) colortools::adjacent(color = x, plot=F)) %>%
        purrr::reduce(c) %>%
        unique()
    }
    color_pool = sample(color_pool, size = num_colors)
  }

  return(color_pool)
}
