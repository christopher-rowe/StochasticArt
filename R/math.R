#' Apply a random sine wave
#'
#' @param x a vector of x values
#' @param num_sine_waves number individual sine waves to sum
#' @param frequency a numeric value, frequency will be selected randomly between
#' the - and + of the value specified in this argument
#' @param seed a numeric value, the seed to initialize psuedo-randomness
#' @param print_seed a boolean value, set to TRUE to print seed
#'
#' @export
#'
#' @return a vector of y values for the random sine wave
#'
apply_random_sine_wave = function(x, num_sine_waves, frequency, seed,
                                  print_seed = FALSE){

  # set & print seed
  if(missing(seed)){seed = sample(1:1e8, 1)}
  set.seed(seed)
  if(print_seed){message(paste('Seed:', seed))}

  if(missing(frequency)){frequency = sample(5:30, 1)}
  if(missing(num_sine_waves)){num_sine_waves = sample(1:100,1)}

  y = rep(0, length(x))

  for(i in 1:num_sine_waves){
    a = runif(1, -10, 10)
    b = runif(1, -frequency, frequency)
    y = y + a*sin(b*x)
  }

  y = (y - min(y)) / (max(y) - min(y))

  return(y)

}

#' Rotate points
#'
#' @param x_y_df a dataframe/tibble with columns x & y
#' @param angle a numeric value, specified angle of rotation in radians
#' @param seed a numeric value, the seed to initialize psuedo-randomness
#' @param print_seed a boolean value, set to TRUE to print seed
#'
#' @export
#'
#' @return a vector of y values for the random sine wave
#'

rotate_points = function(x_y_df, angle, seed, print_seed = FALSE){

  # set & print seed
  if(missing(seed)){seed = sample(1:1e8, 1)}
  set.seed(seed)
  if(print_seed){message(paste('Seed:', seed))}

  if(missing(angle)){angle = runif(1,0,2*pi)}

  # identify centroid of all points & create centroid matrix
  centroid = c(mean(x_y_df$x), mean(x_y_df$y))
  centroid_matrix = matrix(rep(centroid, nrow(x_y_df)), ncol = 2, byrow = T)

  # define rotation matrix
  r_matrix = matrix(
    c(cos(angle), -sin(angle), sin(angle), cos(angle)),
    ncol = 2
  )

  # center and transpose input coordinates
  t_c_x_y = (as.matrix(x_y_df[,1:2]) - centroid_matrix) %>% t()

  # apply rotation, transpose, and un-center
  r_x_y = (r_matrix %*% t_c_x_y) %>% t() + centroid_matrix

  # save output such that we retain any additional columns in x_y_df
  r_x_y_df = x_y_df
  r_x_y_df$x = r_x_y[,1]
  r_x_y_df$y = r_x_y[,2]

  # return
  return(r_x_y_df)

}
