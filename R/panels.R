#' Generate panels
#'
#' @param num_panels the number of panels in the artwork; if unspecified, a value
#' will be chosen randomly
#' @param square a boolean value, set to TRUE for the artwork to be square
#' @param single_color a boolean value, set to TRUE to use a single color
#' @param seed a numeric value, the seed to initialize psuedo-randomness
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'
generate_panels = function(num_panels, square = TRUE, single_color = TRUE, seed){

  # set & print seed
  if(missing(seed)){seed = sample(1:1e8, 1)}
  set.seed(seed)
  message(paste('Seed:', seed))

  # randomly choose parameter values if not specified
  if(missing(num_panels)){num_panels = sample(1:50, 1)}

  # generate x values
  panel_values = seq(0,1, length.out=num_panels+1)
  xmin = panel_values[1:num_panels]
  xmax = panel_values[2:(num_panels+1)]

  # generate bottom & top panel data
  bottom_panel_data = dplyr::tibble(
    xmin = xmin,
    xmax = xmax,
    ymin = {if(square){rep(0,num_panels)} else {runif(num_panels, -0.25, 0)}},
    ymax = runif(num_panels)
  )

  top_panel_data = dplyr::tibble(
    xmin = xmin,
    xmax = xmax,
    ymin = bottom_panel_data$ymax,
    ymax = {if(square){rep(1,num_panels)} else {runif(num_panels, 1, 1.25)}},
  )

  # randomly pick a color
  color = rgb(
    red = sample(0:255, 1), blue = sample(0:255, 1), green = sample(0:255, 1),
    maxColorValue = 255
  )

  # assign bottom/top colors to same or different colors as specified
  if(single_color){
    bottom_color = color
    top_color = color
  } else {
    bottom_color = color
    top_color = colortools::adjacent(color, plot=F)[sample(2:3,1)]
  }

  # flip the alpha coin to determine the direction of the alpha gradient
  alpha_coin = rbinom(1, 1, 0.5) == 1

  # generate plot
  p = ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = bottom_panel_data,
      ggplot2::aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, alpha = if(alpha_coin){xmin} else{1-xmin}),
      fill = bottom_color, col = 'white') +
    ggplot2::geom_rect(
      data = top_panel_data,
      ggplot2::aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, alpha = if(alpha_coin){1-xmin} else{xmin}),
      fill = top_color, col = 'white') +
    ggplot2::xlim(-0.25, 1.25) +
    ggplot2::ylim(-0.25, 1.25) +
    ggplot2::theme_classic() +
    ggplot2::theme(aspect.ratio = 1,
                   axis.line = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank())

  # randomly flip the axes
  if(rbinom(1,1,0.5)==1){
    p + ggplot2::coord_flip()
  } else {
    p
  }

}

