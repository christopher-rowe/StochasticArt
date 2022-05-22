#' Generate panels
#'
#' @param num_panels the number of panels in the artwork; if unspecified, a value
#' will be chosen randomly
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'
generate_panels = function(num_panels){

  # randomly choose parameter values if not specified
  if(missing(num_panels)){num_panels = sample(1:50, 1)}

  # generate x values
  panel_values = seq(0,1, length.out=num_panels+1)
  xmin = panel_values[1:num_panels]
  xmax = panel_values[2:(num_panels+1)]

  # generate bottom & top panel data
  bottom_panel_data = dplyr::tibble(
    xmin = xmin, xmax = xmax,
    ymin = rep(0,num_panels), ymax = runif(num_panels)
  )

  top_panel_data = dplyr::tibble(
    xmin = xmin, xmax = xmax,
    ymin = bottom_panel_data$ymax, ymax = rep(1,num_panels)
  )

  # randomly pick a color
  color = rgb(
    red = sample(0:255, 1), blue = sample(0:255, 1), green = sample(0:255, 1),
    maxColorValue = 255
  )

  # flip the alpha coin to determine the direction of the alpha gradient
  alpha_coin = rbinom(1, 1, 0.5) == 1

  # generate plot
  p = ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = bottom_panel_data,
      ggplot2::aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, alpha = if(alpha_coin){xmin} else{1-xmin}),
      fill = color, col = 'white') +
    ggplot2::geom_rect(
      data = top_panel_data,
      ggplot2::aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, alpha = if(alpha_coin){1-xmin} else{xmin}),
      fill = color, col = 'white') +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
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

