#' Generate string waves
#'
#' @param stability_n an integer value >=1, higher values will make the waves more
#' stable; technically, this is the number of data points sampled to generate
#' each wave
#' @param stability_sd a numeric value >=0, lower values will make the waves
#' more stable; technically, this is the standard deviation of the error applied
#' to the output of each wave function
#' @param wobbliness an integer value >=1, higher values will make the waves
#' more wobbly (i.e. having peaks a trough of variable height); technically
#' this is the number if individual sine functions that are summed together to get
#' the final wave function
#' @param frequency a numberic value >= 0, higher values will increase the frequency
#' of the wave function; technically this is the passed to the frequency argument
#' of the random_sine_wave() function (see random_sine_wave() for details)
#' @param frame 'points', 'circle' or 'none'; specifies the frame desired for
#' each artwork; defaults to 'points' because they look kind of cool.
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'
generate_string_waves = function(num_waves, stability_n, stability_sd,
                                 wobbliness, frequency, frame = 'points'){

  # check frame argument
  assertthat::assert_that(
    frame %in% c('points', 'circle', 'none'),
    msg = "frame must be one of 'points', 'circle', or 'none'"
    )

  # randomly choose parameter values if not specified
  if(missing(num_waves)){num_waves = sample(1:200,1)}
  if(missing(stability_n)){stability_n = 200}
  if(missing(stability_sd)){stability_sd = 0.3}
  if(missing(wobbliness)){wobbliness = sample(1:10, 1)}
  if(missing(frequency)){frequency = sample(1:30, 1)}

  # generate data
  all_data = dplyr::tibble(
    x = runif(stability_n*num_waves),
    group = rep(1:num_waves, each = stability_n)
    ) %>%
    dplyr::bind_rows(
      dplyr::tibble(x=0:1) # for generating anchor values
    ) %>%
    dplyr::mutate(
      y = random_sine_wave(x, wobbliness, frequency) + rnorm(n=(stability_n*num_waves) +2,mean=0,sd=stability_sd)
    )

  # generate and append anchors so all waves converge
  n_anchor = 100
  y_anchor_x_min = all_data %>% dplyr::filter(x==0) %>% dplyr::pull(y)
  y_anchor_x_max = all_data %>% dplyr::filter(x==1) %>% dplyr::pull(y)
  anchor_data = dplyr::tibble(
    x = c(rep(-0.2, num_waves*n_anchor), rep(1.2, num_waves*n_anchor)),
    y = c(rep(y_anchor_x_min,num_waves*n_anchor), rep(y_anchor_x_max,num_waves*n_anchor)),
    group = rep(rep(1:num_waves, n_anchor), 2)
  )
  all_data = all_data %>%
    dplyr::filter(!is.na(group)) %>%
    dplyr::bind_rows(anchor_data)

  circle_c =  (c(1.2, y_anchor_x_max) + c(-0.2, y_anchor_x_min)) / 2
  circle_r = norm((c(1.2, y_anchor_x_max) - circle_c), type = '2')

  # randomly pick a color
  color = rgb(
    red = sample(0:255, 1), blue = sample(0:255, 1), green = sample(0:255, 1),
    maxColorValue = 255
  )

  # define plot limits
  plot_lim = c(min(c(all_data$x, all_data$y, circle_c - circle_r)),
               max(c(all_data$x, all_data$y, circle_c + circle_r)))

  # generate plot
  p = ggplot2::ggplot() +
    ggplot2::geom_smooth(
      data = all_data,
      ggplot2::aes(x = x, y = y, group=group),
      method = 'gam', formula = y ~ s(x, bs = "cs"), se = FALSE, size = 0.1,
      col = color) +
    ggplot2::theme_classic() +
    ggplot2::xlim(plot_lim) +
    ggplot2::ylim(plot_lim) +
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

  if(frame == 'circle'){
    p = p + ggforce::geom_circle(
      ggplot2::aes(x0 = circle_c[1], y0 = circle_c[2], r = circle_r),
      col = 'grey50')
  } else if(frame == 'points'){
    p = p + ggplot2::geom_point(
      data = anchor_data %>% dplyr::distinct(x,y),
      ggplot2::aes(x=x,y=y),
      shape = 21, col = 'white', fill = 'black', size = 5, stroke = 1)
  }


  if(rbinom(1,1,0.5)==1){
    p + ggplot2::coord_flip()
  } else {
    p
  }


}

