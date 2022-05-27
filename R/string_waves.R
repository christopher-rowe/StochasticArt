#' Generate string waves
#'
#' @param num_waves an integer value >=1, the number of waves desired
#' @param wobbliness an integer value >=1, higher values will make the waves
#' more wobbly (i.e. having peaks a trough of variable height); technically
#' this is the number if individual sine functions that are summed together to get
#' the final wave function
#' @param frequency a numberic value >= 0, higher values will increase the frequency
#' of the wave function; technically this is the passed to the frequency argument
#' of the random_sine_wave() function (see random_sine_wave() for details)
#' @param stability_n an integer value >=1, higher values will make the waves more
#' stable; technically, this is the number of data points sampled to generate
#' each wave
#' @param stability_sd a numeric value >=0, lower values will make the waves
#' more stable; technically, this is the standard deviation of the error applied
#' to the output of each wave function
#' @param loess_span a numeric value between 0-1, lower numbers will make each wave
#' more "wiggly" when fitting the underlying points, values < 0.3 seem to throw an
#' error
#' @param anchor a boolean value, set to true to make the waves converge to
#' anchor points
#' @param frame 'points', 'circle', 'square', or 'none'; specifies the frame desired for
#' each artwork
#' @param seed a numeric value, the seed to initialize psuedo-randomness
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'
generate_string_waves = function(num_waves, num_colors, wobbliness, frequency,
                                 stability_n, stability_sd, loess_span, anchor = F,
                                 frame = 'square', seed){

  # check frame argument
  assertthat::assert_that(
    frame %in% c('points', 'circle', 'square', 'none'),
    msg = "frame must be one of 'points', 'circle', 'square', or 'none'"
    )

  # set & print seed
  if(missing(seed)){seed = sample(1:1e8, 1)}
  set.seed(seed)
  message(paste('Seed:', seed))

  # randomly choose parameter values if not specified
  if(missing(num_waves)){num_waves = sample(1:100,1)}
  if(missing(num_colors)){num_colors = sample(1:5, 1)}
  if(missing(wobbliness)){wobbliness = sample(1:10, 1)}
  if(missing(frequency)){frequency = sample(1:30, 1)}
  if(missing(stability_n)){stability_n = 200}
  if(missing(stability_sd)){stability_sd = 0.3}
  if(missing(loess_span)){loess_span = runif(1,0.3,0.7)}


  # randomly pick a color
  color_pool = rgb(
    red = sample(0:255, 1), blue = sample(0:255, 1), green = sample(0:255, 1),
    maxColorValue = 255
  )

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


  # generate data
  all_data = dplyr::tibble(
    x=rep(c(0,1), num_waves),
    group = rep(1:num_waves, each = 2)
  ) %>%
    dplyr::bind_rows(
      dplyr::tibble(
        x = runif(num_waves*(stability_n-2)),
        group = rep(1:num_waves, each = stability_n-2)
      )
    ) %>%
    dplyr::mutate(
      y = random_sine_wave(x, wobbliness, frequency) + rnorm(n=(stability_n*num_waves),mean=0,sd=stability_sd)
    )

  # generate and append anchors so all waves converge, if specified
  y_anchor_x_min = all_data %>% dplyr::filter(x==0) %>% dplyr::pull(y) %>% mean()
  y_anchor_x_max = all_data %>% dplyr::filter(x==1) %>% dplyr::pull(y) %>% mean()
  if(anchor){

    n_anchor = 100
    anchor_data = dplyr::tibble(
      x = c(rep(-0.2, num_waves*n_anchor), rep(1.2, num_waves*n_anchor)),
      y = c(rep(y_anchor_x_min,num_waves*n_anchor), rep(y_anchor_x_max,num_waves*n_anchor)),
      group = rep(rep(1:num_waves, n_anchor), 2)
    )

  } else {
    anchor_data = dplyr::tibble(x=numeric(), y=numeric(), group=numeric())
  }

  all_data = all_data %>%
    dplyr::filter(!is.na(group)) %>%
    dplyr::bind_rows(anchor_data) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(color = sample(color_pool, 1),
                  alpha = runif(1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(color = factor(color, levels = unique(.$color)))

  # define plot limits
  circle_c =  (c(1.2, y_anchor_x_max) + c(-0.2, y_anchor_x_min)) / 2
  circle_r = norm((c(1.2, y_anchor_x_max) - circle_c), type = '2')
  plot_lim = c(min(c(all_data$x, all_data$y, circle_c - circle_r)),
               max(c(all_data$x, all_data$y, circle_c + circle_r)))

  # generate plot
  p = ggplot2::ggplot() +
    {if(frame=='square'){
      ggplot2::geom_rect(ggplot2::aes(xmin=min(all_data$x),xmax=max(all_data$x),
                                      ymin=min(all_data$y)-0.1,ymax=max(all_data$y)+0.1),
                         alpha = runif(1), fill = 'grey50', col = 'grey50')
    }} +
    ggplot2::geom_smooth(
      data = all_data,
      ggplot2::aes(x = x, y = y, group=group, color=color, alpha=alpha),
      method = 'loess', formula = y ~ x, span = loess_span, se = FALSE, size = 0.1) +
    ggplot2::scale_color_manual(values = levels(all_data$color)) +
    ggplot2::scale_alpha_identity() +
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

  if(frame == 'circle'){
    p = p + ggforce::geom_circle(
      ggplot2::aes(x0 = circle_c[1], y0 = circle_c[2], r = circle_r),
      col = 'grey50')
  } else if(frame == 'points'){
    p = p + ggplot2::geom_point(
      data = anchor_data %>% dplyr::distinct(x,y),
      ggplot2::aes(x=x,y=y),
      shape = 21, col = 'white', fill = 'black', size = 4, stroke = 1)
  }


  if(rbinom(1,1,0.5)==1){
    p + ggplot2::coord_flip()
  } else {
    p
  }


}

