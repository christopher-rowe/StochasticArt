
#' Generate inner concentric circles
#'
#' @param x0 a numeric value, the x-coordinate of the center of the source circle
#' @param y0 a numeric value, the y-coordinate of the center of the source circle
#' @param r a numeric value, the radius of the source circle
#' @param color a character value, the color (hex code) of the source circle
#' @param num_inner_circles an integer value, the desired number of inner concentric
#' circles
#' @param equal_spacing a boolean value, if true the spacing betwen concentric
#' circles will be equal
#'
#' @return a tibble contain with one row per concentric circle, including the
#' original source circle, including x0, y0, r, and color
#'
generate_inner_circles = function(x0, y0, r, color, num_inner_circles = 10,
                                equal_spacing = TRUE){

  if(equal_spacing){
    p = seq(0,1, length.out = num_inner_circles + 2)[2:(num_inner_circles+2)]
  } else {
    p = c(runif(num_inner_circles), 1)
  }

  dplyr::tibble(
    x0 = rep(x0, num_inner_circles + 1),
    y0 = rep(y0, num_inner_circles + 1),
    r = r * p,
    color = rep(color, num_inner_circles + 1)
  ) %>%
    return()


}

#' Create inner concentric circles
#'
#' @param num_circles an integer value, the number of circles in the artwork
#' @param global_max_r a numeric value, the maximum allowable radius of each circle
#' @param circle_gap_p a numeric value, higher values increase the gap between
#' each circle; technically, the radius of a new circle will have a max radius of
#' (1 - circle_gap_p) multiplied by the distance from the center of the new circle
#' to the edge of the nearest other circle
#' @param max_alpha a numeric value between 0-1, the maximum alpha value for each
#' circle
#' @param min_concentric_circles an integer value, the minimum number of concentric
#' circles within each circle
#' @param max_concentric_circles an integer value, the maximum number of concentric
#' circles within each circle
#' @param equal_concentric_spacing a boolean value, set to true to make concentric
#' circles evenly spaced
#' @param num_colors an integer value, the number of colors in the artwork; each
#' circle is randomly assigned a color from a pool of this size
#' @param frame a boolean value, set to true to include a circular frame around the
#' artwork
#' @param seed a numeric value, the seed to initialize psuedo-randomness
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'
generate_circles = function(num_circles, global_max_r,
                            circle_gap_p, max_alpha, min_concentric_circles = 1,
                            max_concentric_circles = 20,
                            equal_concentric_spacing, num_colors,
                            frame = T, seed){

  # set & print seed
  if(missing(seed)){seed = sample(1:1e8, 1)}
  set.seed(seed)
  message(paste('Seed:', seed))

  # randomly choose parameter values if not specified
  if(missing(num_circles)){num_circles = sample(1:300, 1)}
  if(missing(global_max_r)){global_max_r = runif(1, 0.1, 0.3)}
  if(missing(circle_gap_p)){circle_gap_p = runif(1, 0, 0.01)}
  if(missing(max_alpha)){max_alpha = runif(1, 0, 1)}
  if(missing(equal_concentric_spacing)){equal_concentric_spacing = rbinom(1,1,0.5)==1}
  if(missing(num_colors)){num_colors = sample(1:5, 1)}

  # initialize storage for circles
  circles = dplyr::tibble(
    x0 = numeric(),
    y0 = numeric(),
    r = numeric()
  )

  # generate non-overlapping circles
  for(i in 1:num_circles){

    too_close = TRUE

    while(too_close){

      # for aesthetics, we restrict the centers of circles to be within a circle
      out_of_bounds = TRUE
      while(out_of_bounds){
        new_x0 = runif(1)
        new_y0 = runif(1)
        new_norm = norm(c(new_x0, new_y0) - c(0.5, 0.5), type = '2')
        out_of_bounds = new_norm > 0.5
      }

      # calculate distance from the center of the new circle to the edge of each
      # existing circle
      check = circles %>%
        dplyr::mutate(
          dist = sqrt((x0 - new_x0)^2 + (y0 - new_y0)^2) - r
        )

      # if any distance is <0, new circle is too close, try again
      too_close = any(check$dist<0)

      # if the new circle is not too close, safe it & proceed
      if(!too_close){
        local_max_r = 0.5 - new_norm
        if(nrow(circles)==0){
          r = runif(1, 0, min(local_max_r, global_max_r))
        } else {
          r = min((1-circle_gap_p)*min(check$dist), local_max_r, global_max_r)
        }
        circles = circles %>%
          dplyr::bind_rows(
            dplyr::tibble(
              x0 = new_x0,
              y0 = new_y0,
              r = min(r, global_max_r)
            )
          )
      }
    }
  }


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

  # gennerate concentric circles
  circles = circles %>%
    dplyr::mutate(
      color = sample(color_pool, num_circles, replace = T),
      num_inner_circles = sample(min_concentric_circles:max_concentric_circles,
                                 num_circles, replace = T)
    ) %>%
    purrr::pmap(., generate_inner_circles, equal_spacing = equal_concentric_spacing) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(alpha=runif(n=nrow(.), 0, max_alpha),
                  color = factor(color, levels = unique(.$color))) %>%
    dplyr::arrange(x0)

  # generate plot
  p = ggplot2::ggplot() +
    ggforce::geom_circle(data=circles,
                         ggplot2::aes(x0 = x0, y0 = y0, r = r, fill = color,
                                      alpha = alpha),
                         col = 'white') +
    ggplot2::scale_fill_manual(values = levels(circles$color)) +
    ggplot2::scale_alpha_identity() +
    ggplot2::theme_classic() +
    ggplot2::xlim(-0.02, 1.02) +
    ggplot2::ylim(-0.02, 1.02) +
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

  if(frame){
    p + ggforce::geom_circle(
      ggplot2::aes(x0 = 0.5, y0 = 0.5, r = 0.51),
      col = main_color, lwd = 0.5)
  } else {
    p
  }

}
