#' Generate lines and rectangles
#'
#' @param num_lines an integer value >=0, the number of lines in the artwork;
#' if unspecified, a value will be chosen randomly
#' @param p_v_line a numeric value between 0 and 1, the probability that each
#' line will be vertical as opposed to horizontal; if unspecified, a value will
#' be chosen randomly
#' @param max_line_size a numeric value between 0 and 1, the maximum size of
#' each line; if unspecified, a value will be chosen randomly
#' @param num_rect an integer value >=0, the number of rectangles in the artwork;
#' if unspecified, a value will be chosen randomly
#' @param max_rect_size a numberic value between 0 and 1, the maximum size of
#' each rectangle; if unspecified, a value will be chosen randomly
#' @param seed a numeric value, the seed to initialize psuedo-randomness
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'
generate_lines_and_rectangles = function(num_lines, p_v_line, max_line_size,
                                         num_rect, max_rect_size, seed){

  # set & print seed
  if(missing(seed)){seed = sample(1:1e8, 1)}
  set.seed(seed)
  message(paste('Seed:', seed))

  # randomly choose parameter values if not specified
  if(missing(num_lines)){num_lines = sample(250:2000, 1)}
  if(missing(p_v_line)){p_v_line = runif(1, 0, 1)}
  if(missing(max_line_size)){max_line_size = rbeta(1, 1, 4)}
  if(missing(num_rect)){num_rect = sample(1:20, 1)}
  if(missing(max_rect_size)){max_rect_size = rbeta(1, 1, 2)}

  # initialize tibbles for line and rectangle data
  line_data = dplyr::tibble(x = numeric(),
                            y = numeric(),
                            xend = numeric(),
                            yend = numeric(),
                            alpha = numeric())

  rect_data = dplyr::tibble(xmin = numeric(),
                            xmax = numeric(),
                            ymin = numeric(),
                            ymax = numeric(),
                            alpha = numeric())

  # generate line data
  for(i in 1:num_lines){

    k_line = runif(1) * max_line_size
    x_new = runif(1)
    y_new = runif(1)
    if(rbinom(1, 1, p_v_line) == 1){
      xend_new = x_new
      yend_new = y_new + k_line
    } else {
      xend_new = x_new + k_line
      yend_new = y_new
    }
    alpha_new = runif(1)
    line_data = line_data %>% dplyr::bind_rows(
      dplyr::tibble(x = x_new, y = y_new,
                    xend = xend_new, yend = yend_new,
                    alpha = alpha_new)
    )

  }

  # generate rectangle data
  for(i in 1:num_rect){

    k_rect = runif(1) * max_rect_size
    xmin_new = runif(1)
    ymin_new = runif(1)
    xmax_new = xmin_new + k_rect
    ymax_new = ymin_new + k_rect
    alpha_new = runif(1)

    rect_data = rect_data %>% dplyr::bind_rows(
      dplyr::tibble(xmin = xmin_new, ymin = ymin_new,
                    xmax = xmax_new, ymax = ymax_new,
                    alpha = alpha_new)
    )

  }

  # drop lines and rectanges that extend out of bounds
  line_data = line_data %>%
    dplyr::filter(dplyr::across(.fns = ~ .x >= 0 & .x <= 1))

  rect_data = rect_data %>%
    dplyr::filter(dplyr::across(.fns = ~ .x >= 0 & .x <= 1))

  # randomly pick a color pallete & assign line and rectange colors
  data("palettes")
  palette = palettes[[sample(1:length(palettes), 1)]]
  line_color = sample(palette, 1)
  rect_color = sample(palette[!palette == line_color], 1)

  # generate plot
  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = rect_data,
      ggplot2::aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
                   alpha = alpha),
      fill = rect_color) +
    ggplot2::geom_segment(
      data = line_data,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, alpha = alpha),
      col = line_color) +
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
                   plot.background = ggplot2::element_blank()) %>%
    return()

}
