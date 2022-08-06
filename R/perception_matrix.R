#' Generate perception matrix
#'
#' @param source_num_sine_waves an integer value, the number of sine waves
#' that will be summed to generate the waves of the matrix (this is used as
#' input to the apply_random_sine_wave() function)
#' @param source_frequency a numeric value, controls maximum frequency of
#' waves of the matrix (this is used as input to apply_random_sine_wave()
#' function)
#' @param num_waves an integer value, the approximate number of waves that make
#' up the matrix in each directional axis
#' @param even_spacing a boolean value, set to TRUE to make the waves evenly
#' spaced
#' @param seed a numeric value, the seed to initialize psuedo-randomness
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'

generate_perception_matrix = function(source_num_sine_waves,  source_frequency,
                                      num_waves, even_spacing = TRUE, seed){

  # set & print seed
  if(missing(seed)){seed = sample(1:1e8, 1)}
  set.seed(seed)
  message(paste('Seed:', seed))

  # randomly choose parameter values if not specified
  if(missing(num_waves)){num_waves = sample(10:200, 1)}
  if(missing(source_num_sine_waves)){source_num_sine_waves = sample(1:10,1)}
  if(missing(source_frequency)){source_frequency = sample(3:10,1)}

  # number of points per wave (essentially resolution of wave)
  n = 2000

  # generate source wave
  source_wave = dplyr::tibble(x=seq(-0.5, 1.5, length.out = n)) %>%
    dplyr::mutate(
      y=apply_random_sine_wave(x, num_sine_waves = source_num_sine_waves,
                               frequency = source_frequency))


  # initialize wave spacing
  if(even_spacing){
    wave_vector = -num_waves:num_waves
  } else {
    wave_vector = sample(-num_waves:num_waves, num_waves)
  }

  # generate all parallel waves
  all_waves = dplyr::tibble()
  for(i in wave_vector){
    new_wave = source_wave %>% dplyr::mutate(y=y+(2/num_waves)*i, group = i)
    all_waves = all_waves %>% dplyr::bind_rows(new_wave)
  }

  # obtain rotated points & create arbitrary groups different from original groups
  r_waves = rotate_points(all_waves)

  # rotate newly rotated points by 90 degrees
  angle90 = if(rbinom(1,1,0.5)==1){pi/2} else{-pi/2}
  r90_waves = rotate_points(r_waves, angle = angle90) %>%
    dplyr::mutate(group = group + 10000)

  # combine waves
  plot_waves = r_waves %>%
    dplyr::bind_rows(r90_waves)

  # obtain center points for color and alpha gradients
  color_c = runif(2)
  alpha_c = runif(2)

  # filter to points in bounds and obtain color and alpha gradient values (
  # as distance to/from the center points generated above)
  plot_waves = plot_waves %>%
    dplyr::filter(x>=0 & x<=1 & y>=0 & y<=1) %>%
    dplyr::mutate(color = sqrt((x-color_c[1])^2 + (y-color_c[2])^2),
                  alpha = sqrt((x-alpha_c[1])^2 + (y-alpha_c[2])^2),
                  alpha = max(alpha) - alpha)

  # obtain colors for gradient
  key_color = get_random_color_pool(1)
  color_pool = colortools::complementary(key_color, plot = F)

  # create plot
  ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin=0, xmax=1, ymin=0, ymax=1), fill = 'black'
      ) +
    ggplot2::geom_path(
      data=plot_waves,
      ggplot2::aes(x=x,y=y, group = group, color = color, alpha = alpha)
      ) +
    ggplot2::coord_cartesian(xlim=c(0, 1), ylim=c(0, 1)) +
    ggplot2::scale_colour_gradient(low = color_pool[1], high = color_pool[2]) +
    ggplot2::xlim(0,1) +
    ggplot2::ylim(0,1) +
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
                   plot.background = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))

}
