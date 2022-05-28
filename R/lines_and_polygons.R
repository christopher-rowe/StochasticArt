#' Generate lines and polygons
#'
#' @param num_shapes an integer value, number of points
#' @param background 'white' or 'black'
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'

generate_lines_and_polygons = function(num_shapes, background = 'black', seed){

  # set & print seed
  if(missing(seed)){seed = sample(1:1e8, 1)}
  set.seed(seed)
  message(paste('Seed:', seed))

  # randomly choose parameter values if not specified
  if(missing(num_shapes)){num_shapes = sample(1:4, 1)}

  # initialize arguments for each shape
  line_args = rep(T,num_shapes) %>%
    purrr::map(~ return(list(n_points = sample(50:300, 1),
                             n_edges = sample(4:10, 1))))

  # obtain color pool
  color_pool = get_random_color_pool(num_shapes)

  # generate edge data
  edge_data = line_args %>%
    purrr::map(~ do.call(generate_points_and_edges, .x)) %>%
    purrr::imap(function(x,y) x %>% dplyr::mutate(color = color_pool[{{y}}])) %>%
    purrr::reduce(dplyr::bind_rows)

  # plot
  ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin=-0.1, xmax=1.1, ymin=-0.1, ymax=1.1), fill = background) +
    ggplot2::geom_segment(
      data=edge_data,
      ggplot2::aes(x=x,y=y,xend=xend,yend=yend, alpha = alpha, col = color),
      lwd = 0.2) +
    ggplot2::xlim(-0.1, 1.1) +
    ggplot2::ylim(-0.1, 1.1) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_color_identity() +
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


#' Generate lines and polygons
#'
#' @param n_points an integer value, number of points
#' @param n_edges an integer value, number of edges for each point
#'
#'
#' @return a tibble of edges to support the generate_lines_and_polygons()
#' function
#'
generate_points_and_edges = function(n_points, n_edges){
  point_data = dplyr::tibble(
    x = rbeta(n=n,sample(1:10,1), sample(1:10,1)),
    y = rbeta(n=n,sample(1:10,1), sample(1:10,1)),
    id = 1
  )

  line_data = point_data %>%
    dplyr::inner_join(
      point_data %>%
        dplyr::rename(xend=x,yend=y),
      by = 'id'
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dist = norm(c(xend-x, yend-y), type = '2')) %>%
    dplyr::filter(!(x==xend)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::slice_min(dist, n = edges) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      alpha = norm(c(0.5,0.5), type = '2') - min(norm(c(0.5,0.5) - c(x,y), type='2'),
                                                 norm(c(0.5,0.5) - c(xend,yend), type='2'))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(alpha = (alpha - min(alpha))/(max(alpha)-min(alpha)))

  return(line_data)

}



