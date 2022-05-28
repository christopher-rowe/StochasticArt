#' Generate webs
#'
#' @param n an integer value, number of points
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'

generate_web = function(n=50, seed){

  # set & print seed
  if(missing(seed)){seed = sample(1:1e8, 1)}
  set.seed(seed)
  message(paste('Seed:', seed))

  point_data = dplyr::tibble(
    x = rbeta(n=n,sample(1:10,1), sample(1:10,1)),
    y = rbeta(n=n,sample(1:10,1), sample(1:10,1)),
    id = sample(1:round(n/10), n, replace = T)
  )

  line_data = point_data %>%
    dplyr::inner_join(
      point_data %>%
        dplyr::rename(xend=x,yend=y),
      by = 'id'
    ) %>%
    dplyr::select(-id) %>%
    dplyr::filter(!(x==xend))

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin=-0.2, xmax=1.2, ymin=-0.2, ymax=1.2), fill = 'black'
      ) +
    ggplot2::geom_curve(
      data=line_data, ggplot2::aes(x=x,y=y,xend=xend,yend=yend),
      alpha = 1, lwd = 0.1, curvature = runif(1), col = 'white'
      ) +
    xlim(-0.2, 1.2) +
    ylim(-0.2, 1.2) +
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


