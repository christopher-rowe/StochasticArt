#' Generate fabric of reality
#'
#' @param n_points an integer value, number of points
#' @param distribution 'beta', 'uniform', or 'normal'
#' @param seed a numeric value, the seed to initialize psuedo-randomness
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'
 generate_fabric = function(n_points, distribution = 'beta', seed){

   # check inputs
   assertthat::assert_that(
     distribution %in% c('beta', 'uniform', 'normal'),
     msg = "distribution must currently be either 'beta', 'uniform', or 'normal'"
     )

   # set & print seed
   if(missing(seed)){seed = sample(1:1e8, 1)}
   set.seed(seed)
   message(paste('Seed:', seed))

   # randomly choose parameter values if not specified
   if(missing(n_points)){n_points = sample(200:2000)}

   # set fixed parameters, to vary later
   n_layers = 4
   max_alpha = 1
   min_alpha = 0.4

   # generate a color pool
   color = rgb(
     red = sample(0:255, 1), blue = sample(0:255, 1), green = sample(0:255, 1),
     maxColorValue = 255
   )
   color_pool = colortools::tetradic(color, plot = F)

   # generate voronoi tile data
   tile_data = rep(list(create_voronoi(n_points, distribution)),
                   n_layers) %>%
     purrr::imap(
       function(x,y){
         alpha_x = runif(1)
         alpha_y = runif(1)
         x %>%
           dplyr::mutate(color = color_pool[{{y}}]) %>%
           dplyr::rowwise() %>%
           dplyr::mutate(dist = norm(c(alpha_x,alpha_y) - c(x_c, y_c), type = "2"),
                         dist = dist) %>%
           dplyr::ungroup() %>%
           dplyr::mutate(
             alpha = min_alpha + (max_alpha - ((dist-min(dist))/(max(dist)-min(dist)))*(max_alpha-min_alpha))
           )
       }
     )


   # generate line data
   line_data = tile_data[[1]] %>%
     dplyr::select(id, x, y) %>%
     dplyr::inner_join(
       tile_data[[1]] %>%
         dplyr::select(id, xend=x,yend=y),
       by = c('id')) %>%
     dplyr::filter(x!=xend)


   # generate plot
   p = ggplot2::ggplot()
   for(i in 1:length(tile_data)){
     p = p + ggplot2::geom_polygon(
       data=tile_data[[i]],
       ggplot2::aes(x=x,y=y, group = id, fill = color, alpha = alpha),
       col = 'white', size = 0.1
     )
   }

   p +
     ggplot2::geom_curve(
       data=line_data,
       ggplot2::aes(x=x,y=y,xend=xend,yend=yend),
       col = 'white', lwd=0.1, curvature=runif(1,0,0.7)
     ) +
     ggplot2::scale_fill_identity() +
     ggplot2::scale_alpha_identity() +
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

#' Generate random points & associated varanoi
#'
#' @param n_points an integer value, number of points
#' @param distribution 'beta', 'uniform', or 'normal'
#'
#' @export
#'
#' @return a random work of art as a ggplot object
#'
create_voronoi = function(n_points, distribution){

  # Calculate Voronoi Tesselation and tiles
  if(distribution=='uniform'){
    x=runif(n_points)
    y=runif(n_points)
  } else if(distribution=='beta'){
    x = rbeta(n_points, sample(1:3,1), sample(1:3,1))
    y = rbeta(n_points, sample(1:3,1), sample(1:3,1))
  } else if(distribution=='normal'){
    x = rnorm(n_points,0.5,runif(1,0.005,0.5))
    y = rnorm(n_points,0.5,runif(1,0.005,0.5))
  }

  tesselation = deldir::deldir(x, y, rw = c(0,1,0,1))

  tile_data = deldir::tile.list(tesselation) %>%
    purrr::imap(~ dplyr::tibble(x=.x$x,
                                y=.x$y,
                                id=paste0(.y, runif(1)))) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(x_c = sum(x)/length(x),
                  y_c = sum(y)/length(y)) %>%
    dplyr::ungroup()

  return(tile_data)

}


