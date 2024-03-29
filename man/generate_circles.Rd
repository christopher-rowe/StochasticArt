% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circles.R
\name{generate_circles}
\alias{generate_circles}
\title{Create inner concentric circles}
\usage{
generate_circles(
  num_circles,
  global_max_r,
  circle_gap_p,
  max_alpha,
  min_concentric_circles = 1,
  max_concentric_circles = 20,
  equal_concentric_spacing,
  num_colors,
  frame = F,
  seed
)
}
\arguments{
\item{num_circles}{an integer value, the number of circles in the artwork}

\item{global_max_r}{a numeric value, the maximum allowable radius of each circle}

\item{circle_gap_p}{a numeric value, higher values increase the gap between
each circle; technically, the radius of a new circle will have a max radius of
(1 - circle_gap_p) multiplied by the distance from the center of the new circle
to the edge of the nearest other circle}

\item{max_alpha}{a numeric value between 0-1, the maximum alpha value for each
circle}

\item{min_concentric_circles}{an integer value, the minimum number of concentric
circles within each circle}

\item{max_concentric_circles}{an integer value, the maximum number of concentric
circles within each circle}

\item{equal_concentric_spacing}{a boolean value, set to true to make concentric
circles evenly spaced}

\item{num_colors}{an integer value, the number of colors in the artwork; each
circle is randomly assigned a color from a pool of this size}

\item{frame}{a boolean value, set to true to include a circular frame around the
artwork}

\item{seed}{a numeric value, the seed to initialize psuedo-randomness}
}
\value{
a random work of art as a ggplot object
}
\description{
Create inner concentric circles
}
