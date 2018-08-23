# Tony Gojanovic
# Coursera "R Programming Capstone Project"
# Final project
# August 2018

#' Geom for plotting earthquake data over time
#' 
#' @details This function
#' @note 
#' 
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = 'identity', na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

bubble_subroutine <- function(data, params, size) {
  grid::circleGrob(r = data$size/25, gp = grid::gpar(col = data$color, fill = alpha(data$fill, data$alpha),lwd = 0.1))
}

GeomTimeline <- ggplot2::ggproto('GeomTimeline', ggplot2::Geom,
                                 required_aes = c('x'),
                                 default_aes = ggplot2::aes(y = NULL, color = 'red',
                                                            fill = 'black', size = 5, alpha = 0.3),
                                 draw_key = bubble_subroutine,
                                 draw_group = function(data, panel_params, coord) {
                                   coords <- coord$transform(data, panel_params)
                                   grid::circleGrob(
                                     coords$x,
                                     coords$y,
                                     r = coords$size / 225,
                                     gp = grid::gpar(
                                       col = coords$colour,
                                       fill = coords$fill,
                                       alpha = coords$alpha
                                       
                                     )
                                   )
                                 }
)