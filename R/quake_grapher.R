# Tony Gojanovic
# Coursera "R Programming Capstone Project"
# Final project
# August 2018

#' Geom for plotting earthquake data as a time series.
#' 
#' @description This function provides a time series interpretation of earthquakes over time expressed as a bubble plot.
#' @note Countries and data ranges are modified using dplyr prior to using the geom for time series plotting. 
#' For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' @export 
#' @import ggplot2
#' @import ggplot2 grid
#' @importFrom dplyr mutate 
#' @export
#' @references 
#' @examples
#' \dontrun{
#' 
#' library(dplyr)
#' library(ggplot2)
#' 
#' any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% 
#' filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
#' ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) 
#' 
#' # Additional graph metadata can be added using standard ggplot syntax
#' # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' 
#' library(dplyr)
#' library(ggplot2)
#' 
#' any_name_df<-eq_clean_data(results)
#' 
#' p1<-any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% 
#' filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
#' ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY))
#'    
#' p1<-p1 + ggtitle("NOAA Significant Earthquake Data")
#' 
#' p1<-p1 + ylab("Country")+xlab("Date") + scale_size_continuous(name = 'Richter')  
#' 
#' p1<-p1  +scale_color_continuous(name = 'Mortality (deaths)') +theme(axis.text.x=element_text(angle=90,hjust=3))
#' 
#' p1
#' 
#' }
#' 
#' @details geom_timeline provides a time series interpretation on filtered NOAA Significant Earthquake data.
#' @note Aesthetic considerations for the number of countries and data ranges should be taken into consideration as to not plot too many data points rendering the graph unreadable.
#' Use of dplyr and piping to filter the data set is suggested. 
#' Additional graphical elements can be added using standard ggplot syntax as shown in the example.
#' 
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = 'identity', na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,...) {
  ggplot2::layer(
    geom = geomtimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# The following is a subroutine used for generating data bubbles on the timeline.

bubble_subroutine <- function(data, params, size) {
  grid::circleGrob(r = data$size/25, gp = grid::gpar(col = data$color, fill = alpha(data$fill, data$alpha),lwd = 0.1))
}

geomtimeline <- ggplot2::ggproto('geomtimeline', ggplot2::Geom,
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


#' Geom for labeling the time series plot.
#' 
#' @description This function provides label meta data for the time series plot of earthquakes.
#' @note For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' @importFrom dplyr mutate 
#' @import ggplot2
#' @references The following are usefule references:
#' \url{https://ggplot2.tidyverse.org/reference/geom_text.html}
#' \url{https://ggplot2.tidyverse.org/articles/ggplot2-specs.html}
#' 
#' @examples 
#' \dontrun{
#'
#' # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' 
#' library(dplyr)
#' library(ggplot2)
#' 
#' any_name_df<-eq_clean_data(results)
#' 
#' p1<- any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA" | COUNTRY=="GREECE") %>% 
#' filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
#' ggplot()
#' 
#' p1<-p1 + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) 
#' 
#' p1<-p1 + geom_timeline_label(aes(x = datevalue, y = COUNTRY, magnitude = EQ_PRIMARY,label = location_name, max_labels= 5))
#' 
#' p1<-p1 + ggtitle("NOAA Signficant Earthquakes Data with High Profile Events")
#' 
#' p1
#' 
#' }
geom_timeline_label <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = auto_labeler,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

auto_labeler <-
  ggplot2::ggproto(
    "auto_labeler",
    ggplot2::Geom,required_aes = c('x', 'label', 'magnitude','max_labels'),
    
    default_aes = ggplot2::aes(
      max_labels = 3,y = 0,color = 'red',size = 0.25, linetype = 11,alpha = 0.5
    ),
    
    draw_key = bubble_subroutine,
    
    draw_panel = function(data, panel_scales, coord) {
      max_labels <- data$max_labels[1]
      
      data <- data %>% dplyr::group_by(group) %>%dplyr::top_n(max_labels, magnitude)
      
      data$xend <- data$x
      data$yend <- data$y + 0.25
      panel <- ggplot2::GeomSegment$draw_panel(unique(data), panel_scales, coord)
      
      # Provisions for text formatting.
      
      data$y <- data$yend 
      data$angle <- 60
      data$fontface <- 'bold'
      data$family <- 'sans'
      data$size <- 3.5
      data$fill<-'white'
      data$colour <- 'red'
      data$lineheight <- 4
      data$hjust <- 'left'
      data$vjust <- 'top'
     
      meta_data <- ggplot2::GeomText$draw_panel(unique(data), panel_scales, coord)
      
      ggplot2:::ggname('geom_timeline_label', grid::grobTree(panel, meta_data))
    }
  )


# Tony Gojanovic
# Coursera "R Programming Capstone Project"
# Final project
# August 2018

#' Geom for plotting earthquake data as a time series.
#' 
#' @description This function provides a time series interpretation of earthquakes over time expressed as a bubble plot.
#' @note Countries and data ranges are modified using dplyr prior to using the geom for time series plotting. 
#' For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' @export 
#' @import ggplot2
#' @import ggplot2 grid
#' @importFrom dplyr mutate 
#' @export
#' @references 
#' @examples
#' \dontrun{
#' 
#' library(dplyr)
#' library(ggplot2)
#' 
#' any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% 
#' filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
#' ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) 
#' 
#' # Additional graph metadata can be added using standard ggplot syntax
#' # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' 
#' library(dplyr)
#' library(ggplot2)
#' 
#' any_name_df<-eq_clean_data(results)
#' 
#' p1<-any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA") %>% 
#' filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
#' ggplot() + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY))
#'    
#' p1<-p1 + ggtitle("NOAA Significant Earthquake Data")
#' 
#' p1<-p1 + ylab("Country")+xlab("Date") + scale_size_continuous(name = 'Richter')  
#' 
#' p1<-p1  +scale_color_continuous(name = 'Mortality (deaths)') +theme(axis.text.x=element_text(angle=90,hjust=3))
#' 
#' p1
#' 
#' }
#' 
#' @details geom_timeline provides a time series interpretation on filtered NOAA Significant Earthquake data.
#' @note Aesthetic considerations for the number of countries and data ranges should be taken into consideration as to not plot too many data points rendering the graph unreadable.
#' Use of dplyr and piping to filter the data set is suggested. 
#' Additional graphical elements can be added using standard ggplot syntax as shown in the example.
#' 
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = 'identity', na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,...) {
  ggplot2::layer(
    geom = geomtimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# The following is a subroutine used for generating data bubbles on the timeline.

bubble_subroutine <- function(data, params, size) {
  grid::circleGrob(r = data$size/25, gp = grid::gpar(col = data$color, fill = alpha(data$fill, data$alpha),lwd = 0.1))
}

geomtimeline <- ggplot2::ggproto('geomtimeline', ggplot2::Geom,
                                 required_aes = c('x'),
                                 default_aes = ggplot2::aes(y = NULL, color = 'red',
                                                            fill = 'black', size = 5, alpha = 0.3),
                                 draw_key = bubble_subroutine,
                                 draw_group = function(data, panel_params, coord) {
                                   coords <- coord$transform(data, panel_params)
                                   grid::circleGrob(coords$x,coords$y,r= coords$size / 225,gp = grid::gpar(alpha = coords$alpha,fill = coords$fill,col = coords$colour
                                     )
                                   )
                                 }
)


#' Geom for labeling the time series plot.
#' 
#' @description This function provides label meta data for the time series plot of earthquakes.
#' @note For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' @importFrom dplyr mutate 
#' @import ggplot2
#' @references The following are usefule references:
#' \url{https://ggplot2.tidyverse.org/reference/geom_text.html}
#' \url{https://ggplot2.tidyverse.org/articles/ggplot2-specs.html}
#' 
#' @examples 
#' \dontrun{
#'
#' # For BC date ranges, use a one sided inequality e.g. datevalue < '0000-01-01'
#' 
#' library(dplyr)
#' library(ggplot2)
#' 
#' any_name_df<-eq_clean_data(results)
#' 
#' p1<- any_name_df %>% filter(COUNTRY =="CHINA" | COUNTRY=="USA" | COUNTRY=="GREECE") %>% 
#' filter(datevalue > '1900-01-01', datevalue < '1950-01-01') %>% 
#' ggplot()
#' 
#' p1<-p1 + geom_timeline(aes(x = datevalue, y = COUNTRY, color = TOTAL_DEATHS, size = EQ_PRIMARY)) 
#' 
#' p1<-p1 + geom_timeline_label(aes(x = datevalue, y = COUNTRY, magnitude = EQ_PRIMARY,label = location_name, max_labels= 5))
#' 
#' p1<-p1 + ggtitle("NOAA Signficant Earthquakes Data with High Profile Events")
#' 
#' p1
#' 
#' }
geom_timeline_label <-
  function(data = NULL,na.rm = FALSE,show.legend = NA,mapping = NULL, stat = "identity", position = "identity", inherit.aes = TRUE,...) 
    {
    ggplot2::layer(
      geom = auto_labeler,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

auto_labeler <-
  ggplot2::ggproto(
    "auto_labeler",
    ggplot2::Geom,required_aes = c('x', 'label', 'magnitude','max_labels'),
    
    default_aes = ggplot2::aes(
      max_labels = 3,y = 0,color = 'red',size = 0.25, linetype = 11,alpha = 0.5
    ),
    
    draw_key = bubble_subroutine,
    
    draw_panel = function(data, panel_scales, coord) {
      max_labels <- data$max_labels[1]
      
      data <- data %>% dplyr::group_by(group) %>%dplyr::top_n(max_labels, magnitude)
      
      data$xend <- data$x
      data$yend <- data$y + 0.25
      panel <- ggplot2::GeomSegment$draw_panel(unique(data), panel_scales, coord)
      
      # Provisions for text formatting for plot readability.
      
      data$y <- data$yend 
      data$angle <- 60
      data$size <- 3.5
      data$fill<-'white'
      data$colour <- 'red'
      data$lineheight <- 4
      data$fontface <- 'bold'
      data$family <- 'sans'
      data$vjust <- 'top'
      data$hjust <- 'left'
      meta_data <- ggplot2::GeomText$draw_panel(unique(data), panel_scales, coord)
      ggplot2:::ggname('geom_timeline_label', grid::grobTree(panel, meta_data))
    }
  )

