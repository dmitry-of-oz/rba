#' RBA theme for ggplot2
#'
#' @param flipped Has the graph been flipped (90 degrees)?
#' @param legend Do you need a legend?
#' @param x_title Does the graph need a label for the x axis?
#' @param multipanel Is your graph a multipanel?
#'
#' @return A ggplot2 theme
#' @export
#'
#'
#' @examples
#' df <- tibble::tibble(group = c(rep("gdp", 5 ), rep("consumption",5) , rep("investment", 5 ), rep("exports", 5)),
#'                               x =  rep(1:5, 4) ,
#'                               value = 3*runif(length(x)))
#'
#' ggplot(df) + geom_line(aes(x = x, y = value, colour = group)) + rba_theme()
#' ggplot(df) + geom_line(aes(x = x, y = value, colour = group)) + rba_theme(legend = T)
#' ggplot(df) + geom_line(aes(x = x, y = value, colour = group)) + rba_theme(legend = T, x_title = T)
#' ggplot(df) + geom_line(aes(x = x, y = value )) + facet_wrap(~ group) + rba_theme(facet = T)
#'
rba_theme <- function(flipped = F, legend = F,  x_title = F, facet = F) {
  thm <- ggplot2::theme(
    #Titles and captions
    plot.title = element_text(size = rel(2.5), face = "bold", vjust = -0.5, hjust = 0.5, margin = unit(c(0, 0, 0.25, 0), "cm")),
    plot.subtitle = element_text(size = rel(1.6), hjust = 0.5),
    plot.caption = element_text(size = rel(1), hjust = 0),
    
    #Border and background
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.background = element_blank(),
    
    #Faceting
    panel.spacing = unit(0, "cm"),
    strip.background = element_blank(),
    
    #Legends
    legend.key = element_blank(),
    legend.background = element_blank()
  )
  
  if (!legend) {
    thm <- thm + ggplot2::theme(
      legend.position = "none"
    )
  } else {
    thm <- thm + ggplot2::theme(
      legend.position = "bottom",
      legend.text = element_text(size = rel(1.2)),
      legend.margin = margin(t = -0.3, unit = "cm"),
      legend.title = element_blank(),
      legend.key = element_rect(fill = "white", colour = "white"),
    )
  }
  
  if (flipped) {
    thm <- thm + ggplot2::theme(
      #Axes
      axis.title.x.top = element_blank(),
      axis.title.x = element_text(size = 16, angle = 0, vjust = 1, margin = unit(c(0.2, -0, 0, 0), "cm")),
      axis.text.x = element_text(margin=unit(c(0.4,0.5,0.1,0.5), "cm"), size = 16),
      axis.text.x.top = element_blank(),
      
      axis.text.y = element_text(margin=unit(c(0.5,0.4,0.5,0.0), "cm"), size = 16),
      
      #Gridlines
      panel.grid.major.x = element_line(colour = "gray", size = 0.5),
      panel.grid.major.y = element_blank()
    )
  } else {
    thm <- thm + ggplot2::theme(
      #Axes
      axis.title.x = element_blank(),
      axis.text.x = element_text(margin=unit(c(0.4,0.5,0.1,0.5), "cm"), size = 16),
      axis.title.y.right = element_text(size = 16, angle = 0, vjust = 1, margin = unit(c(0, 0, 0, -4), "mm")),
      axis.title.y = element_text(size = 16, angle = 0, vjust = 1, margin = unit(c(0, -4, 0, 0), "mm")),
      axis.text.y = element_text(margin=unit(c(0.5,0.4,0.5,0.0), "cm"), size = 16),
      axis.text.y.right = element_text(margin=unit(c(0.5,0.0,0.5,0.4), "cm"), size = 16),
      axis.text = element_text(angle = 0, colour="black"),
      
      #Gridlines
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      panel.grid.major.y = element_line(colour = "gray", size = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      
      #ticks
      axis.ticks.y = element_blank(),
      axis.ticks.length=unit(-0.17, "cm")
    )
  }
  if (x_title) {
    thm <- thm + ggplot2::theme(axis.title.x = element_text(size = 16, angle = 0, vjust = 1, margin = unit(c(0.2, -0, 0, 0), "cm")))
  }
  
  if (facet) {
    thm <-  thm +  ggplot2::theme(strip.background = element_blank(),
                                  strip.text =element_text(size = rel(1.2)),panel.spacing = unit(0, "lines"))
  }
  
  
  thm
}


#' Duplicate axis and set y axis settings
#'
#' This is a small wrapper around `ggplot2`'s [ggplot2::scale_y_continuous()]. It sets the expand limits to zero and duplicates the axis if `dup` is `TRUE`.
#'
#' @param dup Duplicate the axis?
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' df <- tibble::tibble(group = c(rep("gdp", 5 ), rep("consumption",5) , rep("investment", 5 ), rep("exports", 5)),
#'                               x =  rep(1:5, 4) ,
#'                               value = 3*runif(length(x)))
#'
#' ggplot(df) + geom_line(aes(x = x, y = value, colour = group)) + rba_theme() + rba_syc()
#' ggplot(df) + geom_line(aes(x = x, y = value, colour = group)) + rba_theme() + rba_syc(dup = F)
#'
#'
#'
rba_syc <- function(dup = T) {
  if (dup == T) return(ggplot2::scale_y_continuous(expand = c(0, 0),
                                                   sec.axis = dup_axis()))
  ggplot2::scale_y_continuous(expand = c(0, 0))
}
#' Set y axis limits
#'
#' Best combined with [rba_syc()] to avoid gaps at the top and bottom of your graph.
#'
#' @param ymin Y axis minimum
#' @param ymax Y axis maximum
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' df <- tibble::tibble(group = c(rep("gdp", 5 ), rep("consumption",5) , rep("investment", 5 ), rep("exports", 5)),
#'                               x =  rep(1:5, 4) ,
#'                               value = 3*runif(length(x)))
#'
#' ggplot(df) + geom_line(aes(x = x, y = value, colour = group)) + rba_theme() + rba_syc() + rba_ylim(0, 4)
rba_ylim <- function(ymin, ymax) {
  ggplot2::expand_limits(y = c(ymin, ymax - 0.0001))
}



#' Colour legend options
#' Expand lines in legend to boxes and/or change columns/rows of legend
#'
#' @param size (default = T) logical or numeric. If logical, T will change legend colour aesthetic overrise to size = 6. If numeric that will be the
#' size of aesthetic override
#' #' @param ncol (default = NULL) number of columns legend will have
#' @param nrow (default = NULL) number of rows legend will have
#' @export
#'
#' @examples
#' df <- tibble::tibble(group = c(rep("gdp", 5 ), rep("consumption",5) , rep("investment", 5 ), rep("exports", 5)),
#'                               x =  rep(1:5, 4) ,
#'                               value = 3*runif(length(x)))
#' ggplot(df) + geom_line(aes(x = x, y = value, colour = group)) + rba_legend_colour(ncol = 2)
#' ggplot(df) + geom_line(aes(x = x, y = value, colour = group)) + rba_legend_colour(expand = F, ncol = 2)

rba_legend_colour <- function(expand = T, ncol = NULL, nrow = NULL){
  
  if (is.logical(expand) && expand){
    ggplot2::guides(colour = guide_legend(override.aes= list(size = 6), ncol = ncol, nrow = nrow ))
  } else if(is.numeric(expand) && expand > 0){
    ggplot2::guides(colour = guide_legend(override.aes= list(size = expand), ncol = ncol, nrow = nrow))
  } else if(is.logical(expand) && !expand){
    ggplot2::guides(colour = guide_legend(ncol = ncol, nrow = nrow))
  }
  
}


#' Fill legend options
#' Expand lines in legend to boxes and/or change columns/rows of legend
#'
#' #' @param ncol (default = NULL) number of columns legend will have
#' @param nrow (default = NULL) number of rows legend will have
#' @export
#'
#' @examples
#' fill_data <- tibble::tibble(group = rep(c("a", "b", "c"), 5),
#'                         type =  rep(c("math", "english", "science", "history", "economics"), 3) ,
#'                         value = 100*runif(length(type)))
#' ggplot(fill_data) + geom_col(aes( x= group, y = value, fill = type)) + rba_legend_fill( ncol = 2)

rba_legend_fill <- function( ncol = NULL, nrow = NULL){
  
  ggplot2::guides(fill = guide_legend(ncol = ncol, nrow = nrow))
  
}


#' Move y-axis title around
#'
#' @param y_left_margin Distance of left y-axis title from plot panel. The more negative the closer to the plot panel. Default = -4
#' @param y_right_margin Distance of right y-axis title from plot panel. The more negative the closer to the plot panel. Default = -4
#'
#' @return theme with modified y-axis title margins
#' @export
#' @examples
#' ggplot(mtcars) + geom_line(aes( x= mpg, y = disp, colour = factor(am))) + ylab("disp") + rba_title_y_position(y_left_margin = -6, y_right_margin = -6 )
rba_title_y_position <- function(y_left_margin = -4, y_right_margin = -4){
  .Deprecated("rba_ylab_position")
  ggplot2::theme( axis.title.y.right = element_text( margin = unit(c(0, 0, 0, y_right_margin) , "mm")),
                  axis.title.y = element_text( margin = unit(c(0, y_left_margin , 0, 0), "mm")))
}


#' Move y-axis title around
#'
#' @param x Distance of left and right y-axis title from plot panel. The more negative the closer to the plot panel.
#' @param left_margin Distance of left y-axis title from plot panel. Only applied if `x` is missing. Default = -4
#' @param right_margin Distance of right y-axis title from plot panel. Only applied if `x` is missing. Default = -4
#'
#' @return theme with modified y-axis title margins
#' @export
#' @examples
#' ggplot(mtcars) + geom_line(aes( x= mpg, y = disp, colour = factor(am))) + ylab("disp") + rba_ylab_position(-6)
#' ggplot(mtcars) + geom_line(aes( x= mpg, y = disp, colour = factor(am))) + ylab("disp") + rba_ylab_position(left_margin = -5, right_margin = 3)
rba_ylab_position <- function(x, left_margin = -4, right_margin = -4){
  
  if (rlang::is_missing(x)){
    ggplot2::theme( axis.title.y.right = element_text( margin = unit(c(0, 0, 0, right_margin) , "mm")),
                    axis.title.y = element_text( margin = unit(c(0, left_margin , 0, 0), "mm")))
  } else{
    ggplot2::theme( axis.title.y.right = element_text( margin = unit(c(0, 0, 0, x) , "mm")),
                    axis.title.y = element_text( margin = unit(c(0, x , 0, 0), "mm")))
  }
  
}

#' Change axis text size
#' Change text size of x- and y-axis, including axis titles
#' @param size size of axis text; default = 16
#' @param x_title Do you have an x-axis title; default = F
#' @return theme with modified text size
#' @export
#' @examples
#' ggplot(mtcars) + geom_line(aes( x= mpg, y = disp, colour = factor(am))) + ylab("disp") + rba_axis_text_size(size = 14)

rba_axis_text_size <- function(size = 16, x_title = F){
  out <- ggplot2::theme(
    axis.text.x = element_text( size = size),
    axis.title.y.right = element_text(size = size),
    axis.title.y = element_text(size = size),
    axis.text.y = element_text( size = size),
    axis.text.y.right = element_text(size = size))
  
  if (x_title) out <- out + ggplot2::theme(axis.title.x = element_text(size = size))
  return(out)
}

#' Rotate x-axis text
#' Rotates x-axis text on ggplot
#'
#' @param angle (default = 50) number of degrees you want the rotation
#' @param hjust (default = 1) horizontal justification
#'
#' @export
#'
#' @examples
#' ggplot(iris) + geom_point(aes(x = Species, y = Sepal.Length)) + rba_rotate_x_text()
#'
rba_rotate_x_text <- function(angle = 50, hjust = 1){
  out <-  ggplot2::theme(axis.text.x = element_text(angle = angle, hjust = hjust))
  return(out)
}


#' Change panel text size
#' Change relative text size of title, subtitle or caption
#'
#' @param title relative size of title; default = NA
#' @param subtitle relative size of subtitle; default = NA
#' @param caption relative size of caption; default = NA
#' @param legend relative size of legend; default = NA
#'
#' @return theme with modified size of title, subtitle and/or caption
#' @export
#' @examples
#' ggplot(mtcars) + geom_line(aes( x= mpg, y = disp, colour = factor(am))) +
#' labs(title = "Some Title") + rba_panel_text_size(title = 2)
#'
#' ggplot(mtcars) + geom_line(aes( x= mpg, y = disp, colour = factor(am))) +
#' labs(title = "Some Title", caption = "Sources: RBA, R") + rba_panel_text_size(title = 2, caption = 1.3)
#'
#' ggplot(mtcars) + geom_line(aes( x= mpg, y = disp, colour = factor(am))) +
#' labs(title = "Some Title", subtitle = "A subtitle",  caption = "Sources: RBA, R") + rba_panel_text_size(title = 2, subtitle = 1.65, caption = 1.3)
rba_panel_text_size <- function(title = NA, subtitle = NA, caption = NA, legend = NA, facet = NA){
  
  out <- theme()
  
  if (!is.na(title)) out <- out + ggplot2::theme(plot.title = element_text(size = rel(title)))
  
  if (!is.na(subtitle)) out <- out + ggplot2::theme( plot.subtitle = element_text(size = rel(subtitle)))
  
  if (!is.na(caption)) out <- out + ggplot2::theme( plot.caption = element_text(size = rel(caption)))
  
  if (!is.na(legend)) out <- out + ggplot2::theme(  legend.text = element_text(size = rel(legend)))
  
  if (!is.na(facet)) out <- out + ggplot2::theme(  strip.text = element_text(size = rel(facet)))
  
  return(out)
}


#' Add x-ticks based on date frequency
#' @description Takes a ggplot and adds more x-tick marks but retains the same number x axis labels.
#'
#' @param p ggplot2 object.
#' @param date_breaks (default = "1 year") A string giving the distance between breaks like "2 weeks", or "10 years".
#' These breaks must include the date breaks of the given plot
#'
#' @return ggplot2 object with more x-ticks
#' @export
#'
#' @note Only works if x-axis is a date
#' @examples
#' \dontrun{
#' df <- data.frame(date = seq.Date(from = as.Date("1950-01-01"), to = as.Date("2020-01-01"), by = "1 year"), y = runif(71))
#' p <- ggplot(df, aes(x=date, y=y))+
#'   rba_theme()+
#'   rba_syc() +
#'   rba_ylim(0, 1) +
#'   geom_line() +
#'   scale_x_date(date_labels = "%Y")
#'
#' rba_xticks_date(p, date_breaks = "1 year")
#'
#'
#'
#' p <- ggplot(df, aes(x=date, y=y))+
#'  rba_theme()+
#'  rba_syc() +
#'  rba_ylim(0, 1) +
#'  geom_line() +
#'  scale_x_date(breaks = as.Date(c("1950-01-01", "1970-01-01", "1990-01-01", "2010-01-01")) , date_labels = "%YM%m" )
#'
#' rba_xticks_date(p, date_breaks = "1 year")
#'
#' }
#'
rba_xticks_date <- function(p, date_breaks	 = "1 year"){
  
  if (!is.ggplot(p)) stop(glue::glue("Argument `p` must be a ggplot2 object."))
  
  # original x breaks and labels
  p_build <- ggplot2::ggplot_build(p)
  x_breaks_orig <- p_build$layout$panel_params[[1]]$x$get_breaks()
  x_breaks <- x_breaks_orig[!is.na(x_breaks_orig)]
  
  x_labels <- p_build$layout$panel_params[[1]]$x$get_labels()
  x_labels <- x_labels[!is.na(x_breaks_orig)]
  
  
  # Create minimum and maximum dates that are way off the chart
  min_date <- p_build$layout$panel_scales_x[[1]]$range$range %>% min %>% as.Date(origin = "1970-01-01")  %>% seq.Date(., length.out = 3, by = paste0("-", date_breaks)) %>% min()
  max_date <-  p_build$layout$panel_scales_x[[1]]$range$range %>% max %>% as.Date(origin = "1970-01-01") %>% seq.Date(., length.out = 3, by = date_breaks) %>% max()
  first_x_break <- as.Date(x_breaks[1], origin = "1970-01-01") # first proper x-break which will be used as an anchor throughout - this ensures the original labels are used.
  
  # Create new breaks based on dates crated
  new_breaks_forward <- seq.Date(from = first_x_break, to = max_date, by = date_breaks)
  new_breaks_backward <- seq.Date(from = first_x_break, to = min_date, by = paste0("-" , date_breaks) ) %>% sort()
  new_breaks <- c(new_breaks_backward, new_breaks_forward[2:length(new_breaks_forward)])
  
  # add check if original labels are in new labels
  if (!all(as.Date(x_breaks, origin = "1970-01-01") %in% new_breaks)) stop("Selected `date_breaks` frequency does not contain the original x-labels. Try another frequency.")
  
  # Make final label
  final_label <- as.character(new_breaks)
  final_label[!final_label %in% as.character(as.Date(x_breaks, origin = "1970-01-01")) ] <- ""
  final_label[final_label != ""] <- x_labels
  
  # return graph with specified breaks and labels
  return(suppressMessages(p + ggplot2::scale_x_date(breaks = new_breaks, labels = final_label)))
  
}

#' Add x-ticks based on numeric interval
#' @description Takes a ggplot and adds more x-tick marks but retains the same number x axis labels.
#'
#' @param p ggplot2 object.
#' @param breaks (default = 1) A numeric giving the distance between breaks e.g. 1, 5, 10.
#' @return ggplot2 object with more x-ticks
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = seq(from = 0, to = 100, by = 10), y = runif(11))
#' p <- ggplot(df, aes(x = x, y = y))+
#'   rba_theme()+
#'   rba_syc() +
#'   rba_ylim(0, 1) +
#'   geom_line()
#'
#' rba_xticks_continous(p, breaks = 1)
#'
#' }
#'
rba_xticks_continuous <- function(p, breaks	= 1){
  
  if (!is.ggplot(p)) stop(glue::glue("Argument `p` must be a ggplot2 object."))
  
  
  # original x breaks and labels
  p_build <- ggplot2::ggplot_build(p)
  x_breaks_orig <- p_build$layout$panel_params[[1]]$x$get_breaks()
  x_breaks <- x_breaks_orig[!is.na(x_breaks_orig)]
  
  x_labels <- p_build$layout$panel_params[[1]]$x$get_labels()
  x_labels <- x_labels[!is.na(x_breaks_orig)]
  
  
  # Create minimum and maximum dates that are way off the chart
  min_point <- p_build$layout$panel_scales_x[[1]]$range$range %>% min %>% seq(., length.out = 1000, by = -breaks) %>% min()
  max_point <-  p_build$layout$panel_scales_x[[1]]$range$range %>% max %>% seq(., length.out = 1000, by = breaks) %>% max()
  first_x_break <- x_breaks[1] # first proper x-break which will be used as an anchor throughout - this ensures the original labels are used.
  
  # Create new breaks based on dates crated
  new_breaks_forward <- seq(from = first_x_break, to = max_point, by = breaks)
  new_breaks_backward <- seq(from = first_x_break, to = min_point, by = -breaks ) %>% sort()
  new_breaks <- c(new_breaks_backward, new_breaks_forward[2:length(new_breaks_forward)])
  
  # add check if original labels are in new labels
  if (!all(x_breaks %in% new_breaks)) stop("Selected `breaks` intervals do not contain the original x-labels. Try another frequency.")
  
  # Make final label
  final_label <- new_breaks
  final_label[!final_label %in% x_breaks ] <- ""
  final_label[final_label != ""] <- x_labels
  
  # return graph with specified breaks and labels
  return(suppressMessages(p + ggplot2::scale_x_continuous(breaks = new_breaks, labels = final_label)))
  
}

#' Create RBA-themed ggplot2 graph
#'
#' @param p A `ggplot2` plot object
#' @param ymin Y axis minimum, passed to [rba_ylim()]
#' @param ymax Y axis maximum, passed to [rba_ylim()]
#' @param dup Duplicate the axis? Passed to [rba_syc()]
#' @param ... Parameters to pass to [rba_theme()]
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(group = c(rep("gdp", 5 ), rep("consumption",5) , rep("investment", 5 ), rep("exports", 5)),
#'                               x =  rep(1:5, 4) ,
#'                               value = 3*runif(length(x)))
#'
#' p <- ggplot(df) + geom_line(aes(x = x, y = value, colour = group))
#' rba_graph(p, 0, 4, dup = T)
#'
rba_graph <- function(p, ymin, ymax, dup = T, ...) {
  p + rba_syc(dup = dup) + rba_ylim(ymin, ymax) + rba_theme(...)
}


#' RBA save ggplot
#' @description Will save a file with width = 189mm, height = 161mm, and dpi set to 500
#' @param ... parameters parsed through ggsave
#'
#' @import ggplot2
#' @export
ggsave_rba <- function(...) {
  ggplot2::ggsave(..., units = "mm", width = 189, height = 161, dpi = 500)
  message("Saving 189mm x 161mm in image")
}



#' Save plot to clipboard
#'
#' @param plot your plot to copy to clipboard
#'
#' @return invisibly return your plot
#' @export
#'
#' @examples
#' p <- ggplot(mtcars, aes(x = cyl, y = disp)) + geom_point()
#' clipboard_plot(p)
clipboard_plot <- function(plot){
  win.metafile(width = 4, height = 3.407408)
  print(plot)
  dev.off()
  return(invisible(plot))
}


# Modify some ggplot2 defaults - will only be used if there is only need for one colour.
ggplot2::update_geom_defaults("line", list(colour = rba[["default1"]], size = 0.75))
ggplot2::update_geom_defaults("point", list(colour = rba[["default2"]], size = 1.3))
ggplot2::update_geom_defaults("bar", list(fill = rba[["default1"]]))
ggplot2::update_geom_defaults("col", list(fill = rba[["default1"]]))