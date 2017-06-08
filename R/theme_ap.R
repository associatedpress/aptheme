#' ggplot AP theme
#'
#' This function is a theme function for use with ggplot2.
#' It tries to have a very tight composition, suitable for email or chat message

# primitives
# element_text:
#   family = font family
#   face = font face
#   colour = font color
#   size = font size (pts)
#   hjust = horiz just .5
#   vjust = vert just .5
#   angle = text angle 0
#   lineheight = line height 1.1

# element_line
#   color = line color "black"
#   size = line thickness .5
#   linetype = type of line 1

# element_rect
#   fill = fill color NA
#   colour = border color "black"
#   size = thickness of border line .5
#   linetype = type of border line  1



theme_ap <- function() {
  theme(
    #  axis.line = element_blank(),
    #  axis.text = element_text(),
    #  axis.text.x = element_text(), # inheritance
    #  axis.text.y = element_text(),
    #  axis.title = element_text(),
    #  axis.title.x = element_text(), # inheritance
    #  axis.title.y = element_text(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(-2, "pt"),
    #  panel.background = element_rect(fill = '#FFF9EE', linetype=0),
    panel.background = element_blank(),
    panel.grid.major = element_line(color='grey', linetype=1),
    #  panel.grid.major.x = element_line(),
    #  panel.grid.major.y = element_line(),
    panel.grid.minor = element_line(color='lightgrey'),
    #  panel.grid.minor.x = element_line(),
    #  panel.grid.minor.y = element_line(),
    #  plot.margin = unit(c(0, 0, 0, 0), "cm"),
    plot.margin = margin(),
    #  plot.background = element_blank(),
    plot.title = element_text(face="bold"),
    #  legend.box.background = element_rect(),
    legend.key = element_blank(),
    legend.position = "top",
    legend.margin = margin(-15, 0, -15, 0),
    aspect.ratio = 9/16
  )}


# TODO define a custom viz function that makes a scatterplot, accepts labels as a variable, and will automatically label the outliers

