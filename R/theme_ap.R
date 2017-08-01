#' ggplot AP theme
#'
#' This function is a theme function for use with ggplot2.
#' It tries to have a very tight composition, suitable for email or chat message
#' @include aptheme-package.R
#'
#' http://ggplot2.tidyverse.org/reference/theme.html

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


#' @export
theme_ap <- function(size = 12, family = "GoodComp-CondBook") {
  theme(
    # Text elements
    text = element_text(size, family = family),
    title = element_text(size = size + 4),
#    axis.text = element_blank(),
#    axis.title = element_blank(),
    # Question: x and y direction style difference? Should there be one? Maybe only for certain graphs (i.e. bar graphs)?
#    legend.text = element_blank(),
#    legend.title = element_blank(),
    # Question: Plot title, axis title, legend title- all same, or different?
#    plot.caption = element_blank(),
#    plot.title = element_blank(),
#    plot.subtitle = element_blank(),
#    strip.text = element_blank(),

    # Line elements
#    line = element_blank(),
#    axis.ticks = element_blank(),
#    axis.line = element_blank(),
#    panel.grid = element_blank(),
    panel.grid.major = element_line(color='#E7E2D8'),
    panel.grid.minor = element_line(color='#B6B6AB', linetype=1),

    # Rect elements
#    rect = element_blank(),
#    legend.key = element_blank(),
#    legend.background = element_blank(),
#    legend.box.background = element_blank(),
#    panel.background = element_blank(), # Drawn underneath the plot
#    panel.border = element_blank(), # Drawn on top of plot, covering tick marks and grid lines (fill = NA)
#    plot.background = element_blank(),
#    strip.background = element_blank(),

    # Other
    axis.ticks.length = unit(-2, "pt"),

    legend.margin = margin(-15, 0, -15, 0, "pt"),
#    legend.spacing = element_blank(),
#    legend.key.size = element_blank(),
    legend.text.align = 0, # 0: left, 1: right
    legend.title.align = 0,
    legend.position = "right", # none, left, right, bottom, top, or two-element numeric vector
    legend.direction = "vertical", # horizontal, vertical
    legend.justification = "center", # center or two-element numeric vector
    legend.box = "horizontal", # horizontal, vertical
    legend.box.just = "left", # top, bottom, left, right
    legend.box.margin = margin(0, 0, 0, 0, "pt"), # specified
    legend.box.spacing = unit(0, "pt"),

    panel.spacing = unit(0, "pt"),
    plot.margin = margin(5, 0, 0, 0, "pt"),
    strip.placement = "inside", # inside, outside
    aspect.ratio = 3/4
  )}


# TODO: Define custom color scales (discrete, continuous) for use with AP