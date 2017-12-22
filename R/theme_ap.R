#' ggplot AP theme
#'
#' This function is a theme function for use with ggplot2.
#' It tries to have a very tight composition, suitable for email or chat message
#'
#' http://ggplot2.tidyverse.org/reference/theme.html
#' @import ggplot2

# Notes on primitives:
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
    axis.text = element_text(size = size - 4),
    axis.title = element_text(size = size),
#    legend.text = element_blank(),
#    legend.title = element_blank(),
    plot.caption = element_text(size = size - 2),
#    plot.title = element_blank(),
    plot.subtitle = element_text(size = size),
#    strip.text = element_blank(),

    # Line elements
#    line = element_blank(),
#    axis.ticks = element_blank(),
#    axis.line = element_blank(),
#    panel.grid = element_blank(),
    panel.grid.major = element_line(color = '#CCCCCC', linetype = 'dotted'),
    panel.grid.minor = element_blank(),

    # Rect elements
#    rect = element_blank(),
#    legend.key = element_blank(),
#    legend.background = element_blank(),
#    legend.box.background = element_blank(),
    panel.background = element_blank(), # Drawn underneath the plot
#    panel.border = element_blank(), # Drawn on top of plot, covering tick marks and grid lines (fill = NA)
    plot.background = element_blank(),
#    strip.background = element_blank(),

    # Other
    axis.ticks.length = unit(0, "pt"),

#    legend.margin = margin(-15, 0, -15, 0, "pt"),
#    legend.spacing = element_blank(),
#    legend.key.size = element_blank(),
#    legend.text.align = 0, # 0: left, 1: right
#    legend.title.align = 0,
    legend.position = 'none', # none, left, right, bottom, top, or two-element numeric vector
#    legend.direction = "vertical", # horizontal, vertical
#    legend.justification = "center", # center or two-element numeric vector
#    legend.box = "horizontal", # horizontal, vertical
#    legend.box.just = "left", # top, bottom, left, right
#    legend.box.margin = margin(0, 0, 0, 0, "pt"), # specified
#    legend.box.spacing = unit(0, "pt"),

    panel.spacing = unit(0, "pt"),
    plot.margin = margin(5, 0, 0, 0, "pt"),
    strip.placement = "inside", # inside, outside
    aspect.ratio = 3/4
  )}


# Color data object
# This contains the color definitions specified in the style guide
ap.pal <- function(name, direction = 1) {
  palette <-
    switch(
      name,
      neutrals = c(
        "#FFFFFF", # White
        "#E7E2D8", # Light Gray
        "#B1AAA8", # Medium Gray
        "#8F8F8F", # Deep Gray
        "#5D5A58"  # Heavy Gray
      ),
      primary = c(
        "#146994", # Medium Blue
        "#669900", # Medium Green
        "#D19600", # Medium Orange
        "#674875", # Medium Purple
        "#C83728"  # Medium Red
      ),
      secondary = c(
        "#003753", # Heavy Blue
        "#36572C", # Heavy Green
        "#B55927", # Heavy Orange
        "#382C4B", # Heavy Purple
        "#831618", # Heavy Red
        "#B0C8D2", # Light Blue
        "#AFC8A0", # Light Green
        "#EABF88", # Light Orange
        "#B8B5C8", # Light Purple
        "#EDB1A6"  # Light Red
      ),
      blues = c(
        "#B0C8D2",
        "#6B91AF",
        "#146994",
        "#055072",
        "#003753"
      ),
      greens = c(
        "#AFC8A0",
        "#89AE6C",
        "#669900",
        "#4E7738",
        "#36572C"
      ),
      oranges = c(
        "#EABF88",
        "#DCAA59",
        "#CF962A",
        "#C17729",
        "#B55927"
      ),
      purples = c(
        "#B8B5C8",
        "#8A7798",
        "#674875",
        "#4F3A60",
        "#382C4B"
      ),
      reds = c(
        "#EDB1A6",
        "#D8755E",
        "#C83728",
        "#A52722",
        "#831618"
      ),
      election = c(
        "#C43D49", # GOP Red
        "#DA968E", # GOP Leading
        "#5186B4", # Dem Blue
        "#A2B7D3", # Dem Leading
        "#CCB43D", # Other Yellow
        "#EDDB95", # Other Leading
        "#A751B4"  # Runoff
      )
    )
  if (direction == -1) {
    palette <- rev(palette)
  }
  palette
}

#' @export
scale_color_ap <- function(..., palette = "default", direction = 1) {

  palette_function <- function(x) {
    ap.pal(x, direction)
  }

  if (palette == "default") {
    scale_color_manual(..., values = unlist(
      lapply(
        c("primary",
          "secondary",
          "blues",
          "greens",
          "oranges",
          "purples",
          "reds",
          "neutrals"),
        palette_function)))
  } else {
    scale_color_manual(..., values = palette_function(palette))
  }
}

#' @export
scale_fill_ap <- function(..., palette = "default") {

  palette_function <- function(x) {
    ap.pal(x, direction)
  }

  if (palette == "default") {
    scale_fill_manual(..., values = unlist(
      lapply(
        c("primary",
          "secondary",
          "blues",
          "greens",
          "oranges",
          "purples",
          "reds",
          "neutrals"),
        palette_function)))
  } else {
    scale_fill_manual(..., values = palette_function(palette))
  }
}
