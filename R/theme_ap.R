#' ggplot AP theme
#'
#' This function is a theme function for use with ggplot2.
#' It tries to have a very tight composition, suitable for email or chat message
#'
#' http://ggplot2.tidyverse.org/reference/theme.html
#' @import ggplot2

# Notes on primitives: (defaults)
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
theme_ap <- function(size = 12, family = "AP Condensed") {
  theme(
    # Text elements
    text = element_text(family = family, size = size),
    plot.caption = element_text(hjust = 0, color = "#808080"),
    axis.text = element_text(margin = c(0, 0, 0, 0)),
    legend.text = element_text(),
    title = element_text(size = size),
    axis.title = element_text(size = size, margin = c(0, 0, 0, 0)),
    legend.title = element_blank(),
    plot.title = element_text(size = rel(2)),
    plot.subtitle = element_blank(), # AP style - no subtitles
#    strip.text = element_blank(),

    # Line elements
#    line = element_blank(),
    axis.ticks.y = element_blank(),
#    axis.line = element_blank(),
#    panel.grid = element_blank(),
    panel.grid.major = element_line(color = '#939598', linetype = 'dotted'),
    panel.grid.minor = element_blank(),

    # Rect elements
#    rect = element_blank(),
    legend.key = element_blank(),
#    legend.background = element_blank(),
#    legend.box.background = element_blank(),
    panel.background = element_blank(), # Drawn underneath the plot
#    panel.border = element_blank(), # Drawn on top of plot, covering tick marks and grid lines (fill = NA)
#    plot.background = element_blank(),
#    strip.background = element_blank(),

    # Other
    axis.ticks.length = unit(4, "pt"),
#    legend.margin = margin(-15, 0, -15, 0, "pt"),
#    legend.spacing = unit(0, 'pt'),
    legend.key.size = unit(10, 'pt'),
#    legend.text.align = 0, # 0: left, 1: right
#    legend.title.align = 1,
    legend.position = 'none', # none, left, right, bottom, top, or two-element numeric vector
#    legend.direction = "vertical", # horizontal, vertical
#    legend.justification = "center", # center or two-element numeric vector
#    legend.box = "horizontal", # horizontal, vertical
#    legend.box.just = "left", # top, bottom, left, right
#    legend.box.margin = margin(0, 0, 0, 0, "pt"), # specified
#    legend.box.spacing = unit(0, "pt"),

    panel.spacing = unit(0, "pt"),
    plot.margin = margin(10, 10, 5, 5, "pt"),
    strip.placement = "inside"#, inside, outside
    #aspect.ratio = 3/4
  )}


# Color data object
# This contains the color definitions specified in the style guide
ap.pal <- function(name, direction = 1) {
  palette <-
    switch(
      name,
      red = "#FF1818",
      neutrals = c(
        "#000000", # Black
        "#5F5F5F", # Dark Gray
        "#A5A5A5", # Gray
        "#DCDCDC", # Light Gray
        "#F2F2F2"  # Even Lighter Gray
      ),
      papers = c(
        "#5D5A58", # Heavy Gray
        "#8F8F8F", # Deep Gray
        "#B1AAA8", # Medium Gray
        "#E7E2D8"  # Light Gray
      ),
      primary = c(
        "#00BAFC", # Primary Cyan
        "#8CD200", # Primary Yellow Green
        "#FFBA00", # Primary Yellow Orange
        "#9E0ECE", # Primary Purple
        "#FF1818", # Primary Red
        "#043CFF", # Primary Blue
        "#04CA62", # Primary Green
        "#FDE800", # Primary Yellow
        "#EB0EA5", # Primary Pink
        "#622EDC", # Primary Violet
        "#23E8C8", # Primary Blue Green
        "#FE761A"  # Primary Orange
      ),
      activated = c(
        "#57DAFE", # Activated Cyan
        "#BCEA00", # Activated Yellow Green
        "#FFC80A", # Activated Yellow Orange
        "#C444FA", # Activated Purple
        "#FF1818", # Activated Red
        "#0E6EFF", # Activated Blue
        "#4EFA4E", # Activated Green
        "#FFF400", # Activated Yellow
        "#FF54BC", # Activated Pink
        "#7632FF", # Activated Violet
        "#19FFD7", # Activated Blue Green
        "#FF8C02"  # Activated Orange
      ),
      dark1 = c(
        "#0081E1", # Dark 1 Cyan
        "#62A20A", # Dark 1 Yellow Green
        "#FBA20B", # Dark 1 Yellow Orange
        "#8011A2", # Dark 1 Purple
        "#DA1026", # Dark 1 Red
        "#0A26DE", # Dark 1 Blue
        "#00945A", # Dark 1 Green
        "#FBD206", # Dark 1 Yellow
        "#C40FA1", # Dark 1 Pink
        "#4C28C2", # Dark 1 Violet
        "#16A5A3", # Dark 1 Blue Green
        "#E06016"  # Dark 1 Orange
      ),
      dark2 = c(
        "#005EC7", # Dark 2 Cyan
        "#3A8200", # Dark 2 Yellow Green
        "#E48E00", # Dark 2 Yellow Orange
        "#691684", # Dark 2 Purple
        "#BC0420", # Dark 2 Red
        "#0524C2", # Dark 2 Blue
        "#006C42", # Dark 2 Green
        "#E8B800", # Dark 2 Yellow
        "#9C0087", # Dark 2 Pink
        "#3E10B4", # Dark 2 Violet
        "#0C7B85", # Dark 2 Blue Green
        "#B64A00"  # Dark 2 Orange
      ),
      dark3 = c(
        "#0042A9", # Dark 3 Cyan
        "#1C6C00", # Dark 3 Yellow Green
        "#C87604", # Dark 3 Yellow Orange
        "#5A177A", # Dark 3 Purple
        "#AB0022", # Dark 3 Red
        "#0522A4", # Dark 3 Blue
        "#00573C", # Dark 3 Green
        "#CD9800", # Dark 3 Yellow
        "#7A086E", # Dark 3 Pink
        "#2F1498", # Dark 3 Violet
        "#005E75", # Dark 3 Blue Green
        "#A0400C"  # Dark 3 Orange
      ),
      dark4 = c(
        "#00327C", # Dark 4 Cyan
        "#1E4E00", # Dark 4 Yellow Green
        "#A25A06", # Dark 4 Yellow Orange
        "#4C0472", # Dark 4 Purple
        "#8C0026", # Dark 4 Red
        "#041E8E", # Dark 4 Blue
        "#00483C", # Dark 4 Green
        "#8C6A02", # Dark 4 Yellow
        "#620054", # Dark 4 Pink
        "#280C88", # Dark 4 Violet
        "#004660", # Dark 4 Blue Green
        "#843514"  # Dark 4 Orange
      ),
      dark5 = c(
        "#002860", # Dark 5 Cyan
        "#1C3A00", # Dark 5 Yellow Green
        "#703700", # Dark 5 Yellow Orange
        "#400A60", # Dark 5 Purple
        "#640028", # Dark 5 Red
        "#04196C", # Dark 5 Blue
        "#003832", # Dark 5 Green
        "#644808", # Dark 5 Yellow
        "#50004C", # Dark 5 Pink
        "#24086E", # Dark 5 Violet
        "#00324B", # Dark 5 Blue Green
        "#60300F"  # Dark 5 Orange
      ),
      light1 = c(
        "#84EBFE", # Light 1 Cyan
        "#CBF526", # Light 1 Yellow Green
        "#FFDC64", # Light 1 Yellow Orange
        "#CC6EFF", # Light 1 Purple
        "#FF5C58", # Light 1 Red
        "#4898FE", # Light 1 Blue
        "#96FF94", # Light 1 Green
        "#FDF45A", # Light 1 Yellow
        "#FF78DE", # Light 1 Pink
        "#8C6CFF", # Light 1 Violet
        "#8EFFD8", # Light 1 Blue Green
        "#FFAC2C"  # Light 1 Orange
      ),
      light2 = c(
        "#BDF8FF", # Light 2 Cyan
        "#DAFF69", # Light 2 Yellow Green
        "#FFE682", # Light 2 Yellow Orange
        "#E2ACFF", # Light 2 Purple
        "#FF8E8A", # Light 2 Red
        "#7CCAFE", # Light 2 Blue
        "#C8FDAE", # Light 2 Green
        "#FAFA86", # Light 2 Yellow
        "#FFA0F5", # Light 2 Pink
        "#B1A0FF", # Light 2 Violet
        "#BCFFDE", # Light 2 Blue Green
        "#FFC074"  # Light 2 Orange
      ),
      light3 = c(
        "#D4FFFF", # Light 3 Cyan
        "#EEFFA2", # Light 3 Yellow Green
        "#FFF0AE", # Light 3 Yellow Orange
        "#EBD0FF", # Light 3 Purple
        "#FFCAC6", # Light 3 Red
        "#C8EEFF", # Light 3 Blue
        "#DEFFC4", # Light 3 Green
        "#FAFAB4", # Light 3 Yellow
        "#FFCAFF", # Light 3 Pink
        "#DDD8FF", # Light 3 Violet
        "#D8FFEC", # Light 3 Blue Green
        "#FFEAAF"  # Light 3 Orange
      ),
      light4 = c(
        "#E4FFFF", # Light 4 Cyan
        "#F8FFD0", # Light 4 Yellow Green
        "#FFF6D5", # Light 4 Yellow Orange
        "#F5EFFF", # Light 4 Purple
        "#FFE8E2", # Light 4 Red
        "#E8FAFF", # Light 4 Blue
        "#EFFFD5", # Light 4 Green
        "#FCFCD6", # Light 4 Yellow
        "#FFEEFE", # Light 4 Pink
        "#EFEFFF", # Light 4 Violet
        "#E8FFF3", # Light 4 Blue Green
        "#FFF4D4"  # Light 4 Orange
      ),
      light5 = c(
        "#F0FDFD", # Light 5 Cyan
        "#F8FFE4", # Light 5 Yellow Green
        "#FFFCEA", # Light 5 Yellow Orange
        "#F8F3FF", # Light 5 Purple
        "#FFF4F0", # Light 5 Red
        "#F2FDFF", # Light 5 Blue
        "#F4FDEE", # Light 5 Green
        "#FDFDE8", # Light 5 Yellow
        "#FDF2FF", # Light 5 Pink
        "#F5F5FF", # Light 5 Violet
        "#F4FDFA", # Light 5 Blue Green
        "#FFFAED"  # Light 5 Orange
      ),
      reds = c(
        "#640028",
        "#8C0026",
        "#AB0022",
        "#BC0420",
        "#DA1026",
        "#FF1818",
        "#FF5C58",
        "#FF8E8A",
        "#FFCAC6",
        "#FFE8E2",
        "#FFF4F0"
      ),
      oranges = c(
        "#60300F",
        "#843514",
        "#A0400C",
        "#B64A00",
        "#E06016",
        "#FE761A",
        "#FFAC2C",
        "#FFC074",
        "#FFEAAF",
        "#FFF4D4",
        "#FFFAED"
      ),
      yellow_oranges = c(
        "#703700",
        "#A25A06",
        "#C87604",
        "#E48E00",
        "#FBA20B",
        "#FFBA00",
        "#FFDC64",
        "#FFE682",
        "#FFF0AE",
        "#FFF6D5",
        "#FFFCEA"
      ),
      yellows = c(
        "#644808",
        "#8C6A02",
        "#CD9800",
        "#E8B800",
        "#FBD206",
        "#FDE800",
        "#FDF45A",
        "#FAFA86",
        "#FAFAB4",
        "#FCFCD6",
        "#FDFDE8"
      ),
      yellow_greens = c(
        "#1C3A00",
        "#1E4E00",
        "#1C6C00",
        "#3A8200",
        "#62A20A",
        "#8CD200",
        "#CBF526",
        "#DAFF69",
        "#EEFFA2",
        "#F8FFD0",
        "#F8FFE4"
      ),
      greens = c(
        "#003832",
        "#00483C",
        "#00573C",
        "#006C42",
        "#00945A",
        "#04CA62",
        "#96FF94",
        "#C8FDAE",
        "#DEFFC4",
        "#EFFFD5",
        "#F4FDEE"
      ),
      blue_greens = c(
        "#00324B",
        "#004660",
        "#005E75",
        "#0C7B85",
        "#16A5A3",
        "#23E8C8",
        "#8EFFD8",
        "#BCFFDE",
        "#D8FFEC",
        "#E8FFF3",
        "#F4FDFA"
      ),
      cyans = c(
        "#002860",
        "#00327C",
        "#0042A9",
        "#005EC7",
        "#0081E1",
        "#00BAFC",
        "#84EBFE",
        "#BDF8FF",
        "#D4FFFF",
        "#E4FFFF",
        "#F0FDFD"
      ),
      blues = c(
        "#04196C",
        "#041E8E",
        "#0522A4",
        "#0524C2",
        "#0A26DE",
        "#043CFF",
        "#4898FE",
        "#7CCAFE",
        "#C8EEFF",
        "#E8FAFF",
        "#F2FDFF"
      ),
      violets = c(
        "#24086E",
        "#280C88",
        "#2F1498",
        "#3E10B4",
        "#4C28C2",
        "#622EDC",
        "#8C6CFF",
        "#B1A0FF",
        "#DDD8FF",
        "#EFEFFF",
        "#F5F5FF"
      ),
      purples = c(
        "#400A60",
        "#4C0472",
        "#5A177A",
        "#691684",
        "#8011A2",
        "#9E0ECE",
        "#CC6EFF",
        "#E2ACFF",
        "#EBD0FF",
        "#F5EFFF",
        "#F8F3FF"
      ),
      pinks = c(
        "#50004C",
        "#620054",
        "#7A086E",
        "#9C0087",
        "#C40FA1",
        "#EB0EA5",
        "#FF78DE",
        "#FFA0F5",
        "#FFCAFF",
        "#FFEEFE",
        "#FDF2FF"
      ),
      election = c(
        "#970420", # GOP Flip
        "#C9062A", # GOP Red
        "#DA968E", # GOP Leading
        "#0B5C98", # Dem Flip
        "#0375C9", # Dem Blue
        "#A2B7D3", # Dem Leading
        "#B68C22", # Other Flip
        "#F3BB2D", # Other Yellow
        "#EDDB95", # Other Leading
        "#9D42BE"  # Runoff
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
        c("dark1",
          "dark3",
          "primary",
          "dark2"),
        palette_function)))
  } else if (palette == "testing") {
    scale_color_manual(..., values = unlist(
      lapply(
        c("red",
          "neutrals",
          "papers",
          "primary",
          "activated",
          "dark1",
          "dark2",
          "dark3",
          "dark4",
          "dark5",
          "light1",
          "light2",
          "light3",
          "light4",
          "light5",
          "reds",
          "oranges",
          "yellow_oranges",
          "yellows",
          "yellow_greens",
          "greens",
          "blue_greens",
          "cyans",
          "blues",
          "violets",
          "purples",
          "pinks",
          "election"),
        palette_function)))
  } else {
    scale_color_manual(..., values = palette_function(palette))
  }
}

#' @export
scale_fill_ap <- function(..., palette = "default", direction = 1) {

  palette_function <- function(x) {
    ap.pal(x, direction)
  }

  if (palette == "default") {
    scale_fill_manual(..., values = unlist(
      lapply(
        c("dark1",
          "dark3",
          "primary",
          "dark2"),
          palette_function)))
  } else if (palette == "testing") {
    scale_fill_manual(..., values = unlist(
      lapply(
        c("red",
          "neutrals",
          "papers",
          "primary",
          "activated",
          "dark1",
          "dark2",
          "dark3",
          "dark4",
          "dark5",
          "light1",
          "light2",
          "light3",
          "light4",
          "light5",
          "reds",
          "oranges",
          "yellow_oranges",
          "yellows",
          "yellow_greens",
          "greens",
          "blue_greens",
          "cyans",
          "blues",
          "violets",
          "purples",
          "pinks",
          "election"),
        palette_function)))
  } else {
    scale_fill_manual(..., values = palette_function(palette))
  }
}

