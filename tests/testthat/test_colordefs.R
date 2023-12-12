#' Test plots to check AP style features
#' Basic plots:
#' Scatterplot, histogram, bar plot, line chart, box-and-whisker
#' Fancy plots:
#' Map, hex grid, slope graph, regression plot, small multiples
#' Q: What should the 'default' color be? Right now it's "ggplot black"-- is there a reason to tweak this? (something off-black, for ex?)
#' TODO: Style the regression line object as well

library(ggplot2)
#library(aptheme)

palette_names <- c("primary", "activated", paste0("dark", 1:5), paste0("light", 1:5))
color_names <- c("reds", "oranges", "yellow_oranges", "yellows", "yellow_greens", "greens", "blue_greens", "cyans", "blues", "violets", "purples", "pinks")

swatch <-
  ggplot(
  data = data.frame(
    x = factor(c("red", rep("neutrals", 5), rep("papers", 4), rep(palette_names, each = 12), rep(color_names, each = 11), rep("election", 10)), levels = c("red", "neutrals", "papers", palette_names, color_names, "election")),
    y = rep(1, times = 296),
    box = as.factor(1:296)
    ),
  aes(x = x, y = y, group = x, fill = box)) +
  geom_bar(stat = 'identity') +
  theme_ap() +
  scale_fill_ap(palette = "testing") +
  theme(aspect.ratio = 1, axis.text.x = element_blank(), panel.grid.major = element_blank()) +
  labs(title = "AP colors", x = "", y = "", caption = "Source: AP Style Guide") +
  coord_flip()
swatch

# Scatterplot
sample_scatterplot <-
  ggplot(data = iris,
         aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  theme_ap() +
  scale_color_ap() +
  labs(title = "Sample scatterplot",
       subtitle = "Longer petals are also wider",
       caption = "Source: Iris dataset",
       x = "Length (in.)",
       y = "Width (in.)") +
  theme(legend.position = 'bottom') +
  guides(colour = guide_legend(override.aes = list(size=2.5)))
sample_scatterplot

# Sample histogram
sample_histogram <-
  ggplot(data = morley,
         aes(x = Speed,
             group = Expt,
             fill = as.factor(Expt))) +
  geom_histogram(binwidth = 50) +
  theme_ap() +
  scale_fill_ap(palette = 'blues') +
  labs(title = "Sample histogram",
       subtitle = "Speed of light observations",
       caption = "Source: Morley dataset",
       x = "km/sec - 299000",
       y = "frequency")
sample_histogram

# Sample barplot
sample_barplot <-
  ggplot(data = data.frame(carb = as.factor(mtcars$carb), wt = mtcars$wt),
         aes(x = carb, y = wt, fill = carb)) +
  geom_col() +
  theme_ap() +
  scale_fill_ap(palette = 'election') +
  labs(title = "Sample bar plot",
       subtitle = "Mileage vs. gears",
       caption = "Source: mtcars dataset",
       x = "Carb",
       y = "Combined weight")
sample_barplot

# Sample line chart
sample_linechart <-
  ggplot(data = airquality[which(airquality$Month >= 8),],
         aes(x = Day,
             y = Temp,
             group = Month,
             color = as.factor(Month),
             fill = as.factor(Month),
             label = paste(Month, Temp, sep = '\n'))) +
  geom_line(size = 1) +
  geom_point(data = airquality[c(123, 153),], shape = 21, colour = 'white', size = 4, stroke = 1) +
  geom_text(data = airquality[c(123, 153),], hjust = 0, vjust = 0, nudge_x = .5, nudge_y = -.5) +
  theme_ap() +
  scale_color_ap() +
  scale_fill_ap() +
  labs(title = "Sample line chart",
       subtitle = "Temperature in two months of 1973",
       caption = "Source: airquality dataset")
sample_linechart

# Sample box-and-whisker
sample_boxplot <-
  ggplot(data = iris,
         aes(x = Species,
             y = Sepal.Width,
             group = Species,
             color = Species)) +
  geom_boxplot(coef = .5, width = .2) +
  theme_ap() +
  scale_color_ap() +
  labs(title = "Sample box-and-whisker plot",
       subtitle = "Differences observed between setosa, virginica varieties",
       caption = "Source: Iris dataset",
       y = "Sepal width")
sample_boxplot

# Sample regression
sample_regression <-
  ggplot(data = longley,
         aes(x = GNP,
             y = Employed)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_ap() +
  labs(title = "Sample regression plot",
       subtitle = "Coefficient: .03475, R-sq of .97",
       caption = "Source: Longley dataset")
sample_regression

# Write out
ggsave("~/git/apthemedemo/data/swatch.png", swatch, width=8, height=6)
ggsave("~/git/apthemedemo/data/sample_barplot.png", width=8, height=6, sample_barplot)
ggsave("~/git/apthemedemo/data/sample_boxplot.png", width=8, height=6, sample_boxplot)
ggsave("~/git/apthemedemo/data/sample_histogram.png", width=8, height=6, sample_histogram)
ggsave("~/git/apthemedemo/data/sample_linechart.png", width=8, height=6, sample_linechart)
ggsave("~/git/apthemedemo/data/sample_regression.png", width=8, height=6, sample_regression)
ggsave("~/git/apthemedemo/data/sample_scatterplot.png", width=8, height=6, sample_scatterplot)

ggsave("~/git/apthemedemo/data/swatch.svg", width=8, height=6, swatch)
ggsave("~/git/apthemedemo/data/sample_barplot.svg", width=8, height=6, sample_barplot)
ggsave("~/git/apthemedemo/data/sample_boxplot.svg", width=8, height=6, sample_boxplot)
ggsave("~/git/apthemedemo/data/sample_histogram.svg", width=8, height=6, sample_histogram)
ggsave("~/git/apthemedemo/data/sample_linechart.svg", width=8, height=6, sample_linechart)
ggsave("~/git/apthemedemo/data/sample_regression.svg", width=8, height=6, sample_regression)
ggsave("~/git/apthemedemo/data/sample_scatterplot.svg", width=8, height=6, sample_scatterplot)

# Sample hex grid
# Sample map
# quakes
#
# ggplot(data = quakes,
#        aes(x = lat,
#            y = long,
#            size = mag)) +
#   geom_hex()
