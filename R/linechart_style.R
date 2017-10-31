#' Line chart in AP style
#'
#' This function presents an AP-styled line chart in various formats.
#' @include theme_ap.R

uk <- data.frame(Date = as.integer(12 * time(UKDriverDeaths))/12, UKDeaths = as.matrix(UKDriverDeaths))
us <- data.frame(Date = as.integer(12 * time(USAccDeaths))/12, USDeaths = as.matrix(USAccDeaths))
data <- merge(uk, us, by = "Date", all = TRUE)

# Data style point: data to visuals should be tidy
uk <- data[, c("Date", "UKDeaths")]
us <- data[, c("Date", "USDeaths")]
uk$Country <- "UK"
us$Country <- "US"
colnames(uk)[2] <- "Deaths"
colnames(us)[2] <- "Deaths"
data <- rbind(uk, us)[, c(1, 3, 2)]


# Together ----------------------------------------------------------------

# Code style:
# Initial invocation
p <- ggplot(data) +
  # Plot objects
  geom_line(aes(x = Date, y = Deaths, color = Country)) +
  # Labels and legend
  labs(title = "US & UK accident deaths",
       subtitle = "Many more deaths in the US than in the UK",
       caption = "Data from base R packages",
       x = "Date",
       y = "Driving deaths",
       color = "Country") +
  theme_ap()
p
# Faceted -----------------------------------------------------------------

# p + facet_wrap(~Country, scales = 'free')
rm(uk)