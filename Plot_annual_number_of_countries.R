# ---------- Load required packages ----------
library(dplyr)
library(scales)

# ---------- Function to process and expand data ----------
process_data <- function(filepath) {
  dt <- read.csv(filepath)
  # 'dt' must have columns: Year, UniqueCountries, NumberOfArticles
  expanded <- dt[rep(1:nrow(dt), dt$NumberOfArticles), c("Year", "UniqueCountries")]
  # Make Year a factor, but only over the years present
  expanded$Year <- factor(expanded$Year, levels = sort(unique(expanded$Year)))
  expanded$Year_num <- as.numeric(expanded$Year)
  return(expanded)
}

# ---------- Function to extract and count outliers ----------
get_outlier_counts <- function(expanded_data) {
  outliers <- expanded_data %>%
    group_by(Year) %>%
    mutate(is_outlier = UniqueCountries %in% boxplot.stats(UniqueCountries)$out) %>%
    ungroup() %>%
    filter(is_outlier)
  
  outlier_counts <- outliers %>%
    group_by(Year, UniqueCountries) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      Year_num   = as.numeric(factor(Year, levels = sort(unique(expanded_data$Year)))),
      cex_scaled = rescale(count, to = c(0.5, 2))
    )
  return(outlier_counts)
}

# ---------- Function to draw a single boxplot with overlayed means ----------
plot_box_with_means <- function(data_all, data_high, outliers_all, outliers_high, title, y_max = NULL) {
  # If y_max is not provided, compute it from all‐article data
  if (is.null(y_max)) {
    y_max <- max(data_all$UniqueCountries, na.rm = TRUE)
  }
  y_ticks <- seq(0, y_max, by = 1)
  
  # Draw the boxplot of all‐article data
  boxplot(UniqueCountries ~ Year,
          data    = data_all,
          main    = title,
          xlab    = "Year",
          ylab    = "Number of Countries",
          col     = "lightgray",
          border  = "black",
          las     = 2,
          ylim    = c(0, y_max + 0.5),
          yaxt    = "n",
          outline = FALSE)
  
  # Light horizontal grid lines
  abline(h = y_ticks, col = "lightgray", lty = "dotted")
  axis(2, at = y_ticks, las = 1)
  
  # Plot outliers for ALL articles (small black circles)
  points(outliers_all$Year_num, outliers_all$UniqueCountries,
         pch = 20, col = "black", cex = outliers_all$cex_scaled)
  
  # Add black stars for the mean of ALL articles
  means_all <- tapply(data_all$UniqueCountries, data_all$Year, mean, na.rm = TRUE)
  for (i in seq_along(means_all)) {
    if (!is.na(means_all[i])) {
      points(i, means_all[i], pch = 8, col = "turquoise1", cex = 1)
    }
  }
  
  # Add red crosses for the mean of HIGH‐CITATION articles
  if (!is.null(data_high)) {
    # tapply will produce a vector of length = full Year factor levels,
    # with NA for any Year having no high‐citation entries
    means_high <- tapply(data_high$UniqueCountries, data_high$Year, mean, na.rm = TRUE)
    for (i in seq_along(means_high)) {
      if (!is.na(means_high[i])) {
        points(i, means_high[i], pch = 4, col = "red", cex = 1.5)
      }
    }
  }
  
  # (Optional) Highlight high‐citation outliers:
  # points(outliers_high$Year_num, outliers_high$UniqueCountries,
  #        pch = 20, col = "red", cex = outliers_high$cex_scaled)
}

# ---------- Load & process the “all‐article” datasets ----------
math_onco_all <- process_data("~/OneDrive - Uppsala universitet/Aim 1/Results/International/country_summary_by_year_math_onco.csv")
math_bio_all  <- process_data("~/OneDrive - Uppsala universitet/Aim 1/Results/International/country_summary_by_year_math_bio_without_onco.csv")

outliers_onco_all <- get_outlier_counts(math_onco_all)
outliers_bio_all  <- get_outlier_counts(math_bio_all)

# ---------- Load & process the HIGH‐CITATION datasets (same filenames + "_high_citations") ----------
math_onco_high <- process_data("~/OneDrive - Uppsala universitet/Aim 1/Results/International/Higher than average citations/country_summary_by_year_math_onco_high_citations.csv")
# Re‐factor Year so that levels match the “all‐article” years exactly
math_onco_high$Year <- factor(math_onco_high$Year, levels = levels(math_onco_all$Year))
math_onco_high$Year_num <- as.numeric(math_onco_high$Year)

math_bio_high <- process_data("~/OneDrive - Uppsala universitet/Aim 1/Results/International/Higher than average citations/country_summary_by_year_math_bio_without_onco_high_citations.csv")
# Re‐factor Year to match “all‐article” levels
math_bio_high$Year <- factor(math_bio_high$Year, levels = levels(math_bio_all$Year))
math_bio_high$Year_num <- as.numeric(math_bio_high$Year)

outliers_onco_high <- get_outlier_counts(math_onco_high)
outliers_bio_high  <- get_outlier_counts(math_bio_high)

# ---------- Determine a common y‐axis max (optional) ----------
y_max_onco <- max(math_onco_all$UniqueCountries, math_onco_high$UniqueCountries, na.rm = TRUE)
y_max_bio  <- max(math_bio_all$UniqueCountries,  math_bio_high$UniqueCountries,  na.rm = TRUE)
y_max      <- max(y_max_onco, y_max_bio)

# ---------- Save the combined plot to a PNG ----------
png("~/OneDrive - Uppsala universitet/Aim 1/Results/International/BOXPLOTS_NUMBER_COUNTIES_withMeans.png",
    width = 2400, height = 800, res = 150)

# ---------- Set up side‐by‐side plotting region ----------
par(mfrow = c(1, 2), mar = c(6, 4, 4, 2))

# ---------- Draw the “Mathematical Oncology” boxplot with means ----------
plot_box_with_means(
  data_all     = math_onco_all,
  data_high    = math_onco_high,
  outliers_all  = outliers_onco_all,
  outliers_high = outliers_onco_high,
  title        = "Mathematical Oncology",
  y_max        = y_max
)

# ---------- Draw the “Mathematical Biology excluding Onco” boxplot with means ----------
plot_box_with_means(
  data_all     = math_bio_all,
  data_high    = math_bio_high,
  outliers_all  = outliers_bio_all,
  outliers_high = outliers_bio_high,
  title        = "Mathematical Biology excluding Mathematical Oncology",
  y_max        = y_max
)

dev.off()
