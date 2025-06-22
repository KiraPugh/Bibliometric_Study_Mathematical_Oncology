# Load required libraries
library(countrycode)
library(stringr)
library(dplyr)
library(tidyr)

# Define custom aliases for specific countries
custom_country_aliases <- list(
  "United States" = c("United States", "USA", "US", "America", "United States of America", "U.S.A"),
  "United Kingdom" = c("United Kingdom", "UK", "U.K.", "Britain", "Great Britain", "England", "Scotland", "Wales", "Northern Ireland"),
  "Czechia" = c("Czechia", "Czech Republic"),
  "United Arab Emirates" = c("United Arab Emirates", "U Arab Emirates")
)

# Get vector of standard country names
standard_countries <- countrycode::codelist$country.name.en
standard_countries <- standard_countries[!is.na(standard_countries)]

# Combine standard names and aliases
all_country_variants <- unlist(custom_country_aliases)
all_country_names <- c(standard_countries, all_country_variants)

# Escape special regex characters
escaped_country_names <- stringr::str_replace_all(all_country_names, "([\\^$.|?*+(){}])", "\\\\\\1")
pattern <- paste0("\\b(", paste(unique(escaped_country_names), collapse = "|"), ")\\b")

# Function to normalize matched country names
normalize_country <- function(match) {
  match <- str_to_title(trimws(match))
  
  for (key in names(custom_country_aliases)) {
    if (tolower(match) %in% tolower(custom_country_aliases[[key]])) {
      return(key)
    }
  }
  
  match_from_code <- countrycode(match, origin = "country.name", destination = "country.name")
  return(match_from_code)
}

# Define input files
input_files <- c(
  "OneDrive - Uppsala universitet/Aim 1/Results/International/ALL_math_onco_high_citations_have_countries.txt", 
  "OneDrive - Uppsala universitet/Aim 1/Results/International/All_math_bio_without_onco_high_citations_have_countries.txt"
)

# Process each file
for (file in input_files) {
  # Determine file suffix
  if (grepl("math_onco", file)) {
    output_suffix <- "math_onco"
  } else if (grepl("math_bio_without_onco", file)) {
    output_suffix <- "math_bio_without_onco"
  } else {
    output_suffix <- "unknown"
  }
  
  # Read lines
  lines <- readLines(file)
  
  # Find article start and end indices
  article_indices <- grep("^PT ", lines)
  end_indices <- grep("^ER", lines)
  
  if (length(article_indices) != length(end_indices)) {
    stop("Mismatch in number of article starts and ends in file: ", file)
  }
  
  # Initialize storage
  results <- list()
  missing_c1_count <- 0
  missing_c1_articles <- list()
  zero_country_articles <- list()
  country_count_articles <- list()
  
  # Process articles
  for (i in seq_along(article_indices)) {
    article_lines <- lines[article_indices[i]:end_indices[i]]
    
    # Extract year
    year_line <- grep("^PY ", article_lines, value = TRUE)
    year <- if (length(year_line) > 0) as.integer(str_extract(year_line, "\\d{4}")) else NA
    
    # Extract C1 lines
    c1_lines <- grep("^C1 ", article_lines, value = TRUE)
    
    if (length(c1_lines) == 0) {
      missing_c1_count <- missing_c1_count + 1
      missing_c1_articles[[length(missing_c1_articles) + 1]] <- article_lines
      next
    }
    
    # Collapse text and match countries
    c1_text <- paste(c1_lines, collapse = " ")
    matches <- str_extract_all(c1_text, regex(pattern, ignore_case = TRUE))[[1]]
    normalized <- unique(na.omit(sapply(matches, normalize_country)))
    num_countries <- length(normalized)
    
    # Store if no recognized countries
    if (num_countries == 0) {
      zero_country_articles[[length(zero_country_articles) + 1]] <- article_lines
      next
    }
    
    # Store for summary
    results[[length(results) + 1]] <- data.frame(
      Year = year,
      UniqueCountries = num_countries,
      stringsAsFactors = FALSE
    )
    
    # Group articles by country count
    num_countries_str <- as.character(num_countries)
    if (!num_countries_str %in% names(country_count_articles)) {
      country_count_articles[[num_countries_str]] <- list()
    }
    country_count_articles[[num_countries_str]][[length(country_count_articles[[num_countries_str]]) + 1]] <- article_lines
  }
  
  # Combine results
  all_data <- bind_rows(results)
  
  summary <- all_data %>%
    group_by(Year, UniqueCountries) %>%
    summarise(NumberOfArticles = n(), .groups = "drop") %>%
    arrange(Year, UniqueCountries)
  
  # Define output directory
  output_dir <- "OneDrive - Uppsala universitet/Aim 1/Results/International/Higher than average citations/"
  
  # Write summary CSV
  write.csv(summary, file.path(output_dir, paste0("country_summary_by_year_", output_suffix, ".csv")), row.names = FALSE)
  
  # Write articles with missing C1
  writeLines(
    as.character(unlist(lapply(missing_c1_articles, function(x) paste(x, collapse = "\n")))),
    file.path(output_dir, paste0(output_suffix, "_missing_c1_articles.txt"))
  )
  
  # Write articles with 0 countries
  writeLines(
    as.character(unlist(lapply(zero_country_articles, function(x) paste(x, collapse = "\n")))),
    file.path(output_dir, paste0(output_suffix, "_articles_0_countries.txt"))
  )
  
  # Write articles grouped by country count
  for (country_count in names(country_count_articles)) {
    suffix <- ifelse(country_count == "1", "country", "countries")
    file_path <- file.path(output_dir, paste0(output_suffix, "_articles_", country_count, "_", suffix, ".txt"))
    
    writeLines(
      as.character(unlist(lapply(country_count_articles[[country_count]], function(x) paste(x, collapse = "\n")))),
      file_path
    )
  }
  
  # Console output
  cat("✅ Done for file:", file, "\n")
  cat("Articles processed:", length(article_indices), "\n")
  cat("Articles missing C1 field:", missing_c1_count, "\n")
  cat("Articles with 0 recognized countries:", length(zero_country_articles), "\n")
  cat("Articles grouped by number of countries:\n")
  for (country_count in sort(as.integer(names(country_count_articles)))) {
    cat("  ", country_count, "country(ies):", length(country_count_articles[[as.character(country_count)]]), "\n")
  }
}
