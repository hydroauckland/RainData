# ============================================================================
# AUCKLAND RAINFALL CACHE REFRESHER
# Run once a day with Rscript. Retrieves KiWIS observations, prepares the
# rainfall summaries and hydrological-year comparison, then saves one RDS file
# per station for the Shiny app to read. The Shiny app does not query KiWIS.
#
# Packages: readxl, rvest, dplyr, lubridate, jsonlite
# Default cache folder: <script folder>/rainfall_cache
# Optional override: set environment variable RAINFALL_CACHE_DIR.
# ============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(rvest)
  library(dplyr)
  library(lubridate)
  library(jsonlite)
})

TZ <- "Pacific/Auckland"
KIWIS_BASE_URL <- "http://aklc.hydrotel.co.nz:8080/KiWIS/KiWIS"
STATION_URL <- "https://raw.githubusercontent.com/hydroauckland/rainfall_sites/main/RainList.xlsx"

# Per-site workbooks in this repository provide the Average and historical
# cumulative profiles. KiWIS is queried only for the current hydrological year.
GITHUB_OWNER <- Sys.getenv("RAINFALL_GITHUB_OWNER", unset = "hydroauckland")
GITHUB_REPO <- Sys.getenv("RAINFALL_GITHUB_REPO", unset = "rainfall_sites")
GITHUB_BRANCH <- Sys.getenv("RAINFALL_GITHUB_BRANCH", unset = "main")

get_script_directory <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
  }
  frames <- sys.frames()
  if (length(frames)) {
    for (i in rev(seq_along(frames))) {
      ofile <- frames[[i]]$ofile
      if (!is.null(ofile) && length(ofile) == 1L && nzchar(as.character(ofile))) {
        path <- normalizePath(as.character(ofile), winslash = "/", mustWork = FALSE)
        if (file.exists(path)) return(dirname(path))
      }
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

SCRIPT_DIR <- get_script_directory()
CACHE_DIR <- Sys.getenv("RAINFALL_CACHE_DIR", unset = file.path(SCRIPT_DIR, "rainfall_cache"))
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(CACHE_DIR)) stop("Could not create cache folder: ", CACHE_DIR)

local_today <- function() as.Date(format(Sys.time(), tz = TZ, format = "%Y-%m-%d"))

safe_piece <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- "missing"
  gsub("[^A-Za-z0-9_.-]", "_", x)
}

station_cache_file <- function(station_no, ts_id) {
  file.path(
    CACHE_DIR,
    paste0("station__", safe_piece(station_no), "__", safe_piece(ts_id), ".rds")
  )
}

save_rds_safely <- function(object, path) {
  tmp <- tempfile(pattern = paste0(basename(path), "_"), tmpdir = dirname(path), fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(object, tmp, compress = "gzip")
  # file.copy(overwrite=TRUE) works across Windows and Unix. The temporary
  # file is created in the same folder to avoid cross-volume copying.
  ok <- file.copy(tmp, path, overwrite = TRUE)
  if (!ok) stop("Failed to write cache file: ", path)
  invisible(path)
}

clean_kiwis_column_name <- function(x) {
  tolower(gsub("[^[:alnum:]]", "", trimws(as.character(x))))
}

empty_kiwis_observations <- function() {
  data.frame(
    time = as.POSIXct(character(), tz = TZ),
    observation_date = as.Date(character()),
    value = numeric(),
    quality_code = character(),
    stringsAsFactors = FALSE
  )
}

find_kiwis_values_table <- function(page) {
  tables <- tryCatch(
    rvest::html_table(page, fill = TRUE, convert = FALSE),
    error = function(e) list()
  )
  for (tab in tables) {
    normalized_names <- clean_kiwis_column_name(names(tab))
    if (all(c("timestamp", "value") %in% normalized_names)) {
      timestamp_col <- match("timestamp", normalized_names)
      value_col <- match("value", normalized_names)
      quality_col <- match("qualitycode", normalized_names)
      return(data.frame(
        timestamp = as.character(tab[[timestamp_col]]),
        value = as.character(tab[[value_col]]),
        quality_code = if (!is.na(quality_col)) as.character(tab[[quality_col]]) else rep(NA_character_, nrow(tab)),
        stringsAsFactors = FALSE
      ))
    }
    if (nrow(tab) > 0L) {
      for (row_index in seq_len(nrow(tab))) {
        header <- trimws(as.character(unlist(tab[row_index, , drop = FALSE], use.names = FALSE)))
        normalized_header <- clean_kiwis_column_name(header)
        if (all(c("timestamp", "value") %in% normalized_header)) {
          body <- if (row_index < nrow(tab)) tab[(row_index + 1L):nrow(tab), , drop = FALSE] else tab[0, , drop = FALSE]
          timestamp_col <- match("timestamp", normalized_header)
          value_col <- match("value", normalized_header)
          quality_col <- match("qualitycode", normalized_header)
          return(data.frame(
            timestamp = as.character(body[[timestamp_col]]),
            value = as.character(body[[value_col]]),
            quality_code = if (!is.na(quality_col)) as.character(body[[quality_col]]) else rep(NA_character_, nrow(body)),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  NULL
}

parse_kiwis_timestamp <- function(timestamp) {
  x <- trimws(as.character(timestamp))
  x <- gsub("T", " ", x, fixed = TRUE)
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("ymd HMSz", "ymd HMz", "ymd HMS", "ymd HM", "ymd", "dmy HMSz", "dmy HMz", "dmy HMS", "dmy HM", "dmy", "mdy HMSz", "mdy HMz", "mdy HMS", "mdy HM", "mdy"),
    tz = TZ,
    quiet = TRUE
  ))
  as.POSIXct(parsed, tz = TZ)
}

read_kiwis_values_url <- function(request_url, warn_on_missing_table = FALSE) {
  page <- tryCatch(
    rvest::read_html(request_url),
    error = function(e) {
      stop("KiWIS request failed: ", conditionMessage(e), call. = FALSE)
    }
  )
  tab <- find_kiwis_values_table(page)
  if (is.null(tab)) {
    if (warn_on_missing_table) {
      preview <- tryCatch(rvest::html_text2(page), error = function(e) "Unable to read response text")
      warning("Could not find Timestamp and Value columns. Response: ", substr(gsub("[[:space:]]+", " ", preview), 1L, 400L), call. = FALSE)
    }
    return(empty_kiwis_observations())
  }

  timestamp_text <- trimws(as.character(tab$timestamp))
  parsed_time <- parse_kiwis_timestamp(timestamp_text)
  observation_date <- as.Date(rep(NA_character_, length(timestamp_text)))
  iso_date <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", timestamp_text)
  observation_date[iso_date] <- suppressWarnings(as.Date(substr(timestamp_text[iso_date], 1L, 10L), format = "%Y-%m-%d"))
  other_rows <- which(is.na(observation_date) & !is.na(parsed_time))
  if (length(other_rows)) observation_date[other_rows] <- as.Date(parsed_time[other_rows], tz = TZ)

  out <- data.frame(
    time = parsed_time,
    observation_date = observation_date,
    value = suppressWarnings(as.numeric(gsub(",", "", tab$value, fixed = TRUE))),
    quality_code = as.character(tab$quality_code),
    stringsAsFactors = FALSE
  )
  out %>%
    filter(!is.na(time), !is.na(observation_date), is.finite(value)) %>%
    arrange(time)
}

make_kiwis_url <- function(ts_id, from, datasource = 0L, to = NULL) {
  url <- paste0(
    KIWIS_BASE_URL,
    "?service=kisters&type=queryServices&request=getTimeseriesValues",
    "&datasource=", datasource,
    "&format=html",
    "&ts_id=", utils::URLencode(as.character(ts_id), reserved = TRUE),
    "&from=", format(as.Date(from), "%Y-%m-%d"),
    if (!is.null(to)) paste0("&to=", format(as.Date(to), "%Y-%m-%d")) else "",
    "&returnfields=Timestamp,Value,Quality%20Code",
    "&timezone=Etc/GMT-12",
    "&dateformat=yyyy-MM-dd'T'HH:mm:ss"
  )
  url
}

get_kiwis_chunk <- function(ts_id, from, to, datasource = 0L) {
  url <- make_kiwis_url(ts_id, from, datasource = datasource, to = to)
  dat <- read_kiwis_values_url(url, warn_on_missing_table = FALSE)
  if (nrow(dat) == 0L) return(data.frame(time = as.POSIXct(character(), tz = TZ), value = numeric()))
  dat %>%
    filter(observation_date >= as.Date(from), observation_date < as.Date(to)) %>%
    select(time, value) %>%
    arrange(time)
}

get_kiwis_period <- function(ts_id, start_date, end_date, datasource = 0L) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (start_date > end_date) return(data.frame(time = as.POSIXct(character(), tz = TZ), value = numeric()))
  years <- seq.int(lubridate::year(start_date), lubridate::year(end_date))
  chunks <- list()
  for (yr in years) {
    chunk_start <- max(start_date, as.Date(sprintf("%04d-01-01", yr)))
    chunk_end <- min(end_date, as.Date(sprintf("%04d-12-31", yr)))
    if (chunk_start > chunk_end) next
    dat <- get_kiwis_chunk(ts_id, chunk_start, chunk_end + 1, datasource = datasource)
    if (nrow(dat)) chunks[[length(chunks) + 1L]] <- dat
  }
  if (!length(chunks)) return(data.frame(time = as.POSIXct(character(), tz = TZ), value = numeric()))
  bind_rows(chunks) %>% arrange(time) %>% distinct(time, .keep_all = TRUE)
}

get_cumulative_source_period <- function(ts_id, start_date, end_date) {
  # The tested method uses the station's ordinary rainfall series (datasource 0),
  # then calculates a separate cumulative sum within each July-June hydro year.
  # Do not use station_no~Rainfall.CUM2400 here: that is a different series and
  # can give incorrect cross-year cumulative values for this comparison.
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (start_date > end_date) return(empty_kiwis_observations())

  dat <- get_kiwis_period(ts_id, start_date, end_date, datasource = 0L)
  if (!nrow(dat)) return(empty_kiwis_observations())

  out <- data.frame(
    time = as.POSIXct(dat$time, tz = TZ),
    observation_date = as.Date(dat$time, tz = TZ),
    value = suppressWarnings(as.numeric(dat$value)),
    quality_code = NA_character_,
    stringsAsFactors = FALSE
  )
  out %>%
    filter(
      !is.na(time), !is.na(observation_date),
      observation_date >= start_date, observation_date <= end_date,
      is.finite(value)
    ) %>%
    arrange(observation_date, time) %>%
    distinct(time, .keep_all = TRUE)
}

hydro_year_dates <- function(today) {
  month_now <- as.integer(format(today, "%m"))
  year_now <- as.integer(format(today, "%Y"))
  current_start_year <- if (month_now >= 7L) year_now else year_now - 1L
  list(
    current_start = as.Date(sprintf("%04d-07-01", current_start_year)),
    current_start_year = current_start_year,
    current_label = sprintf("%d/%02d", current_start_year, (current_start_year + 1L) %% 100L)
  )
}

hydro_year_label <- function(start_year) {
  sprintf("%d/%02d", as.integer(start_year), (as.integer(start_year) + 1L) %% 100L)
}

# ---------- GitHub workbook discovery and readers ----------
normalize_site_id <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  tolower(gsub("[^[:alnum:]]", "", x))
}

normalize_file_base <- function(path) {
  tolower(gsub("[^[:alnum:]]", "", tools::file_path_sans_ext(basename(path))))
}

github_repo_paths <- function() {
  tree_url <- sprintf(
    "https://api.github.com/repos/%s/%s/git/trees/%s?recursive=1",
    GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH
  )
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({
    utils::download.file(tree_url, tmp, mode = "wb", quiet = TRUE)
    file.exists(tmp) && file.info(tmp)$size > 0
  }, error = function(e) {
    warning("Could not list GitHub repository files: ", conditionMessage(e), call. = FALSE)
    FALSE
  })
  if (!ok) stop("Could not list GitHub files in ", GITHUB_OWNER, "/", GITHUB_REPO,
                 ". Check the repository configuration and internet access.")

  listing <- tryCatch(jsonlite::fromJSON(tmp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(listing$tree) || !length(listing$tree)) {
    api_message <- if (!is.null(listing$message)) as.character(listing$message) else "no file tree was returned"
    stop("GitHub did not return a repository file tree: ", api_message)
  }
  paths <- vapply(listing$tree, function(item) {
    if (identical(item$type, "blob")) as.character(item$path) else NA_character_
  }, character(1))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (!length(paths)) stop("No files were found in the configured GitHub repository.")
  paths
}

github_raw_url <- function(path) {
  parts <- strsplit(gsub("^/+", "", path), "/", fixed = TRUE)[[1]]
  encoded_parts <- vapply(parts, function(part) utils::URLencode(part, reserved = TRUE), character(1))
  paste0(
    "https://raw.githubusercontent.com/", GITHUB_OWNER, "/", GITHUB_REPO, "/",
    GITHUB_BRANCH, "/", paste(encoded_parts, collapse = "/")
  )
}

read_github_excel <- function(path) {
  tmp <- tempfile(fileext = paste0(".", tolower(tools::file_ext(path))))
  on.exit(unlink(tmp), add = TRUE)
  url <- github_raw_url(path)
  ok <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    file.exists(tmp) && file.info(tmp)$size > 0
  }, error = function(e) {
    warning("Could not download GitHub workbook '", path, "': ", conditionMessage(e), call. = FALSE)
    FALSE
  })
  if (!ok) stop("Could not download GitHub workbook: ", url)
  tryCatch(
    as.data.frame(readxl::read_excel(tmp)),
    error = function(e) stop("Could not read workbook '", path, "': ", conditionMessage(e))
  )
}

find_colour_code_path <- function(paths) {
  excel_paths <- paths[tolower(tools::file_ext(paths)) %in% c("xlsx", "xls")]
  normalized <- vapply(excel_paths, normalize_file_base, character(1))
  exact <- which(normalized %in% c("colourcode", "colorcode", "coulurcode", "colurcode"))
  if (!length(exact)) {
    exact <- which(grepl("col.*code", normalized) | grepl("coul.*code", normalized))
  }
  if (!length(exact)) {
    stop("Could not find the Colour code Excel workbook in ", GITHUB_OWNER, "/", GITHUB_REPO,
         ". Expected a workbook named like 'Colour code.xlsx'.")
  }
  excel_paths[exact[[1]]]
}

read_colour_code <- function(path) {
  dat <- read_github_excel(path)
  normalized_names <- tolower(gsub("[^[:alnum:]]", "", names(dat)))
  site_col <- match("site", normalized_names)
  year_col <- match("year", normalized_names)
  red_col <- match("r", normalized_names)
  green_col <- match("g", normalized_names)
  blue_col <- match("b", normalized_names)

  if (anyNA(c(site_col, year_col, red_col, green_col, blue_col))) {
    stop(
      "Colour code workbook must contain separate columns named Site, Year, R, G, and B. ",
      "The R, G, and B columns are read separately and combined into a plot colour."
    )
  }

  red <- suppressWarnings(as.numeric(as.character(dat[[red_col]])))
  green <- suppressWarnings(as.numeric(as.character(dat[[green_col]])))
  blue <- suppressWarnings(as.numeric(as.character(dat[[blue_col]])))
  finite_channels <- c(red, green, blue)
  finite_channels <- finite_channels[is.finite(finite_channels)]
  if (!length(finite_channels)) {
    stop("The R, G, and B columns in the Colour code workbook contain no numeric colour values.")
  }

  # Excel RGB channel columns are normally encoded 0-255. Also accept 0-1
  # channels if the entire workbook uses normalized channel values.
  max_channel <- if (all(finite_channels >= 0 & finite_channels <= 1)) 1 else 255
  make_hex <- function(r, g, b) {
    if (!all(is.finite(c(r, g, b)))) return(NA_character_)
    if (any(c(r, g, b) < 0 | c(r, g, b) > max_channel)) return(NA_character_)
    grDevices::rgb(r, g, b, maxColorValue = max_channel)
  }
  hex_colours <- vapply(seq_len(nrow(dat)), function(i) {
    make_hex(red[[i]], green[[i]], blue[[i]])
  }, character(1))

  out <- data.frame(
    Site = as.character(dat[[site_col]]),
    Year = suppressWarnings(as.integer(as.character(dat[[year_col]]))),
    R = red,
    G = green,
    B = blue,
    rgb = hex_colours,
    stringsAsFactors = FALSE
  )
  out$site_key <- normalize_site_id(out$Site)
  out %>% filter(!is.na(Year), nzchar(site_key), !is.na(rgb), nzchar(rgb))
}


# -----------------------------------------------------------------------------
# Convert the first/site-profile axis column to a 1 July-based plotting day.
# It accepts Excel dates, date strings, or numeric day-of-hydrological-year
# values (1..366 or 0..365). All series use the same leap-aware reference year.
# -----------------------------------------------------------------------------
HYDRO_AXIS_START <- as.Date("1999-07-01")

empty_cumulative_data <- function() {
  data.frame(
    time = as.POSIXct(character(), tz = TZ),
    cumulative_rainfall = numeric(),
    hydro_period = character(),
    hydro_year_label = character(),
    hydro_start_year = integer(),
    rgb = character(),
    stringsAsFactors = FALSE
  )
}

empty_average_data <- function() {
  data.frame(
    calendar_date = as.Date(character()),
    hydro_plot_date = as.Date(character()),
    average_rainfall = numeric(),
    cumulative_average_rainfall = numeric(),
    day_index = integer(),
    stringsAsFactors = FALSE
  )
}

empty_summary_data <- function() {
  data.frame(
    rainfall_24h = NA_real_, rainfall_72h = NA_real_,
    rainfall_30d = NA_real_, annual_rainfall = NA_real_
  )
}

normalise_header <- function(x) {
  tolower(gsub("[^[:alnum:]]", "", trimws(as.character(x))))
}

hydro_date_to_plot_day <- function(dates) {
  dates <- as.Date(dates)
  result <- rep(NA_real_, length(dates))
  valid <- !is.na(dates)
  if (!any(valid)) return(result)

  month_number <- as.integer(format(dates[valid], "%m"))
  reference_year <- ifelse(month_number >= 7L, 1999L, 2000L)
  reference_dates <- as.Date(sprintf(
    "%04d-%s", reference_year, format(dates[valid], "%m-%d")
  ), format = "%Y-%m-%d")
  result[valid] <- as.numeric(reference_dates - HYDRO_AXIS_START)
  result
}

hydro_axis_to_plot_day <- function(x) {
  n <- length(x)
  if (!n) return(numeric())

  if (inherits(x, "POSIXt")) {
    return(hydro_date_to_plot_day(as.Date(x, tz = TZ)))
  }
  if (inherits(x, "Date")) return(hydro_date_to_plot_day(x))

  if (is.numeric(x) || is.integer(x)) {
    values <- suppressWarnings(as.numeric(x))
    finite_values <- values[is.finite(values)]
    if (!length(finite_values)) return(rep(NA_real_, n))

    # Excel serial date values are usually much larger than a day index.
    if (max(finite_values) > 10000 && max(finite_values) < 100000) {
      dates <- as.Date(values, origin = "1899-12-30")
      return(hydro_date_to_plot_day(dates))
    }

    # Numeric day indexes: 1 means 1 July; 0 is also accepted as 1 July.
    if (min(finite_values) >= 0 && max(finite_values) <= 366) {
      if (min(finite_values) >= 1) return(values - 1)
      return(values)
    }
    return(rep(NA_real_, n))
  }

  text_values <- trimws(as.character(x))
  text_values[text_values %in% c("", "NA", "N/A", "NaN")] <- NA_character_
  numeric_values <- suppressWarnings(as.numeric(text_values))
  finite_numeric <- numeric_values[is.finite(numeric_values)]
  if (length(finite_numeric) && all(is.na(text_values) | is.finite(numeric_values)) &&
      min(finite_numeric) >= 0 && max(finite_numeric) <= 366) {
    if (min(finite_numeric) >= 1) return(numeric_values - 1)
    return(numeric_values)
  }

  # First try common actual-date formats, including dates such as 1-Jul.
  parsed <- suppressWarnings(lubridate::parse_date_time(
    text_values,
    orders = c(
      "ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy",
      "mdy HMS", "mdy HM", "mdy", "d-b-Y", "d-b", "d-B-Y", "d-B",
      "b-d-Y", "b-d", "B-d-Y", "B-d", "d/m/Y", "d/m", "m/d/Y",
      "m/d", "d-m-Y", "d-m", "m-d-Y", "m-d"
    ),
    tz = TZ, quiet = TRUE
  ))
  valid_dates <- !is.na(parsed)
  result <- rep(NA_real_, n)
  if (any(valid_dates)) {
    result[valid_dates] <- hydro_date_to_plot_day(as.Date(parsed[valid_dates], tz = TZ))
  }
  result
}

reference_time_from_plot_day <- function(plot_day) {
  as.POSIXct(HYDRO_AXIS_START + floor(plot_day), tz = TZ)
}

find_site_workbook_path <- function(paths, site_values) {
  # Prefer the exact path from the GitHub repository tree when available.
  excel_paths <- paths[tolower(tools::file_ext(paths)) %in% c("xlsx", "xls", "xlsm")]
  if (length(excel_paths)) {
    base_names <- vapply(excel_paths, normalize_file_base, character(1))
    site_values <- as.character(site_values)
    site_values <- site_values[!is.na(site_values) & nzchar(trimws(site_values))]
    candidate_names <- unique(vapply(site_values, normalize_file_base, character(1)))
    candidate_names <- candidate_names[!is.na(candidate_names) & nzchar(candidate_names)]
    for (candidate in candidate_names) {
      hit <- which(base_names == candidate)
      if (length(hit)) return(excel_paths[[hit[[1]]]])
    }
  }

  # Fallback: site workbooks in this repository are named with the Site number.
  # This bypasses the GitHub API tree endpoint and requests the raw file directly.
  site_values <- trimws(as.character(site_values))
  site_values <- site_values[!is.na(site_values) & nzchar(site_values)]
  if (length(site_values)) {
    for (site_value in site_values) {
      return(paste0(site_value, ".xlsx"))
    }
  }
  NA_character_
}

read_site_profile_workbook <- function(path, site_colour_rows, current_start_year) {
  dat <- read_github_excel(path)
  if (!nrow(dat)) stop("Site workbook has no data rows: ", path)
  headers <- trimws(as.character(names(dat)))
  normalized <- normalise_header(headers)

  axis_candidates <- c("hydroyear", "hydroday", "dayofhydroyear", "hydrodate",
                       "date", "day", "daynumber", "julian")
  axis_col <- which(normalized %in% axis_candidates)
  if (!length(axis_col)) axis_col <- 1L
  axis_col <- axis_col[[1]]

  average_col <- which(normalized %in% c("average", "avg", "meancumulative", "averagecumulative"))
  if (!length(average_col)) {
    stop("Site workbook has no 'Average' column: ", path)
  }
  average_col <- average_col[[1]]

  plot_day <- hydro_axis_to_plot_day(dat[[axis_col]])
  # Only the selected year columns from ColourCode are loaded. The current
  # hydrological year is fetched from KiWIS and is deliberately excluded here.
  selected_years <- sort(unique(site_colour_rows$Year))
  selected_years <- selected_years[
    !is.na(selected_years) & selected_years < as.integer(current_start_year)
  ]

  average_values <- suppressWarnings(as.numeric(gsub(",", "", as.character(dat[[average_col]]), fixed = TRUE)))
  average_ok <- is.finite(plot_day) & plot_day >= 0 & plot_day <= 366 & is.finite(average_values)
  average_data <- empty_average_data()
  if (any(average_ok)) {
    average_days <- plot_day[average_ok]
    average_cumulative <- average_values[average_ok]
    ordering <- order(average_days)
    average_days <- average_days[ordering]
    average_cumulative <- average_cumulative[ordering]
    average_dates <- HYDRO_AXIS_START + floor(average_days)
    average_data <- data.frame(
      calendar_date = average_dates,
      hydro_plot_date = average_dates,
      average_rainfall = c(average_cumulative[[1]], diff(average_cumulative)),
      cumulative_average_rainfall = average_cumulative,
      day_index = as.integer(floor(average_days) + 1),
      stringsAsFactors = FALSE
    )
  }

  year_parts <- list()
  missing_years <- integer()
  for (yr in selected_years) {
    year_col <- which(normalized == as.character(as.integer(yr)))
    if (!length(year_col)) {
      missing_years <- c(missing_years, as.integer(yr))
      warning("Selected year column ", yr, " is missing from ", path, call. = FALSE)
      next
    }
    values <- suppressWarnings(as.numeric(gsub(",", "", as.character(dat[[year_col[[1]]]]), fixed = TRUE)))
    keep <- is.finite(plot_day) & plot_day >= 0 & plot_day <= 366 & is.finite(values)
    if (!any(keep)) {
      missing_years <- c(missing_years, as.integer(yr))
      warning("Selected year column ", yr, " contains no usable cumulative values in ", path, call. = FALSE)
      next
    }

    rgb_row <- which(site_colour_rows$Year == as.integer(yr))
    line_colour <- if (length(rgb_row)) as.character(site_colour_rows$rgb[[rgb_row[[1]]]]) else "#666666"
    plot_days <- plot_day[keep]
    values <- values[keep]
    ordering <- order(plot_days)
    line_data <- data.frame(
      time = reference_time_from_plot_day(plot_days[ordering]),
      cumulative_rainfall = values[ordering],
      hydro_period = "selected",
      hydro_year_label = hydro_year_label(yr),
      hydro_start_year = as.integer(yr),
      rgb = line_colour,
      stringsAsFactors = FALSE
    )
    year_parts[[length(year_parts) + 1L]] <- line_data
  }

  historical_data <- if (length(year_parts)) {
    bind_rows(year_parts) %>% arrange(hydro_start_year, time)
  } else {
    empty_cumulative_data()
  }

  list(
    historical_data = historical_data,
    average_data = average_data,
    site_profile_ok = nrow(average_data) > 0L,
    missing_years = sort(unique(missing_years)),
    years_loaded = sort(unique(historical_data$hydro_start_year))
  )
}

calculate_summary <- function(rainfall_data, cumulative_data) {
  result <- empty_summary_data()
  if (is.null(rainfall_data) || !nrow(rainfall_data) || all(is.na(rainfall_data$time))) return(result)
  latest_time <- max(rainfall_data$time, na.rm = TRUE)
  sum_since <- function(cutoff) {
    sum(rainfall_data$rainfall[
      rainfall_data$time >= cutoff & rainfall_data$time <= latest_time
    ], na.rm = TRUE)
  }
  result$rainfall_24h <- sum_since(latest_time - lubridate::hours(24))
  result$rainfall_72h <- sum_since(latest_time - lubridate::hours(72))
  result$rainfall_30d <- sum_since(latest_time - lubridate::days(30))
  current <- cumulative_data %>% filter(hydro_period == "current") %>% arrange(time)
  if (nrow(current)) result$annual_rainfall <- dplyr::last(current$cumulative_rainfall)
  result
}

empty_rainfall_raw <- function() {
  data.frame(time = as.POSIXct(character(), tz = TZ), rainfall = numeric())
}

prepare_rainfall_data <- function(current_year_raw) {
  if (is.null(current_year_raw) || !nrow(current_year_raw)) {
    return(data.frame(
      time = as.POSIXct(character(), tz = TZ), rainfall = numeric(),
      interval_hours = numeric(), interval_minutes = numeric(), intensity_mm_hr = numeric()
    ))
  }
  out <- current_year_raw %>%
    transmute(time = as.POSIXct(time, tz = TZ), rainfall = as.numeric(value)) %>%
    filter(!is.na(time), is.finite(rainfall)) %>%
    arrange(time) %>% distinct(time, .keep_all = TRUE) %>%
    mutate(
      interval_hours = as.numeric(difftime(time, lag(time), units = "hours")),
      interval_minutes = interval_hours * 60,
      intensity_mm_hr = if_else(!is.na(interval_hours) & interval_hours > 0,
                                rainfall / interval_hours, NA_real_)
    )
  out
}

prepare_current_cumulative <- function(current_year_raw, current_start_year) {
  if (is.null(current_year_raw) || !nrow(current_year_raw)) return(empty_cumulative_data())
  current_year_raw %>%
    filter(!is.na(time), !is.na(observation_date), is.finite(value)) %>%
    arrange(time) %>% distinct(time, .keep_all = TRUE) %>%
    mutate(
      cumulative_rainfall = cumsum(dplyr::coalesce(as.numeric(value), 0)),
      hydro_period = "current",
      hydro_year_label = hydro_year_label(current_start_year),
      hydro_start_year = as.integer(current_start_year),
      rgb = "#008000"
    ) %>%
    select(time, cumulative_rainfall, hydro_period, hydro_year_label, hydro_start_year, rgb)
}

# ---------- Read station metadata ----------
message("Loading station metadata from GitHub...")
metadata_tmp <- tempfile(fileext = ".xlsx")
metadata_download_ok <- tryCatch({
  utils::download.file(STATION_URL, metadata_tmp, mode = "wb", quiet = TRUE)
  file.exists(metadata_tmp) && file.info(metadata_tmp)$size > 0
}, error = function(e) {
  warning("Could not download station metadata: ", conditionMessage(e), call. = FALSE)
  FALSE
})
if (!metadata_download_ok) {
  unlink(metadata_tmp)
  stop("Station metadata download failed; no station cache was refreshed.")
}
stations <- as.data.frame(readxl::read_excel(metadata_tmp))
unlink(metadata_tmp)
required <- c("Label", "ts_id", "station_no", "station_longitude", "station_latitude")
missing <- setdiff(required, names(stations))
if (length(missing)) stop("Station spreadsheet is missing columns: ", paste(missing, collapse = ", "))

stations <- stations %>%
  mutate(
    Label = as.character(Label),
    ts_id = trimws(as.character(ts_id)),
    station_no = trimws(as.character(station_no)),
    station_no = na_if(station_no, ""),
    station_longitude = suppressWarnings(as.numeric(station_longitude)),
    station_latitude = suppressWarnings(as.numeric(station_latitude))
  ) %>%
  filter(!is.na(Label), !is.na(ts_id), nzchar(ts_id),
         is.finite(station_longitude), is.finite(station_latitude))

save_rds_safely(stations, file.path(CACHE_DIR, "stations_metadata.rds"))

message("Cache folder: ", normalizePath(CACHE_DIR, winslash = "/", mustWork = FALSE))
message("Indexing GitHub workbooks in ", GITHUB_OWNER, "/", GITHUB_REPO, "...")

# The GitHub API tree is useful for detecting files in subfolders, but must not
# prevent the refresh from running: some R/Windows network configurations can
# fail on api.github.com while raw.githubusercontent.com remains accessible.
github_tree_error <- NA_character_
github_paths <- tryCatch(
  github_repo_paths(),
  error = function(e) {
    github_tree_error <<- conditionMessage(e)
    message("GitHub tree listing unavailable; trying direct raw-file URLs. Detail: ", github_tree_error)
    character()
  }
)

colour_code_path <- NA_character_
colour_code <- NULL
colour_errors <- character()

# First try the file located by the repository tree, if available.
if (length(github_paths)) {
  tree_colour_path <- tryCatch(find_colour_code_path(github_paths), error = function(e) NA_character_)
  if (length(tree_colour_path) == 1L && !is.na(tree_colour_path)) {
    cc_result <- tryCatch(read_colour_code(tree_colour_path), error = function(e) {
      colour_errors <<- c(colour_errors, paste0(tree_colour_path, ": ", conditionMessage(e)))
      NULL
    })
    if (!is.null(cc_result) && nrow(cc_result)) {
      colour_code <- cc_result
      colour_code_path <- tree_colour_path
    }
  }
}

# Direct root-level candidates. The file's capitalization/spaces can differ,
# so try the common forms rather than depending on the API tree lookup.
if (is.null(colour_code)) {
  direct_candidates <- c(
    "ColourCode.xlsx", "Colour Code.xlsx", "Colour code.xlsx",
    "colourcode.xlsx", "ColorCode.xlsx", "Color Code.xlsx",
    "ColourCode.xls", "Colour Code.xls", "Colour code.xls"
  )
  for (candidate in direct_candidates) {
    cc_result <- tryCatch(read_colour_code(candidate), error = function(e) {
      colour_errors <<- c(colour_errors, paste0(candidate, ": ", conditionMessage(e)))
      NULL
    })
    if (!is.null(cc_result) && nrow(cc_result)) {
      colour_code <- cc_result
      colour_code_path <- candidate
      break
    }
  }
}

colour_code_error <- NA_character_
if (is.null(colour_code) || !nrow(colour_code)) {
  colour_code <- data.frame(
    Site = character(), Year = integer(), R = numeric(), G = numeric(), B = numeric(),
    rgb = character(), site_key = character(), stringsAsFactors = FALSE
  )
  colour_code_error <- if (length(colour_errors)) {
    paste(colour_errors, collapse = " | ")
  } else {
    "Could not read a usable ColourCode workbook from the GitHub repository."
  }
  warning(
    "ColourCode workbook unavailable. The refresher will still attempt current-year KiWIS data " ,
    "and each site's Average profile; historical year selection/colours may be missing. Details: ",
    colour_code_error, call. = FALSE
  )
} else {
  message("Colour code workbook: ", colour_code_path,
          " (", nrow(colour_code), " valid site/year/colour rows)")
}

today <- local_today()
dates <- hydro_year_dates(today)
current_start_year <- dates$current_start_year
current_start <- dates$current_start
message("Refresh date: ", format(today), "; current hydrological year: ", dates$current_label,
        "; selected historical profiles and Average are read from GitHub site workbooks.")
message("KiWIS is queried only for current-year rainfall from ", format(current_start), " to ", format(today), ".")

log_rows <- vector("list", nrow(stations))
for (i in seq_len(nrow(stations))) {
  station <- stations[i, , drop = FALSE]
  station_no <- station$station_no[[1]]
  ts_id <- station$ts_id[[1]]
  station_label <- station$Label[[1]]
  cache_path <- station_cache_file(station_no, ts_id)
  message(sprintf("[%d/%d] %s (station_no=%s, ts_id=%s)",
                  i, nrow(stations), station_label, station_no, ts_id))

  old <- if (file.exists(cache_path)) tryCatch(readRDS(cache_path), error = function(e) NULL) else NULL
  old_refreshed_at <- if (!is.null(old$refreshed_at)) {
    tryCatch(as.POSIXct(old$refreshed_at, tz = TZ), error = function(e) as.POSIXct(NA, tz = TZ))
  } else as.POSIXct(NA, tz = TZ)

  site_colour_rows <- colour_code %>%
    filter(site_key == normalize_site_id(station_no))
  selected_years <- sort(unique(site_colour_rows$Year))
  selected_years <- selected_years[!is.na(selected_years) & selected_years < current_start_year]
  site_numbers <- unique(c(as.character(site_colour_rows$Site), as.character(station_no)))
  site_workbook_path <- find_site_workbook_path(github_paths, site_numbers)

  profile_result <- tryCatch({
    if (is.na(site_workbook_path)) {
      stop("Could not find a per-site Excel workbook matching Site/station number: ",
           paste(site_numbers[nzchar(site_numbers)], collapse = ", "))
    }
    out <- read_site_profile_workbook(site_workbook_path, site_colour_rows, current_start_year)
    list(data = out, error = NULL)
  }, error = function(e) list(data = NULL, error = conditionMessage(e)))

  if (is.null(profile_result$error)) {
    historical_data <- profile_result$data$historical_data
    average_data <- profile_result$data$average_data
    profile_query_ok <- isTRUE(profile_result$data$site_profile_ok)
    profile_error <- if (length(profile_result$data$missing_years)) {
      paste0("Missing selected year columns: ", paste(profile_result$data$missing_years, collapse = ", "))
    } else if (!profile_query_ok) {
      "The Average profile has no usable values."
    } else NA_character_
    years_loaded <- profile_result$data$years_loaded
    years_missing <- profile_result$data$missing_years
  } else {
    warning("Could not load GitHub profiles for ", station_label, ": ",
            profile_result$error, call. = FALSE)
    historical_data <- empty_cumulative_data()
    average_data <- empty_average_data()
    profile_query_ok <- FALSE
    profile_error <- profile_result$error
    years_loaded <- integer()
    years_missing <- selected_years

    # Fall back only to a prior cache known to have used this same GitHub-file
    # source; never reuse the old API-derived historical curves or old average.
    if (!is.null(old$profile_source_method) &&
        old$profile_source_method %in% c("github_site_workbook_v1", "github_site_workbook_v2_direct_fallback")) {
      if (!is.null(old$github_historical_data)) historical_data <- old$github_historical_data
      if (!is.null(old$average_data)) average_data <- old$average_data
    }
  }

  # Load/refresh only the current hydrological year's ordinary rainfall series.
  # The same values power the last-7-day rainfall chart, short-term summaries,
  # and the current-year cumulative line. Historical years are never fetched
  # from KiWIS.
  old_current_raw <- empty_kiwis_observations()
  old_same_hydro_year <- !is.null(old$current_start_year) &&
    suppressWarnings(as.integer(old$current_start_year)) == as.integer(current_start_year)
  if (isTRUE(old_same_hydro_year) && !is.null(old$current_year_raw) &&
      all(c("time", "observation_date", "value", "quality_code") %in% names(old$current_year_raw))) {
    old_current_raw <- old$current_year_raw
  } else if (isTRUE(old_same_hydro_year) && !is.null(old$cumulative_source_raw) &&
             all(c("time", "observation_date", "value", "quality_code") %in% names(old$cumulative_source_raw))) {
    # One-time migration from a previous cache, selecting only the current year.
    old_current_raw <- old$cumulative_source_raw %>%
      filter(observation_date >= current_start, observation_date <= today)
  }
  old_current_raw <- old_current_raw %>%
    filter(!is.na(time), !is.na(observation_date),
           observation_date >= current_start, observation_date <= today, is.finite(value)) %>%
    arrange(time) %>% distinct(time, .keep_all = TRUE)

  fetch_start <- current_start
  if (nrow(old_current_raw)) {
    last_cached_date <- max(old_current_raw$observation_date, na.rm = TRUE)
    if (is.finite(as.numeric(last_cached_date))) {
      fetch_start <- max(current_start, last_cached_date - 2L)
      fetch_start <- min(fetch_start, today)
    }
  }

  fetch_error <- NULL
  fresh_current_raw <- tryCatch(
    get_cumulative_source_period(ts_id, fetch_start, today),
    error = function(e) {
      fetch_error <<- conditionMessage(e)
      empty_kiwis_observations()
    }
  )

  current_year_raw <- bind_rows(
    old_current_raw %>% mutate(.source_priority = 2L),
    fresh_current_raw %>% mutate(.source_priority = 1L)
  ) %>%
    filter(!is.na(time), !is.na(observation_date),
           observation_date >= current_start, observation_date <= today, is.finite(value)) %>%
    arrange(time, .source_priority) %>%
    distinct(time, .keep_all = TRUE) %>%
    select(-.source_priority) %>%
    arrange(time)

  if (!is.null(fetch_error)) {
    warning("Current-year KiWIS retrieval failed for ", station_label, ": ", fetch_error, call. = FALSE)
  }
  rainfall_data <- prepare_rainfall_data(current_year_raw)
  current_cumulative <- prepare_current_cumulative(current_year_raw, current_start_year)
  cumulative_data <- bind_rows(historical_data, current_cumulative) %>%
    arrange(hydro_start_year, time)
  summary_data <- calculate_summary(rainfall_data, cumulative_data)

  rainfall_query_ok <- is.null(fetch_error) && nrow(fresh_current_raw) > 0L
  cumulative_query_ok <- is.null(fetch_error) && nrow(current_year_raw) > 0L
  cumulative_fetch_error <- if (!is.null(fetch_error)) fetch_error else if (!nrow(current_year_raw)) {
    "No current-year KiWIS rainfall observations were returned."
  } else NA_character_
  rainfall_fetch_error <- if (!is.null(fetch_error)) fetch_error else if (!nrow(fresh_current_raw)) {
    "No new current-year rainfall observations were returned during this refresh."
  } else NA_character_

  has_any_data <- nrow(rainfall_data) > 0L || nrow(cumulative_data) > 0L || nrow(average_data) > 0L
  if (has_any_data || is.null(old)) {
    result <- list(
      station_label = station_label,
      station_no = station_no,
      ts_id = ts_id,
      profile_source_method = "github_site_workbook_v2_direct_fallback",
      site_profile_path = if (is.na(site_workbook_path)) NA_character_ else site_workbook_path,
      colour_code_path = if (is.na(colour_code_path)) NA_character_ else colour_code_path,
      colour_code_error = colour_code_error,
      github_tree_error = github_tree_error,
      github_historical_data = historical_data,
      average_data = average_data,
      selected_hydro_years = selected_years,
      loaded_hydro_years = years_loaded,
      missing_hydro_years = years_missing,
      average_source_method = "github_site_workbook_average_column",
      current_start_year = as.integer(current_start_year),
      current_year_raw = current_year_raw,
      rainfall_data = rainfall_data,
      cumulative_data = cumulative_data,
      summary_data = summary_data,
      refreshed_at = Sys.time(),
      current_rainfall_refreshed_at = if (is.null(fetch_error)) Sys.time() else old_refreshed_at,
      refresh_date = today,
      rainfall_query_ok = rainfall_query_ok,
      cumulative_query_ok = cumulative_query_ok,
      average_query_ok = profile_query_ok,
      rainfall_fetch_error = rainfall_fetch_error,
      cumulative_fetch_error = cumulative_fetch_error,
      average_fetch_error = profile_error
    )
    save_rds_safely(result, cache_path)
  } else {
    warning("No usable new data for ", station_label, "; retaining the existing cache.", call. = FALSE)
  }

  saved <- if (file.exists(cache_path)) tryCatch(readRDS(cache_path), error = function(e) NULL) else NULL
  saved_rain_count <- if (!is.null(saved$rainfall_data)) nrow(saved$rainfall_data) else 0L
  saved_cum_count <- if (!is.null(saved$cumulative_data)) nrow(saved$cumulative_data) else 0L
  saved_average_count <- if (!is.null(saved$average_data)) nrow(saved$average_data) else 0L
  cached_years <- if (!is.null(saved$loaded_hydro_years)) saved$loaded_hydro_years else years_loaded

  log_rows[[i]] <- data.frame(
    Label = station_label,
    station_no = ifelse(is.na(station_no), "", station_no),
    ts_id = ts_id,
    site_workbook = ifelse(is.na(site_workbook_path), "", site_workbook_path),
    colour_code_error = ifelse(is.na(colour_code_error), "", colour_code_error),
    profile_error = ifelse(is.na(profile_error), "", profile_error),
    github_tree_error = ifelse(is.na(github_tree_error), "", github_tree_error),
    rainfall_values = saved_rain_count,
    cumulative_values = saved_cum_count,
    current_rainfall_updated = rainfall_query_ok,
    current_cumulative_updated = cumulative_query_ok,
    rainfall_error = ifelse(is.na(rainfall_fetch_error), "", rainfall_fetch_error),
    cumulative_error = ifelse(is.na(cumulative_fetch_error), "", cumulative_fetch_error),
    average_values = saved_average_count,
    selected_hydro_years = if (length(selected_years)) paste(selected_years, collapse = ";") else "",
    loaded_hydro_years = if (length(cached_years)) paste(cached_years, collapse = ";") else "",
    missing_hydro_years = if (length(years_missing)) paste(years_missing, collapse = ";") else "",
    average_updated = profile_query_ok,
    average_error = ifelse(is.na(profile_error), "", profile_error),
    stringsAsFactors = FALSE
  )
}

refresh_log <- bind_rows(log_rows)
write.csv(refresh_log, file.path(CACHE_DIR, paste0("refresh_log_", format(today, "%Y%m%d"), ".csv")),
          row.names = FALSE, na = "")
save_rds_safely(
  list(refreshed_at = Sys.time(), refresh_date = today,
       station_count = nrow(stations), log = refresh_log),
  file.path(CACHE_DIR, "last_refresh_status.rds")
)

message("Daily refresh finished. Stations: ", nrow(stations),
        "; per-station cache files present: ", sum(file.exists(vapply(seq_len(nrow(stations)), function(i) {
          station_cache_file(stations$station_no[[i]], stations$ts_id[[i]])
        }, character(1)))),
        "; with recent/current-year rainfall: ", sum(refresh_log$rainfall_values > 0),
        "; with Average profile data: ", sum(refresh_log$average_values > 0),
        "; historical year curves loaded: ", sum(nzchar(refresh_log$loaded_hydro_years)))
message("Diagnostic log: ", file.path(CACHE_DIR, paste0("refresh_log_", format(today, "%Y%m%d"), ".csv")))
