read_csv_column <- function(filename) {
  if (!file.exists(filename)) {
    cat("Файл не знайдено.\n")
    return(numeric(0))
  }

  data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  if (!"Price" %in% names(data)) {
    cat("Стовпець 'Price' не знайдено у файлі.\n")
    return(numeric(0))
  }

  price <- as.numeric(gsub(",", "", data$Price))
  clean_price <- price[!is.na(price)]

  if (length(clean_price) == 0) {
    cat("Файл порожній або не містить числових значень.\n")
  }

  return(clean_price)
}


generate_alphabet <- function(size = NULL, symbols = NULL) {
  if (!is.null(symbols)) {
    return(symbols)
  } else if (!is.null(size)) {
    return(sapply(1:size, function(i) LETTERS[i]))
  } else {
    stop("Потрібно вказати або розмір, або символи алфавіту.")
  }
}


get_intervals_uniform <- function(series, alpha_size) {
  breaks <- seq(min(series), max(series), length.out = alpha_size + 1)
  return(breaks)
}


map_to_alphabet <- function(series, breaks, alphabet) {
  intervals <- cut(series, breaks = breaks, include.lowest = TRUE, labels = alphabet)
  return(as.character(intervals))
}


build_transition_matrix <- function(ling_series, alphabet) {
  size <- length(alphabet)
  matrix <- matrix(0, nrow = size, ncol = size, dimnames = list(alphabet, alphabet))

  for (i in 1:(length(ling_series) - 1)) {
    current <- ling_series[i]
    next_state <- ling_series[i + 1]
    matrix[current, next_state] <- matrix[current, next_state] + 1
  }

  return(matrix)
}


print_ling_series <- function(ling_series) {
  cat("Лінгвістичний ряд:\n")
  cat(paste(ling_series, collapse = ""))
  cat("\n")
}


print_matrix <- function(matrix) {
  cat("Матриця передування:\n")
  print(matrix)
}


main <- function(
    filename = "B-C-D-E-F-Brent Oil Futures Historical Data.csv",
    alphabet_size = 5,
    alphabet_symbols = NULL
) {
  series <- read_csv_column(filename)
  if (length(series) == 0) return()

  alphabet <- generate_alphabet(size = alphabet_size, symbols = alphabet_symbols)
  breaks <- get_intervals_uniform(series, length(alphabet))
  ling_series <- map_to_alphabet(series, breaks, alphabet)
  matrix <- build_transition_matrix(ling_series, alphabet)

  print_ling_series(ling_series)
  print_matrix(matrix)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  filename <- args[1]
} else {
  filename <- "B-C-D-E-F-Brent Oil Futures Historical Data.csv"
}

if (length(args) >= 2) {
  alphabet_size <- as.numeric(args[2])
  if (is.na(alphabet_size)) {
    cat("Неправильне значення для alphabet_size, встановлено за замовчуванням 5\n")
    alphabet_size <- 5
  }
} else {
  alphabet_size <- 5
}

if (length(args) >= 3) {
  alphabet_symbols <- unlist(strsplit(args[3], ","))
} else {
  alphabet_symbols <- NULL
}

main(filename, alphabet_size, alphabet_symbols)
