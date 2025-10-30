#'##################################################################################################
#'
#' Bently's Helper Functions
#'
#' Author: Bently Bartee
#' Copy-write is me
#'
#'##################################################################################################


#'---------------------------------------------------------------------------------------------------
#' Cleans a vector of text by removing punctuation, extra whitespace, leading/trailing whitespace,
#' and non-alphanumeric characters (keeping spaces and apostrophes)

clean_UTF_8_Remove_Special_Charc <- function(text_vector) {
  # Cleans a vector of text by sequentially applying the specified gsub operations.
  # 1. Replace punctuation and one or more whitespace characters with a single space.
  cleaned_vector <- gsub('([[:punct:]])|\\s+', ' ', text_vector)
  # 2. Remove leading/trailing whitespace and reduce multiple spaces to a single space.
  cleaned_vector <- gsub("^\\s+|\\s+$|\\s{2,}", "", cleaned_vector)
  # 3. Remove characters that are not alphanumeric, '/', or apostrophes (case-insensitive).
  cleaned_vector <- gsub("[^a-zA-Z0-9/' ]", "", cleaned_vector, ignore.case = TRUE)
  return(cleaned_vector)
}


#'---------------------------------------------------------------------------------------------------
#' Asks the user whether they want to see the first or the last day of the month and stores their input

ask_month_StartDay_EndDay <- function() {
  choice <- readline(prompt = "Do you want to see the First day of the month or the Last day of the month? (First/Last): ")
  date_input <- as.Date(readline(prompt = "Enter a date (YYYY-MM-DD): "))
  result <- NULL
  
  Start.of.Month <- function(x) {
    as.Date(format(x, "%Y-%m-01"))
  }
  End.of.Month <- function(x) {
    Start.of.Month(Start.of.Month(x) + 35) - 1
  }
  
  if (tolower(choice) == "first") {
    result <- Start.of.Month(date_input)
    cat("The first day of the month is:",
        format(result, "%Y-%m-%d"),
        "\n")
  } else if (tolower(choice) == "last") {
    result <- End.of.Month(date_input)
    cat("The last day of the month is:",
        format(result, "%Y-%m-%d"),
        "\n")
  } else {
    cat("Invalid choice. Please enter 'First' or 'Last'.\n")
  }
  # This means the result won't be automatically printed again unless you explicitly assign the
  # output of the function to a variable and then print that variable.
  return(invisible(result))
}


#'---------------------------------------------------------------------------------------------------
#' Number of days in a month

number_of_Days_Month <- function(date) {
  # Calculates the number of days in the month of a given date using base R.
  days_in_month <- function(date) {
    # Helper function to get the number of days in a month (including leap years).
    year <- as.numeric(format(date, "%Y"))
    month <- as.numeric(format(date, "%m"))
    if (month == 2) {
      ifelse((year %% 4 == 0 &
                year %% 100 != 0) | year %% 400 == 0, 29, 28)
    } else if (month %in% c(4, 6, 9, 11)) {
      30
    } else {
      31
    }
  }
  #---------
  year_month <- format(date, "%Y-%m")
  first_day <- as.Date(paste0(year_month, "-01"))
  last_day <- as.Date(paste0(year_month, "-", days_in_month(first_day)))
  return(as.integer(format(last_day, "%d")))
}


#'---------------------------------------------------------------------------------------------------
#' Excels Left, Right, or Mid function for getting certain text or numbers

excel_Left_Right_Mid_operations <- function() {
  operation_choice <- readline(prompt = "Do you want to use Left, Right, or Mid function? ")
  text_input <- readline(prompt = "Enter the text: ")
  result <- ""
  if (tolower(operation_choice) == "left") {
    num_char <- as.integer(readline(prompt = "Enter the number of characters from the left: "))
    result <- substr(text_input, 1, num_char)
  } else if (tolower(operation_choice) == "right") {
    num_char <- as.integer(readline(prompt = "Enter the number of characters from the right: "))
    result <- substr(text_input,
                     nchar(text_input) - (num_char - 1),
                     nchar(text_input))
  } else if (tolower(operation_choice) == "mid") {
    start_num <- as.integer(readline(prompt = "Enter the starting position for the middle extraction: "))
    num_char <- as.integer(readline(prompt = "Enter the number of characters to extract from the middle: "))
    result <- substr(text_input, start_num, start_num + num_char - 1)
  } else {
    cat("Invalid choice. Please enter Left, Right, or Mid.\n")
  }
  return(result)
}


#'---------------------------------------------------------------------------------------------------
#' Checks for NA values in a data frame or matrix and returns results in a data frame.

perform_NA_checks_df <- function(data) {
  # Checks for NA values in a data frame or matrix and returns results in a data frame.
  NACheck1 <- is.na(data) # Logical matrix indicating NA values
  NACheck2 <- any(NACheck1) # TRUE if any NA values exist
  NACheck3 <- which(NACheck1 == TRUE, arr.ind = TRUE) # Row and column indices of NA values
  # Convert NA indices to a data frame
  if (nrow(NACheck3) > 0) {
    na_indices_df <- as.data.frame(NACheck3)
  } else {
    na_indices_df <- data.frame(row = integer(0), col = integer(0))
  }
  results_df <- data.frame(
    any_na_present = NACheck2,
    num_na_values = sum(NACheck1),
    stringsAsFactors = FALSE
  )
  if (nrow(na_indices_df) > 0) {
    results_df <- cbind(results_df, num_na_locations = nrow(na_indices_df))
    results_df <- cbind(results_df, na_locations = I(list(na_indices_df))) # Store the data frame of locations in a list column
  } else {
    results_df <- cbind(results_df, num_na_locations = 0)
    results_df <- cbind(results_df, na_locations = I(list(
      data.frame(row = integer(0), col = integer(0))
    )))
  }
  return(results_df)
}


#'---------------------------------------------------------------------------------------------------
#' Creates a data frame that shows columns for Month, year, StartDay, EndDay for each year you list

generate_month_data <- function(years) {
  all_months <- 1:12
  month_data <- data.frame(
    Month = rep(month.name, length(years)),
    Year = unlist(sapply(years, function(yr)
      rep(
        yr, length(all_months)
      ))),
    StartDay = as.Date(paste0(
      rep(years, each = length(all_months)),
      "-",
      sprintf("%02d", all_months),
      "-01"
    )),
    EndDay = as.Date(paste0(
      rep(years, each = length(all_months)),
      "-",
      sprintf("%02d", all_months),
      "-",
      ifelse(
        sprintf("%02d", all_months) == "02",
        ifelse(
          rep(years, each = length(all_months)) %% 4 == 0 &
            (
              rep(years, each = length(all_months)) %% 100 != 0 |
                rep(years, each = length(all_months)) %% 400 == 0
            ),
          29,
          28
        ),
        ifelse(
          sprintf("%02d", all_months) %in% c("04", "06", "09", "11"),
          30,
          31
        )
      )
    ))
  )
  return(month_data)
}


#---------------------------------------------------------------------------------------------------
#Read and import all tabs of an excel sheet .xlsx, requires package 'openxlsx'

read_all_sheets <- function(xlsxFile, ...) {
  # Check if the package is installed
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    # Install the package if not installed
    install.packages("openxlsx")
  }
  # Load the library
  library(openxlsx)
  sheet_names <- getSheetNames(xlsxFile)
  sheet_list <- sapply(sheet_names, function(sn) {
    read.xlsx(xlsxFile, sheet = sn, ...)
  }, USE.NAMES = TRUE, simplify = FALSE)
  return(sheet_list)
}

#'---------------------------------------------------------------------------------------------------
#' find all excel type of files in a folder, Looks for .xlsx, .xls, .csv files

find_All_Excel_files <- function(path) {
  # Looks for .xlsx, .xls, .csv files recursively
  files <- list.files(path,
                      pattern = "\\.(xlsx|xls|csv)$",
                      full.names = TRUE,
                      recursive = TRUE)
  if (length(files) > 0) {
    return(files)
  } else {
    print("No Excel or CSV files found in the specified path or its subdirectories.")
    return(character(0)) # Return an empty character vector if no files are found
  }
}


#'---------------------------------------------------------------------------------------------------
#' reads all CSV files within a specified directory, combines their data into a single data frame,
#' and handles potential errors during the process.

read.All_CSVs <- function(filePath) {
  if (!require("data.table")) {
    install.packages("data.table")
    library(data.table)
  }
  # Input validation
  if (!dir.exists(filePath)) {
    stop("Directory path does not exist")
  }
  # Get CSV files
  csv_files <- dir(filePath, pattern = "*.csv$", full.names = TRUE)
  # Display files and wait for confirmation
  cat("Found CSV files:\n")
  cat(csv_files, sep = "\n")
  cat("\n\nPress Enter to continue (or ESC to stop): ")
  # Handle ESC key press
  response <- readline()
  if (tolower(substr(response, 1, 1)) == "esc") {
    return(NULL)
  }
  # Initialize data.table
  csv.data <- c()
  for (i in 1:NROW(csv_files)) {
    tryCatch({
      current_data <- fread(csv_files[i], quote = "")
      
      csv <- read.csv(csv_files[i])
      csv.data <- rbind(csv.data, csv)
      
    }, error = function(e) {
      warning(sprintf("Error processing %s: %s", filePath, e$message))
    })
    
  }
  return(csv.data)
}
