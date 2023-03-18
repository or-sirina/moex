#get securities data from moex
get_security <- function(security, from_date, till_date, out_format, to_table = TRUE){

  print("Checking libraries")
  Sys.sleep(0.5)
  is_httr <- require("httr", quietly = TRUE)
  is_jsonlite <- require("jsonlite", quietly = TRUE)
  is_readr <- require("readr", quietly = TRUE)

  if(is_httr == FALSE) {
    print("ERROR: install 'httr' package")
    stop()
  }
  if(is_jsonlite == FALSE) {
    print("ERROR: install 'jsonlite' package")
    stop()
  }
  if(is_readr == FALSE) {
    print("ERROR: install 'readr' package")
    stop()
  }

  formfile <- c("csv", "json") #accepted data output
  out_format <- tolower(out_format)
  security <- tolower(security)
  from_date <- as.Date(from_date)
  till_date <- as.Date(till_date)

  print("Checking output format") #check accepted formats
  Sys.sleep(0.5)
  if(out_format %in% formfile) {
    print(out_format)
  } else {
      print("Error: wrong output format")
    stop()
  }

  print("Checking time format")
  Sys.sleep(0.5)
  if(isTRUE(tryCatch(!is.na(as.Date(from_date, "%y-%m-%d")),
                     error = function(err) {FALSE}))) {
    print(from_date)
  } else {
    print("Error: wrong from_time format")
    stop()
  }

  if(isTRUE(tryCatch(!is.na(as.Date(till_date, "%y-%m-%d")),
                     error = function(err) {FALSE}))) {
    print(till_date)
  } else {
    print("Error: wrong to_time format")
    stop()
  }
  Sys.sleep(0.5)
  print("Downloading data")
  baseurl <-"http://iss.moex.com/iss/history/engines/stock/markets/shares/boards/tqbr/securities/"
  furl <- paste0(baseurl, sep = "", security, sep = ".", out_format, sep = "?",
                 "from=", sep = "", from_date, sep = "&",
                 "till=", sep = "", till_date)
  data <- GET(furl)

  if(to_table == FALSE) {
    return(data)
  } else if(out_format == "csv") { #response from csv
    data <- content(data, 'text')
    data <- gsub('[history]', '', data)
    data <- read_delim(data, delim = ";")
    return(data)
  } else if(out_format == "json") { #response from json
    data = fromJSON(rawToChar(data$content))
    namecol <- data$history$columns
    data <- as.data.frame(data$history$data)
    colnames(data) <- namecol
    return(data)
  }
}



