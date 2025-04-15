library(httr)
library(jsonlite)
library(xml2)
library(XML)
library(tibble)

#как парсить xml гайд
url <- "https://iss.moex.com/iss/history/engines/stock/markets/shares/securities/MOEX?from=2023-01-01&till=2023-01-31&marketprice_board=1"
res <- GET(url)
data <- read_xml(rawToChar(res$content))
data_xml <- xmlParse(data)
data_xml
xml_structure(data)
xml_find_all(data, ".//row")


xml_data <- read_xml(rawToChar(res$content))
rows <- xml_find_all(xml_data, "//data[@id='history']//rows/row")

attributes_list <- lapply(rows, function(row) {
  as.list(xml_attrs(row))
})

df <- do.call(rbind, lapply(attributes_list, as.data.frame))


print(df)