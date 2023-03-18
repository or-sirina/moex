\name{get_security}
\alias{get_security}
\title{Get securities from MOEX}
\usage{
get_security(security, from_date, till_date, out_format, to_table)
}
\arguments{
\item{security}{Name of security (symbols). For example, ABRD - "Abrau Durso."}
\item{from_date}{Starting date for harvesting data. Date format - YYYY-MM-DD}
\item{till_date}{End date for harvesting data. Date format - YYYY-MM-DD.}
\item{out_format}{Output format in .csv ot .json.}
\item{to_table}{Optional. Lodical: Convert to dataframe or not?}
}
\description{
This function gets security data from MOEX and coverts API request to table format.
}
\examples{
security <- get_security("ABRD", "2019-01-01", "2020-01-01", "csv", to_table = TRUE) #get data
moexplot(security) #print plot
}
\author{
OrSirina. GitHub - https://github.com/or-sirina/moex
}
\concept{ ~moex }
\concept{ ~finance }
\concept{ ~API }
