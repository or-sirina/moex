\name{moexplot}
\alias{moexplot}
\title{Security prices plot}
\description{
Plot data from get_security() function with ggplot.
}
\usage{
moexplot(df, colors)
}
\arguments{
  \item{df}{Dataframe from get_security() function}
  \item{colots}{Optional. What colors moexplot() shound use for visualisation? Use c() to set colors}
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
\concept{ ~visualisation }

