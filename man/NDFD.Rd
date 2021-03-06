% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NDFD.R
\name{NDFD}
\alias{NDFD}
\title{Instantiate a NDFRefClass object}
\usage{
NDFD(query = NULL,
  baseuri = "http://graphical.weather.gov/xml/sample_products/browser_interface",
  interface = c("ndfdXMLclient.php", "ndfdBrowserClientByDay")[1])
}
\arguments{
\item{query}{character string}

\item{baseuri}{character the base URI}

\item{interface}{the interface to use with the baseuri}
}
\value{
NDFDRefClass object
}
\description{
Instantiate a NDFRefClass object
}

