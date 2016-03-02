#' ndfd-package
#'
#' @name ndfd-package
#' @aliases ndfd
#' @docType package
#' @import methods httr XML
#' @description A RESTful R interface to the NWS National Digital Forecast Database#' @seealso \url{http://graphical.weather.gov/xml/rest.php}
NULL

#' The basic query terms for most searches
#'
#' @format a character vector of 4 elements.
#' \itemize{
#'  \item product
#'  \item begin
#'  \item end
#'  \item Unit
#'  }
#' @source \url{http://graphical.weather.gov/xml/rest.php#XML_contents}
"ndfdXMLclient_basic"

#' Query search terms groups by the type of search
#' 
#' It is best to study the source and the structure of this list to get a sense
#'  of the types of queries and the related search terms.
#'
#' @format a named list of character vectors
#' \itemize{
#'  \item query_point
#'  \item query_multipoint
#'  \item query_subgrid
#'  \item list_points_in_subgrid
#'  \item query_line
#'  \item list_points_on_line
#'  \item query_zipcodes
#'  \item list_zipcodes
#'  \item query_cities
#'  \item list_cities
#'  \item query_centerpoint
#'  \item list_centerpoint
#'  \item list_corners
#'  \item query_single_time
#'  }
#' @source \url{http://graphical.weather.gov/xml/rest.php#XML_contents}
"ndfdXMLclient_groups"

#' The available variables for ndfdXMLclient client queries.
#'
#' Variable are coded by short keywords. With the exceptions of \code{begin}, 
#'  \code{end} and \code{Unit} all are the values used in the examples at the
#'  source.  A complete list of \code{element} can be found here 
#'  \url{http://graphical.weather.gov/xml/docs/elementInputNames.php}
#'
#' @format a list with 99 variable identifiers and default values
#' \describe{
#   \item{lat}{39}
#   \item{lon}{-77}
#   \item{product}{time-series}
#   \item{begin}{}
#   \item{end}{}
#   \item{Unit}{m}
#   \item{element}{c("mint", "maxt", "temp")}
#   \item{listlatlon}{38.99,-77.02 39.70,-104.80}
#   \item{lat1}{33.8835}
#   \item{lon1}{-80.0679}
#   \item{lat2}{33.8835}
#   \item{lon2}{-80.0679}
#   \item{resolutionSub}{20}
#   \item{listLat1}{33.8835}
#   \item{listLon1}{-80.0679}
#   \item{listLat2}{33.8835}
#   \item{listLon2}{-80.0679}
#   \item{resolutionList}{20}
#   \item{endPoint1Lat}{39}
#   \item{endPoint1Lon}{-77}
#   \item{endPoint2Lat}{39}
#   \item{endPoint2Lon}{-77}
#   \item{zipCodeList}{20910+25414}
#   \item{listZipCodeList}{20910+25414}
#   \item{citiesLevel}{12}
#   \item{centerPointLat}{39}
#   \item{centerPointLon}{-77}
#   \item{distanceLat}{50}
#   \item{distanceLon}{50}
#   \item{resolutionSquare}{20}
#   \item{listCenterPointLat}{39}
#   \item{listCenterPointLon}{-77}
#   \item{listDistanceLat}{50}
#   \item{listDistanceLon}{50}
#   \item{listResolutionSquare}{20}
#   \item{sector}{conus}
#   \item{gmlListLatLon}{38.99,-77.02 39.70,-104.80}
#   \item{compType}{Between}
#   \item{featureType}{Forecast_Gml2Point}
#' }
#' @source \url{http://graphical.weather.gov/xml/rest.php#XML_contents}
"ndfdXMLclient_vars"



#' The available input elements for queries
#' 
#' @format a named 55 element character vector
#' \describe{
#'  \item{maxt}{Maximum Temperature }
#'  \item{mint}{Minimum Temperature }
#'  \item{temp}{3 Hourly Temperature }
#'  \item{dew}{Dewpoint Temperature }
#'  \item{appt}{Apparent Temperature }
#'  \item{pop12}{12 Hour Probability of Precipitation }
#'  \item{qpf}{Liquid Precipitation Amount }
#'  \item{snow}{Snowfall Amount }
#'  \item{sky}{Cloud Cover Amount }
#'  \item{rh}{Relative Humidity }
#'  \item{wspd}{Wind Speed }
#'  \item{wdir}{Wind Direction }
#'  \item{wx}{Weather }
#'  \item{icons}{Weather Icons }
#'  \item{waveh}{Wave Height }
#'  \item{incw34}{Probabilistic Tropical Cyclone Wind Speed >34 Knots (Incremental) }
#'  \item{incw50}{Probabilistic Tropical Cyclone Wind Speed >50 Knots (Incremental) }
#'  \item{incw64}{Probabilistic Tropical Cyclone Wind Speed >64 Knots (Incremental) }
#'  \item{cumw34}{Probabilistic Tropical Cyclone Wind Speed >34 Knots (Cumulative) }
#'  \item{cumw50}{Probabilistic Tropical Cyclone Wind Speed >50 Knots (Cumulative) }
#'  \item{cumw64}{Probabilistic Tropical Cyclone Wind Speed >64 Knots (Cumulative) }
#'  \item{wgust}{Wind Gust }
#'  \item{critfireo}{Fire Weather from Wind and Relative Humidity }
#'  \item{dryfireo}{Fire Weather from Dry Thunderstorms }
#'  \item{conhazo}{Convective Hazard Outlook }
#'  \item{ptornado}{Probability of Tornadoes }
#'  \item{phail}{Probability of Hail }
#'  \item{ptstmwinds}{Probability of Damaging Thunderstorm Winds }
#'  \item{pxtornado}{Probability of Extreme Tornadoes }
#'  \item{pxhail}{Probability of Extreme Hail }
#'  \item{pxtstmwinds}{Probability of Extreme Thunderstorm Winds }
#'  \item{ptotsvrtstm}{Probability of Severe Thunderstorms }
#'  \item{pxtotsvrtstm}{Probability of Extreme Severe Thunderstorms }
#'  \item{tmpabv14d}{Probability of 8- To 14-Day Average Temperature Above Normal }
#'  \item{tmpblw14d}{Probability of 8- To 14-Day Average Temperature Below Normal }
#'  \item{tmpabv30d}{Probability of One-Month Average Temperature Above Normal }
#'  \item{tmpblw30d}{Probability of One-Month Average Temperature Below Normal }
#'  \item{tmpabv90d}{Probability of Three-Month Average Temperature Above Normal }
#'  \item{tmpblw90d}{Probability of Three-Month Average Temperature Below Normal }
#'  \item{prcpabv14d}{Probability of 8- To 14-Day Total Precipitation Above Median }
#'  \item{prcpblw14d}{Probability of 8- To 14-Day Total Precipitation Below Median }
#'  \item{prcpabv30d}{Probability of One-Month Total Precipitation Above Median }
#'  \item{prcpblw30d}{Probability of One-Month Total Precipitation Below Median }
#'  \item{prcpabv90d}{Probability of Three-Month Total Precipitation Above Median }
#'  \item{prcpblw90d}{Probability of Three-Month Total Precipitation Below Median }
#'  \item{precipa_r}{Real-time Mesoscale Analysis Precipitation }
#'  \item{sky_r}{Real-time Mesoscale Analysis GOES Effective Cloud Amount }
#'  \item{td_r}{Real-time Mesoscale Analysis Dewpoint Temperature }
#'  \item{temp_r}{Real-time Mesoscale Analysis Temperature }
#'  \item{wdir_r}{Real-time Mesoscale Analysis Wind Direction }
#'  \item{wspd_r}{Real-time Mesoscale Analysis Wind Speed }
#'  \item{wwa}{Watches, Warnings, and Advisories }
#'  \item{iceaccum}{Ice Accumulation }
#'  \item{maxrh}{Maximum Relative Humidity }
#'  \item{minrh}{Minimum Relative Humidity }
#' }
#' @source \url{http://graphical.weather.gov/xml/docs/elementInputNames.php}
"ndfdElementInputNames"
