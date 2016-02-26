#' Given a named character vector, craft a query
#'
#' @export
#' @param x a named character vector
#' @param elements a character vector of elements to include
#' @return character query string
#' @examples
#' \dontrun{
#'    s <- c(lat = '45.0', lon = '-77', Unit = 'm') 
#'    elements <- c("mint", "maxt")
#'    build_query(s, elements = elements)
#'    [1] "lat=45.0&lon=-77&Unit=m&mint=mint&maxt=maxt"
build_query <- function(x, elements = NULL){
   if (!is.character(x)) x <- as.character(x) 
   r <- paste(paste0(names(x),"=",x), collapse = '&')
   if (!is.null(elements)) r <- paste(r, build_query_element(elements), sep = "&")
   r
}

#' Given an element list of names from standard list found 
#' \url{http://graphical.weather.gov/xml/docs/elementInputNames.php}
#' @export
build_query_element <- function(x = c('mint', 'max', 'temp')){
   names(x) <- x
   build_query(x)
}


####
#  ndfdXMLclient.php Interface
####


#' Single Point Unsummarized Data
#' Returns DWML-encoded NDFD data for a point
#' @export
query_point<- function(
   lat = 39, 
   lon = -77,
   product = 'time-series',
   begin = '', end = '',
   Unit = 'm',
   element = c('mint', 'max', 'temp') ){
     
}

#' Multiple Point Unsummarized Data
#' Returns DWML-encoded NDFD data for a list of points
#' @export
query_multipoint <- function(
   listlatlon = '38.99,-77.02 39.70,-104.80',
   product = 'time-series',
   begin = '', end = '',
   Unit = 'm',
   element = c('mint', 'max', 'temp') ) {
}  	 

#' Unsummarized Data for a Subgrid
#'
#' Returns DWML-encoded NDFD data for a subgrid defined by a lower left and upper right point
#'
#' @export
query_subgrid<- function(
   lat1 = 33.8835, 
   lon1 = -80.0679, 
   lat2 = 33.8835, 
   lon2 = -80.0679,
   resolutionSub = 20.0,
   product = 'time-series',
   begin = '', 
   end = '',
   Unit = 'm',
   element = c('mint', 'max', 'temp') ){
}

#' A List of NDFD Points for a Subgrid
#'
# Returns the WGS84 latitude and longitude values of all the NDFD grid points
#' within a rectangular subgrid as defined by points at the lower left and upper
#' right corners of the rectangle. The returned list of points is suitable for
#' use in inputs listLatLon and gmlListLatLon. NOTE: The subgrid locations will
#' only form a rectangle when viewed in the NDFD projection applicable to the
#' grid.
#'
#' @export
list_points_in_subgrid <- function(
   listLat1 = 33.8835,
   listLon1 = -80.0679,
   listLat2 = 33.8835,
   listLon2 = -80.0679,
   resolutionList = 20.0
   ){
}
   
#' Unsummarized Data for a Line
#'
#' Returns DWML-encoded NDFD data for a line of points defined by the two end 
#'point
#'
#' @export
query_line <- function(
   endPoint1Lat = 39.0000,
   endPoint1Lon = -77.0000,
   endPoint2Lat = 39.0000,
   endPoint2Lon = -77.0000,
   product = "time-series",
   begin = "2004-04-27T12:00",
   end = "2004-04-30T12:00",
   Unit = "m",
   element = c('mint', 'max', 'temp')){
}

#' A List of NDFD Points for a Line
#'
#' Returns the WGS84 latitude and longitude values for all points on a line
#' defined by the line's end points. The returned list of points is suitable
#' for use in inputs listLatLon and gmlListLatLon. NOTE: The list of locations
#' will only form a straight line when viewed in the NDFD projection applicable
#' to the grid.
#'
#' @export
list_points_on_line <- function(
   endPoint1Lat = 39.0000,
   endPoint1Lon = -77.0000,
   endPoint2Lat = 39.0000,
   endPoint2Lon = -77.0000
   ){
}

#' Unsummarized Data for One or More Zipcodes
#' 
#' Returns DWML-encoded NDFD data for one or more zip codes (50 United States
#' and Puerto Rico). The returned list of points is suitable for use in inputs
#' listLatLon and gmlListLatLon.
#'
#' @export
query_zipcodes <- function(
   zipCodeList = "20910+25414",
   product = "time-series",
   begin = "2004-04-27T12:00",
   end = "2004-04-30T12:00",
   Unit = "m",
   element = c('mint', 'max', 'temp')){
}

#' A List of NDFD Points for a Zipcode
#'
#' Returns the WGS84 latitude and longitude values for one or more zip codes
#' (50 United States and Puerto Rico). The returned list of points is suitable
#' for use in inputs listLatLon and gmlListLatLon.
#'
#' @export
list_zipcodes <- function(
   istZipCodeList = "20910+25414") {
}

#' Unsummarized Data for a List of Cities
#' 
#' Returns DWML-encoded NDFD data for a predefined list of cities. The cities
#' are grouped into a number of subsets to facilitate requesting data. You can
#' view the cities in each group by clicking on the links in the table below.
#'
#' @export
query_cityies <- function(
   citiesLevel = '12',
   product = "time-series",
   begin = "2004-04-27T12:00",
   end = "2004-04-30T12:00",
   Unit = "m",
   element = c('mint', 'max', 'temp')){
   
}

# A List of NDFD Points for a List of Cities
#'
# Returns the WGS84 latitude and longitude values for a predefined list of 
#'cities. The cities are grouped into a number of subsets to facilitate
#' requesting data. You can view the cities in each group by clicking on the
#' links in the table below. The returned list of pointsis suitable for input
#' into NDFDgenLatLonList(), NDFDgenByDayLatLonList(), and GmlLatLonList()
#' which will return NDFD data for those points.
#'
#' @export
list_cities <- function(
   citiesLevel = '12'
   ){
}

# Unsummarized Data for a Subgrid Defined by a Center Point
#'
#' Returns DWML-encoded NDFD data for a rectangle defined by a center point and
#' distances in the latitudinal and longitudinal directions.
#'
#' @export
query_centerpoint <- function(
   centerPointLat = 39.0000,
   centerPointLon = -77.0000,
   distanceLat = 50.0,
   distanceLon = 50.0,
   resolutionSquare = 20.0,
   product = 'time-series',
   begin = '', 
   end = '',
   Unit = 'm',
   element = c('mint', 'max', 'temp')
   ){
}

# A List of NDFD Points for a Subgrid Defined by a Center Point
#'
#' Returns the WGS84 latitude and longitude values for a rectangle defined by a
#' center point and distances in the latitudinal and longitudinal directions.
#' The returned list of points is suitable for use in inputs listLatLon and 
#' gmlListLatLon. NOTE: The subgrid locations will only form a rectangle when 
#' viewed in the NDFD projection applicable to the grid.
#'
#' @export
list_centerpoint <- function(
   listCenterPointLat = 39.0000,
   listCenterPointLon = -77.0000,
   listDistanceLat = 50.0,
   listDistanceLon = 50.0,
   listResolutionSquare = 20.0
   ){
}

# A List of NDFD Points for the Corners of an NDFD Grid
#'
# Returns the WGS84 latitude and longitude values for the corners of an NDFD
#' grid as well as the resolution required to retrieve the entire grid and
#' still stay under the maximum allowed point restriction.
#'
#' @export
list_corners <- function(
   sector = 'conus'){
}

#' Unsummarized Data for a Single Time Encoded in dwGML
#'
# Returns Digital Weather Geography Markup Language (dwGML) encoded NDFD data
#' for a list of points at a single valid time.
#'
#' @param compType Comparison type. Can be IsEqual, Between, GreatThan, GreaterThanOrEqual, LessThan, or LessThanOrEqual. 
#' @param featureType 
#'    GML 2 Compliant Data Structure: Forecast_Gml2Point
#'    GML 3 Compliant Data Structures: Forecast_GmlsfPoint, Forecast_GmlObs, NdfdMultiPointCoverage
#'    KML 2 Compliant Data Structure: Ndfd_KmlPoint
#'
#' @export
query_single_time <- function(
   gmlListLatLon = "38.99,-77.02 39.70,-104.80",
   compType = "Between",
   featureType= "Forecast_Gml2Point", 
   begin = '', 
   end = '',
   element = c('mint', 'max', 'temp')){
}

###
#  ndfdBrowserClientByDay.php Interface
###
