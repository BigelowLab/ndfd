#' Check an \code{httr::response} class object and possible pare to \code{XML::xmlNode}
#'
#' @export
#' @param rsp \code{httr::response} class object
#' @param encoding character, by defalt 'UTF-8'
#' @return \code{xml} object
check_response <- function(rsp, encoding = "UTF-8"){
    w <- httr::http_error(rsp)
    if (w) {
        print(rsp)
        print(httr::content(rsp, as = "text", encoding = encoding)) 
    }
    
    x <- try(httr::content(rsp, as = "text", encoding = encoding))
    if (inherits(x, 'try-error')){
        x  <- create_exception(problem = "error extracting response content")
        return(invisible(x))
    }
    x <- try(xml2::read_xml(x))
    if (inherits(x, "try-error")){
        x <- create_exception(problem = "error with read_xml")
        return(invisible(x))
    }
    
    x <- try(xml2::xml_root(x))
    if (inherits(x, "try-error")){
        x <- create_exception(problem = "error parsing response content with xml_root")
    }
    
    invisible(x)
} # check_response

#' Parse an \code{httr::response} class object to DWMLBaseRefClass object including
#'  an DWMLExceptionRefClass
#'
#' @export
#' @param r \code{httr::response} class object
#' @param form character specifies the type of data to return
#' @param ... further arguments for \code{check_response()}
#' @return object inheriting DWMLBaseRefClass
parse_response <- function(r, form = c('DWMLTopRefClass', 'xml')[1], ...){
    
    x <- check_response(r, ...)
    
    if (xml2::xml_name(x) == "error"){
    
        if (tolower(form[1]) == 'dwmltoprefclass') x <- DWMLExceptionRefClass$new(x)
        
    } else {
        
        if (tolower(form[1]) == 'dwmltoprefclass') x <- DWMLTopRefClass$new(x)
    
    }
    
    invisible(x)
}
    
#' Retrieve a response for a given query
#' 
#' @export
#' @family QUERY
#' @param query character string 
#' @param baseuri character the base URI
#' @param interface the interface to use with the baseuri
#' @param ... other arguments for \code{parse_response}
#' @return object inheriting DWMLBaseRefClass
get_query <- function(query,
    baseuri = "http://graphical.weather.gov/xml/sample_products/browser_interface",
    interface = c('ndfdXMLclient.php', 'ndfdBrowserClientByDay')[1], 
    ...){
    
 
    uri <- file.path(baseuri, interface)
    if (!is.null(query)){
        uri <- paste0(uri, "?", query)
    }
    
    r <- httr::GET(uri)
    return(parse_response(r, ...))

}

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
#' }
build_query <- function(x, elements = NULL){
   if (is.list(x)) x <- unlist(x) 
   ix <- names(x) %in% c('listLatLon', 'gmlListLatLon')
   if (any(ix)) x[ix] <- xml2::url_escape(x[ix])
   r <- paste(paste0(names(x),"=",x), collapse = '&')
   if (!is.null(elements)) r <- paste(r, build_query_element(elements), sep = "&")
   r
}

#' Given an element list of names from standard list found 
#' 
#' @seealso \url{http://graphical.weather.gov/xml/docs/elementInputNames.php}
#'
#' @export
#' @param a character vector of element names
#' @return query character string
build_query_element <- function(x = c('mint', 'max', 'temp')){
   names(x) <- x
   build_query(x)
}


####
#  ndfdXMLclient.php Interface
####

#' Undocumented query parameter (as far as I can tell) '?whichClient=xyz'
#' 
#' @export
#' @param w if not NA then retirve the client by this name, if NA return ALL
#' @return a named character vector of client names or just one
which_client <- function(w = 'query_point'){

    wc <- c(
        query_point = 'NDFDgen',
        query_multipoint = 'NDFDgenLatLonList',
        query_subgrid =  'NDFDgenSubgrid',
        list_points_in_subgrid =  'LatLonListSubgrid',
        query_line =  'NDFDgenLine',
        list_points_on_line =  'LatLonListLine',
        query_zipcodes = 'NDFDgenMultiZipCode',
        list_zipcodes =  'LatLonListZipCode',
        query_cities =  'NDFDgenMultiCities',
        list_cities =  'LatLonListCities',
        query_centerpoint =  'NDFDgenSquare',
        list_centerpoint =  'LatLonListSquare',
        list_corners = 'CornerPoints',
        query_single_time = "")
    
    if(!is.na(w[1])){
        ix <- names(wc) %in% w[1]
        wc <- if (!any(ix)) "" else unname(wc[ix])
    }
    wc
}

    
#' This function is used during development to create and save the data object
#'  \code{ndfdXMLclient_vars}.
#' 
#' @return a named list of default variable values
get_ndfdXMLclient_vars <- function(){
    list(
        lat = 39, 
        lon = -77,
        product = 'time-series',
        begin = '', end = '',
        Unit = 'm',
        element = c('mint', 'maxt', 'temp'),
        listLatLon = '38.99,-77.02 39.70,-104.80', 
        lat1 = 33.8835, 
        lon1 = -80.0679, 
        lat2 = 33.8835, 
        lon2 = -80.0679,
        resolutionSub = 20.0,
        listLat1 = 33.8835,
        listLon1 = -80.0679,
        listLat2 = 33.8835,
        listLon2 = -80.0679,
        resolutionList = 20.0,
        endPoint1Lat = 39.0000,
        endPoint1Lon = -77.0000,
        endPoint2Lat = 39.0000,
        endPoint2Lon = -77.0000,
        zipCodeList = "20910+25414",
        listZipCodeList = "20910+25414",
        citiesLevel = '12',
        centerPointLat = 39.0000,
        centerPointLon = -77.0000,
        distanceLat = 50.0,
        distanceLon = 50.0,
        resolutionSquare = 20.0,
        listCenterPointLat = 39.0000,
        listCenterPointLon = -77.0000,
        listDistanceLat = 50.0,
        listDistanceLon = 50.0,
        listResolutionSquare = 20.0,
        sector = 'conus',      
        gmlListLatLon = "38.99,-77.02 39.70,-104.80",
        compType = "Between",
        featureType= "Forecast_Gml2Point")
}

#' This function is used during development to create and save the data object
#'  \code{ndfdXMLclient_basic}.
#' 
#' @return a named chacter vector of basic data elements for queries
get_ndfdXMLclient_basic <- function(){
    c("product", "begin", "end", "Unit")
}

#' This function is used during development to create and save the data object
#'  \code{ndfdXMLclient_groups}.
#' 
#' @return a named list of query elements for various query types
get_ndfdXMLclient_groups <- function(){
    list(
        query_point = c(
            "lat", 
            "lon", 
            ndfdXMLclient_basic),
        query_multipoint = c(       # Number of points requested can not exceed 200.
            "listLatLon",
            ndfdXMLclient_basic),
        query_subgrid = c(
            "lat1",
            "lon1",
            "lat2",
            "lon2",
            "resolutionSub",
            ndfdXMLclient_basic),
        list_points_in_subgrid = c(
            "listLat1",
            "listLon1",
            "listLat2",
            "listLon2",
            "resolutionList"),
        query_line = c(
            "endPoint1Lat",
            "endPoint1Lon",
            "endPoint2Lat",
            "endPoint2Lon",
            ndfdXMLclient_basic),
        list_points_on_line = c(
            "endPoint1Lat",
            "endPoint1Lon",
            "endPoint2Lat",
            "endPoint2Lon"),
        query_zipcodes = c(
            "zipCodeList",
            ndfdXMLclient_basic),
        list_zipcodes = c(
           "listZipCodeList"),
        query_cities = c(
            "citiesLevel",
            ndfdXMLclient_basic),
        list_cities = c(
            "citiesLevel"),
        query_centerpoint = c(
            "centerPointLat",
            "centerPointLon",  
            "distanceLat",
            "distanceLon",
            "resolutionSquare",
            ndfdXMLclient_basic),
        list_centerpoint = c(
            "listCenterPointLat",
            "listCenterPointLon",  
            "listDistanceLat",
            "listDistanceLon",
            "listResolutionSquare"),    
        list_corners = c(
            "sector"),
        query_single_time = c(
            "gmlListLatLon",
            "compType",
            "featureType",
            ndfdXMLclient_basic[-1]))
}



#' Construct a NDFD uri possibly with a query
#'
#' @export
#' @param query character string
#' @param baseuri character the base URI
#' @param interface the interface to use with the baseuri
#' @return uri
ndfd_uri <- function(query = NULL, 
    baseuri = "http://graphical.weather.gov/xml/sample_products/browser_interface",
    interface = c('ndfdXMLclient.php', 'ndfdBrowserClientByDay')[1]){
    
    uri <- file.path(baseuri[1], interface[1])
    if (!is.null(query)) uri <- paste0(uri, "?", query[1])
    return(uri) 
}

#' Constructs a query to list resources available
#'
#' Defaults are stored in \code{ndfdXMLclient_vars} and are accessed by 
#'  the type of resources requested.  For instance the defaults for a 
#'  \code{points_in_subgrid} are found in \code{ndfdXMLclient_groups} 
#'  under \code{list_points_in_subgrid}
#'
#' @seealso \url{http://graphical.weather.gov/xml/rest.php#XML_contents}
#' @export
#' @param what character - the type of query which defines the defaults.  Defaults
#'  for each type of query are identified \code{ndfdXMLclient_vars} and 
#'  \code{ndfdXMLclient_groups}
#'  \itemize{
#'      \item points_in_subgrid, A List of NDFD Points for a Subgrid
#'      \item points_on_line, A List of NDFD Points for a Line
#'      \item zipcodes, A List of NDFD Points for a Zipcode
#'      \item cities, A List of NDFD Points for a List of Cities
#'      \item centerpoint, A List of NDFD Points for a Subgrid Defined by a Center Point
#'      \item corners, A List of NDFD Points for the Corners of an NDFD Grid
#'  }
#' @examples
#' \dontrun{
#' # default for points, and then with lon and elements specified
#' list_this(what = "points_in_subgrid")
#' query_this(what = "point_in_subgrid", lon = -69)
#' # query by zipcode
#' query_this(what = "zipcode", zipCodeList = "04096",element = c("dew", "temp", "snow"))
#' }
list_this <- function(what = "points_in_subgrid", ...){
    w <- paste0('list_', tolower(what[1]))
    items <- list(...)
    wc <- which_client(w)
    #wc <- ""
    if (nchar(wc) > 0) items[['whichClient']] <- unname(wc)
    defaults <- ndfdXMLclient_vars[ndfdXMLclient_groups[[w]]]
    if (length(items) == 0){
        items <- if (nchar(wc)> 0) c(list(whichClient = unname(wc)), defaults) else defaults
    } else {
        if (nchar(wc) > 0) items <- c(list(whichClient=unname(wc)), items)
        ix <- names(defaults) %in% names(items)
        iy <- !ix
        if (any(iy)) items[names(defaults)[iy]] <- defaults[iy]
    }
    nm <- names(items)
    items <- lapply(items, 
        function(x) {
            if (is.numeric(x)) x <- n2c(x)
            x
        })
    build_query(items, element = NULL)
}

#' Construct a query using an optional preconfigured list of defaults.
#'
#' Defaults are stored in \code{ndfdXMLclient_vars} and are accessed by 
#'  the type of resources requested.  For instance the defaults for a 
#'  \code{point} are found in \code{ndfdXMLclient_groups} under \code{query_point}
#'
#' @seealso \url{http://graphical.weather.gov/xml/rest.php#XML_contents}
#' @export
#' @param what character - the type of query which defines the defaults.  Defaults
#'  for each type of query are identified \code{ndfdXMLclient_vars} and 
#'  \code{ndfdXMLclient_groups}
#'  \itemize{
#'      \item point, Single Point Unsummarized Data
#'      \item multipoint, Multiple Point Unsummarized Data
#'      \item subgrid, Unsummarized Data for a Subgrid
#'      \item line, Unsummarized Data for a Line
#'      \item zipcodes, Unsummarized Data for One or More Zipcodes
#'      \item cities, Unsummarized Data for a List of Cities
#'      \item centerpoint, Unsummarized Data for a Subgrid Defined by a Center Point
#'      \item single_time, Unsummarized Data for a Single Time Encoded in dwGML
#'  }
#' @param element a character vector or elements to retrieve.  See 
#'   \url{http://graphical.weather.gov/xml/docs/elementInputNames.php}
#' @param ... zero or more parameters that will add to or override the 
#'  defaults listed in \code{ndfdXMLclient_vars}
#' @return character of the query to pass to \code{get_query}
#' @examples
#' \dontrun{
#' # default for point, and then with lon and elements specified
#' query_this(what = "point")
#' query_this(what = "point", lon = -69, element = c("dew", "temp", "snow"))
#' # query by zipcode
#' query_this(what = "zipcode", zipCodeList = "04096",element = c("dew", "temp", "snow"))
#' }
query_this <- function(what = 'point', element =  c('mint', 'maxt', 'temp'), ...){
    w <- paste0('query_', tolower(what[1]))
    wc <- which_client(w)
    #wc <- ""
    items <- list(...)
    defaults <- ndfdXMLclient_vars[ndfdXMLclient_groups[[w]]]
    if (length(items) == 0){
        items <- if (nchar(wc) > 0) c(list(whichClient = unname(wc)), defaults) else defaults
    } else {
        if (nchar(wc) > 0) items <- c(list(whichClient=unname(wc)), items)
        ix <- names(defaults) %in% names(items)
        iy <- !ix
        if (any(iy)) items[names(defaults)[iy]] <- defaults[iy]
    }
    nm <- names(items)
    if ('begin' %in% nm) items[['begin']] <- t2c(items[['begin']])
    if ('end' %in% nm) items[['end']] <- t2c(items[['end']])
    items <- lapply(items, 
        function(x) {
            if (is.numeric(x)) x <- n2c(x)
            x
        })
    build_query(items, element = element)
}



###
#  ndfdBrowserClientByDay.php Interface
###
