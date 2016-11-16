
#' Groups indices (1 to n) into groups of MAX size
#'
#' Useful when needing to get data in groups of 200 as per \url{http://graphical.weather.gov/xml/rest.php}
#'
#' @export
#' @param index numeric vector of indicies to split into vectors of MAX length
#' @param MAX numeric, the maximum number of elements per group returned
#' @return list of index vectors of length MAX or shorter
#' @examples
#'  \dontrun{
#'      str(split_indices(201:615))
#'  }
split_indices <- function(index, MAX = 200){
    split(index, seq.int(0L, length(index)-1L) %/% MAX + 1)
}


#' Convert location pairs into characters suitable for 'listLatLon' 
#'  and 'gmlListLatLon'
#'
#' @export
#' @param xy matrix of [n,2] dims ([lat, lon] order)
#' @return character in the form 'lat1,lon1 lat2,lon2 lat3,lon3 ...' 
paste_listLatLon <- function(xy){
    s <- paste(apply(xy, 1, paste, collapse = ","), collapse = " ")
}

#' Convert numeric values to character
#' 
#' @export
#' @param x numeric or character value to format
#' @param fmt character format
#' @return character representation of input
n2c <- function(x, fmt = '%0.5f'){
   if (is.numeric(x)){
      x <- sprintf(fmt, x)
   }
   x
}

#' Format date-time values into YYYY-mm-ddTHH:MM
#'
#' @param x POSIXct of character time
#' @param fmt date format
#' @return character time representation of input
t2c <- function(x, fmt = "%Y-%m-%dT%H:%M"){
   if (inherits(x, 'POSIXct')){
      x <- format(x, format = fmt)
   }
   x
}

#' Test if an object inherits from xml2::xml_node
#'
#' @export
#' @param x object to test
#' @param classname character, the class name to test against, by default 'XMLAbstractNode'
#' @return logical
is_xml_node <- function(x, classname = 'xml_node'){
   inherits(x, classname)
}

#' Convert xml_node to character
#' 
#' @export
#' @param x xmlNode or NodeRefClass
#' @return character
xml_string <- function(x){
   if (inherits(x, 'xml_node')){
      #r <- gsub("\n","", xml2:xml_text(x))
      r <- xml2::xml_text(x)
   } else {
      r <- xml_string(x$node)
   }
   return(r)
}


#' Test xml_node or NodeRefClass is an exception
#'
#' @export
#' @param x node object to test
#' @param space the namespace to test
#' @return logical
is_exception <- function(x, space = 'exc'){
   if (inherits(x, 'DWMLNodeRefClass')) x <- x$node
   is_xml_node(x) && 
    ("exc" %in% names(xml2::xml_ns(x)) || "error" %in% xml2::xml_name(x))
}

#' Extract the value from a simple xml_node object
#'
#' @export
#' @param x xml_node with a value
#' @return the value of the node
xml_value  <- function(x){
   xml2::xml_text(x)
}

#' Extract the attributes from a simple xml_node object
#'
#' @export
#' @param x xml_node with attributes
#' @return character vector of the attributes
xml_atts  <- function(x){
    xml2::xml_attrs(x)
}

# #' Extract the name of a simple xml_node object
# #'
# #' @export
# #' @param x xml_node
# #' @return character vector of the attributes
# xml_name <- function(x){
#     xml2::xml_name(x)
# }
