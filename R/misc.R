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

#' Test if an object inherits from XML::XMLAbstractNode
#'
#' @export
#' @param x object to test
#' @param classname character, the class name to test against, by default 'XMLAbstractNode'
#' @return logical
is_xmlNode <- function(x, classname = 'XMLAbstractNode'){
   inherits(x, classname)
}

#' Convert XML::xmlNode to character
#' 
#' @export
#' @param x xmlNode or NodeRefClass
#' @return character
xmlString <- function(x){
   if (inherits(x, 'NodeRefClass')){
      r <- xmlString(x$node)
   } else {
      r <- gsub("\n","", XML::toString.XMLNode(x))
   }
   return(r)
}


#' Test XML::xmlNode or NodeRefClass is an exception
#'
#' @export
#' @param node object to test
#' @param space the namespace to test
#' @return logical
is_exception <- function(x, space = 'exc'){
   if (inherits(x, 'DWMLNodeRefClass')) x <- x$node
   is_xmlNode(x) && ("exc" %in% names(XML::xmlNamespace(x)) )
}

#' Extract the value from a simple XML::xmlNode object
#'
#' @export
#' @param x XML::xmlNode with a value
#' @param the value of the node
xml_value  <- function(x){
    XML::xmlValue(x)
}

#' Extract the attributes from a simple XML::xmlNode object
#'
#' @export
#' @param x XML::xmlNode with attributes
#' @param character vector of the attributes
xml_atts  <- function(x){
    XML::xmlAttrs(x)
}

