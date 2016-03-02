#' @field uri character
#' @include Top.R
NDFDRefClass <- setRefClass("NDFDRefClass",

    contains = "DWMLTopRefClass", 

    fields = list(
        uri = 'character'
        ),
    
    methods = list(
        
        initialize = function(x){
            if (is.character(x)){          
                r <- httr::GET(x[1])
                .self$node <- parse_response(r)
            }
        },
        
        show = function(prefix = ""){
            callSuper(prefix = prefix)
            cat(prefix, " URI:", .self$uri, "\n")
        }
        )
)           
        
#' Instantiate a NDFREfClass object
#' 
#' @export
#' @param query character string
#' @param baseuri character the base URI
#' @param interface the interface to use with the baseuri
#' @return NDFDRefClass object
NDFD <- function(query = NULL, 
    baseuri = "http://graphical.weather.gov/xml/sample_products/browser_interface",
    interface = c('ndfdXMLclient.php', 'ndfdBrowserClientByDay')[1]){
            
    uri <- file.path(baseuri[1], interface[1])
    if (!is.null(query)) uri <- paste0(uri, "?", query[1]) 
            
    NDFDRefClass$new(uri)
}      



