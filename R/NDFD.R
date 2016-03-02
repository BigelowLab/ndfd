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
                .self$uri <- x[1]          
                r <- httr::GET(x[1])
                .self$node <- check_response(r)
                .self$init()
            }
        },
        
        browse = function(){
            if (interactive() && (nchar(.self$uri) > 0)) httr::BROWSE(.self$uri)
        },
        
        show = function(prefix = ""){
            #cat(prefix, "URI:", .self$uri, "\n", sep = "")
            callSuper(prefix = prefix)
        })
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



