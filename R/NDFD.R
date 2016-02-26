library(httr)
library(XML)


txt <-"~/Downloads/NDFD/multipoint.xml"
node <- XML::xmlRoot(XML::xmlTreeParse(txt))

nm <- file.path('/Users/Shared/code/R/others/ndfd/R',
    c("misc.R", "query.R", "Base.R", "Top.R",
    "Exception.R", "Head.R", "Data.R"))
for (n in nm) source(n)

X <- DWMLTopRefClass$new(node)
X
tl <- X$data$extract_time_layout()
tl


BASEURI <- "http://graphical.weather.gov/xml/sample_products/browser_interface"
interface <- c(client = 'ndfdXMLclient.php', byday = 'ndfdBrowserClientByDay')




check_response <- function(x, encoding = "UTF-8"){
    w <- httr::http_error(rsp)
    if (!w) {
        print(rsp)
        print(httr::content(rsp, as = "text", encoding = encoding)) 
    }
    
    x <- try(httr::content(rsp, as = "text", encoding = encoding))
    if (inherits(x, 'try-error')){
        x  <- create_exception(problem = "error extracting response content")
        return(invisible(x))
    }
    x <- try(XML::xmlTreeParse(x, asText = TRUE, 
        encoding = encoding, useInternalNodes = TRUE,
        fullNamespaceInfo = TRUE))
    if (inherits(x, "try-error")){
        x <- create_exception(problem = "error with xmlTreeParse")
        return(invisible(x))
    }
    
    x <- try(XML::xmlRoot(x))
    if (inherits(x, "try-error")){
        x <- .create_exception(problem = "error parsing response content with xmlRoot")
    }
    
    invisible(x)
} # check_response

parse_response <- function(r){
    
    x <- check_reponse(r)
    
    if (XML::xmlName(x) == "error"){
    
        R <- DWMLExceptionRefClass$new(x)
        
    } else {
        
        R <- DWMLRefClass$new(x)
    
    }
    
    invisible(R)
}
    

get_query <- function(query,
    baseuri = BASEURI,
    interface = c('ndfdXMLclient.php', 'ndfdBrowserClientByDay')[1]){
    
 
    uri <- file.path(baseuri, interface)
    if (!is.null(query)){
        uri <- paste0(uri, "?", query)
    }
    
    r <- httr::GET(uri)
    return(parse_response(r))

}
      





