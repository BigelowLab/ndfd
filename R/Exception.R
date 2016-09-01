
# http://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXMLclient.php?lat1=35.00&lon1=-82.00&lat2=35.5&lon2=-81.50&resolutionSub=20.0&product=time-series&begin=2004-01-01T00:00:00&end=2013-04-21T00:00:00&maxt=maxt&mint=mint

#' A DWML exception class
#' 
#' @include Base.R
#' @export
DWMLExceptionRefClass <- setRefClass("DWMLExceptionRefClass",
    
    contains = 'DWMLBaseRefClass',
    
    methods = list(
        
        get_problem = function(default = 'unspecified'){
            problem <- default
            prob <- xml2::xml_find_first(.self$node, "pre/problem")
            if (inherits(prob, 'xml_node')){
                problem <- xml2::xml_text(prob)
            } else {
                problem <- default
            }
            problem
        },
        
        show = function(prefix = ""){
            callSuper(prefix = prefix)
            problem <- .self$get_problem()
            cat(prefix, "  Problem: ", problem, "\n", sep = "")
        })
)

#' Create an exception xml_node
#'
#' @export
#' @param problem character, the problem statement
#' @return xml_node
create_exception <- function(problem = 'unspecified'){

    root <- xml2::xml_new_document() %>% xml2::xml_add_child("error")
    xml2::xml_add_child(root, "pre") %>%
        xml2::xml_add_child("problem", problem) %>%
        invisible()
    return(root)
}
