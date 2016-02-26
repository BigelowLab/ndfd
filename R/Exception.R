
# http://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXMLclient.php?lat1=35.00&lon1=-82.00&lat2=35.5&lon2=-81.50&resolutionSub=20.0&product=time-series&begin=2004-01-01T00:00:00&end=2013-04-21T00:00:00&maxt=maxt&mint=mint

#' A DWML exception class
#' 
#' @include DWMLBase.R
#' @export
DWMLExceptionRefClass <- setRefClass("DWMLExceptionRefClass",
    
    contains = 'DWMLBaseRefClass',
    
    methods = list(
    
        get_problem = function(default = 'unspecified'){
            if ('pre' %in% names(.self$node)){
                if ('problem' %in% names(.self$node[['pre']])){
                   problem <- xml_value(.self$node[['pre']][['problem']])
                }
            } else {
                problem <- default
            }
            problem
        },
        
        show = function(prefix = ""){
            callSuper(profix = prefix)
            problem <- .self$get_problem()
            cat(prefix, "  Problem: ", .problem, "\n", sep = "")
        })
)

#' Create an exception XML::xmlNode
#'
#' @export
#' @param problem character, the problem statement
#' @return XML::xmlNode
create_exception <- function(problem = 'unspecified'){

    XML::newXMLNode("error",
        kids = list(
            XML::newXMLNode("pre",
                kids = list(XML::newXMLNode("problem", problem))
            )
        )
    )
}
