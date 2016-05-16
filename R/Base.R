#' The root class for any DWML XML structure
#'
#' @seealso \url{http://graphical.weather.gov/xml/mdl/XML/Design/MDL_XML_Design.pdf}
#' @export
#' @field node a single XML::xmlNode
DWMLBaseRefClass <- setRefClass("DWMLBaseRefClass",
    fields = list(node = 'ANY'),
    methods = list(
        initialize = function(x){
            if (!missing(x)){
                if (is_xmlNode(x)){
                    .self$field("node", x)
                } else {
                    stop("input must be XML::XMLAbstractNode")
                }
            } else {
                .self$field("node", NULL)
            }
            init()
        },
        
        show = function(prefix = ""){
            cat(prefix, "Reference Class: ", methods::classLabel(class(.self)), "\n", sep = "")
        },
        
        init = function() {}
    )
                
)