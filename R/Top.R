#' The top level DWML class for containing a non-exception query response that contains
#'  a head and a data section.
#'
#' @seealso \url{http://graphical.weather.gov/xml/mdl/XML/Design/MDL_XML_Design.pdf}
#' 
#' @field version character version string
#' @field head DWMLHeadRefClass container
#' @field data DWMLDataRefClass contaier
#' @include DWMLBase.R
#' @export
DWMLTopRefClass <- setRefClass("DWMLTopRefClass",

    contains = 'DWMLBaseRefClass',
    
    fields = list(
       version = 'character',
       head = 'ANY',
       data = 'ANY'),
   
    methods = list(
       init = function(){
         atts <- xml_atts(.self$node)
         if ("version" %in% names(atts)) .self$version <- atts[['version']]
         child_names <- names(.self$node)
         if ('head' %in% child_names) 
             .self$field("head", DWMLHeadRefClass$new(.self$node[['head']]))
         if ('data' %in% child_names) 
             .self$field("data", DWMLDataRefClass$new(.self$node[['data']]))
        },
        show = function(prefix = ""){
            callSuper(prefix = prefix)
            .self$head$show(prefix = "  ")
            .self$data$show(prefix = "  ")
        })
)

       