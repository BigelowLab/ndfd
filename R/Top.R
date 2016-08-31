#' The top level DWML class for containing a non-exception query response that contains
#'  a head and a data section.
#'
#' @seealso \url{http://graphical.weather.gov/xml/mdl/XML/Design/MDL_XML_Design.pdf}
#' 
#' @field version character version string
#' @field head DWMLHeadRefClass container
#' @field data DWMLDataRefClass contaier
#' @include Base.R
#' @export
DWMLTopRefClass <- setRefClass("DWMLTopRefClass",

    contains = 'DWMLBaseRefClass',
    
    fields = list(
       version = 'character',
       exception = 'ANY',
       head = 'ANY',
       data = 'ANY', 
       latLonList = 'ANY'),
   
    methods = list(
       init = function(){
         .self$field('exception', NULL)
         .self$field('head', NULL)
         .self$field('data', NULL)
         .self$field('latLonList', NULL)
        
         if (is_exception(.self$node)){
            .self$field("exception", DWMLExceptionRefClass$new(.self$node))
         } else {
            atts <- xml2::xml_attrs(.self$node)
            if ("version" %in% names(atts)) .self$version <- atts[['version']]
            
            x <- xml2::xml_find_first(.self$node, 'head')
            if (!inherits(x, 'xml_missing'))
                .self$field("head", DWMLHeadRefClass$new(x))
            x <- xml2::xml_find_first(.self$node, 'data')    
            if (!inherits(x, "xml_missing")) 
                .self$field("data", DWMLDataRefClass$new(x))
            x <- xml2::xml_find_first(.self$node, 'latLonList')
            if (!inherits(x, 'xml_missing'))
                .self$field('latLonList', DWMLLatLonListRefClass$new(x))
        }
        },
        show = function(prefix = ""){
            callSuper(prefix = prefix)
            if (!is.null(.self$exception)){
                 cat(sprintf("[%s has exception element ]", prefix),"\n")
                .self$exception$show(prefix = paste0(prefix, "  "))
            } else {
                #cat(sprintf("[%s no exception element ]", prefix),"\n") 
            }
            if (!is.null(.self$head)) {
                cat(sprintf("[%s has head element ]", prefix),"\n")
                .self$head$show(prefix = paste0(prefix, "  "))
            } else { 
                cat(sprintf("[%s no head element ]", prefix),"\n") 
            }
            if (!is.null(.self$data)){
                cat(sprintf("[%s has data element ]", prefix),"\n")
                .self$data$show(prefix = paste0(prefix, "  "))
            } else {
                cat(sprintf("[%s no data element ]", prefix), "\n")
            }
            if (!is.null(.self$latLonList)) {
                cat(sprintf("[%s has latLonList element ]", prefix),"\n")
                .self$latLonList$show(prefix = paste0(prefix, "  "))
            } else {
                cat(sprintf("[%s no latLonList element ]", prefix), "\n")
            }

        })
)

       