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
       head = 'ANY',
       data = 'ANY', 
       latLonList = 'ANY'),
   
    methods = list(
       init = function(){
         atts <- xml_atts(.self$node)
         if ("version" %in% names(atts)) .self$version <- atts[['version']]
         .self$field('head', NULL)
         .self$field('data', NULL)
         .self$field('latLonList', NULL)
         child_names <- names(.self$node)
         if ('head' %in% child_names)
             .self$field("head", DWMLHeadRefClass$new(.self$node[['head']]))
         if ('data' %in% child_names) 
             .self$field("data", DWMLDataRefClass$new(.self$node[['data']]))
         if ('latLonList' %in% child_names)
             .self$field('latLonList', DWMLLatLonListRefClass$new(.self$node[['latLonList']]))
        },
        show = function(prefix = ""){
            callSuper(prefix = prefix)
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

       