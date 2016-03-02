#' A DWML class for for the lonLatList section
#'
#' @field location a data frame of location information
#' @include Base.R
#' @export
DWMLLatLonListRefClass <- setRefClass("DWMLLatLonListRefClass",
    
    contains = 'DWMLBaseRefClass',
    
    methods = list(
        show = function(prefix = ''){
            callSuper(prefix = prefix)
            loc <- .self$extract_location(form = 'character')
            if (nrow(loc) == 0){
               cat(prefix, " no locations available\n", sep = "") 
            } else if (nrow(loc) <= 12){
                cat(prefix, " locations:\n", sep = "")
                print(loc)
            } else {
                cat(prefix, " locations (head and tail):\n", sep = "")
                print(head(loc))
                print(tail(loc))   
            }
        })
    )
    

#' Extracts location information
#'
#' The \code{as_is} value for the \code{form} argument is useful for creating
#'  a data query for multiple points
#'
#' @name DWMLLatLonListRefClass_extract_location
#' @param form character specifiction for as_is, character or numeric output
#' @return character or data frame, possibly with zero rows
NULL
DWMLLatLonListRefClass$methods(
    extract_location = function(form = c("character", "numeric", "as_is")[2]){
        loc <- xml_value(.self$node)
        if (form[1] == 'as_is') return(loc)
        if (is.null(loc)) return(data.frame())
        if (length(loc) == 0) return(data.frame())
        ss <- strsplit(loc, " ", fixed = TRUE)[[1]]
        xy <- do.call(rbind, strsplit(ss, ",", fixed = TRUE))
        if (tolower(form[1]) == 'numeric'){
            x <- data.frame(
                lat = as.numeric(xy[,1]), 
                lon = as.numeric(xy[,2]), 
                stringsAsFactors = FALSE)
        } else {
            x <- data.frame(
                lat = xy[,1], 
                lon = xy[,2], 
                stringsAsFactors = FALSE)
        }
        x
    })
