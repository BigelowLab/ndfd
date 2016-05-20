#' A DWML class for for the lonLatList section
#'
#' @include Base.R
#' @export
DWMLLatLonListRefClass <- setRefClass("DWMLLatLonListRefClass",

    contains = 'DWMLBaseRefClass',
    
    methods = list(
        show = function(prefix = ''){
            callSuper(prefix = prefix)
            loc <- .self$get_location(form = 'character')
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
#' @name DWMLLatLonListRefClass_get_location
#' @param form character specifiction for as_is, character or numeric output 
#' @param index numeric vector of indices to retrieve
#' @return character or data frame, possibly with zero rows
NULL
DWMLLatLonListRefClass$methods(
    get_location = function(
        form = c("character", "numeric", "as_is")[2],
        index = NA, ...){
        
        # if an index is provided then we call this method first
        # to get a data.frame of character
        if (!is.na(index[1])) {
            ll <- as.matrix(.self$get_location(form = 'character')[index,])
            loc <- paste(
                apply(ll, 1, paste0, collapse = ","),
                collapse = " ")
        } else {
            loc <- xml_value(.self$node)
        }
        if (tolower(form[1]) == 'as_is') return(loc)
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
