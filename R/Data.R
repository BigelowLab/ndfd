#' A DWML class for for the data section
#'
#' @field location a data frame of location information
#; @field data a data.frame of element data
#' @include DWMLBase.R
#' @export
DWMLDataRefClass <- setRefClass("DWMLDataRefClass",
    
    contains = 'DWMLBaseRefClass',
    
    fields = list(
        location = 'data.frame',
        data = 'list'),
        
    methods = list(
        init = function(){
            .self$location <- .self$extract_location()
        },
        
        show = function(prefix = ''){
            callSuper(prefix = prefix)
            if (nrow(.self$location) <= 12){
                cat(prefix, " locations: ",, "\n", sep = "")
                print(.self$location)
            } else {
                cat(prefix, " locations (head and tail):\n", sep = "")
                print(head(.self$location))
                print(tail(.self$location))   
            }
        })
)

#' Extracts location information
#'
#' @name DWMLDataRefClass_extract_location
#' @return data frame, possibly with zero rows
NULL
DWMLDataRefClass$methods(
    extract_location = function(){
        loc <- .self$node[names(.self$node) %in% 'location']
        if (is.null(loc)) return(data.frame())
        
        is_point <- !is.null(loc[[1]][['point']])
        is_city <- !is.null(loc[[1]][['city']])
        if (is_point){
            x <- lapply(loc,
                function(x){
                    c(location_key = xml_value(x[['location-key']]),
                        xml_atts(x[['point']]) )
                })
        } else if (is_city) {
            x <- lapply(loc,
                function(x){
                    c(location_key = xml_value(x[['location-key']]),
                        city = xml_value(x[['city']]),
                        xml_atts(x[['city']]) )
                })
        } else {
            warning("location type not known")
            return(data.frame())
        }
        
        x <- as.data.frame(do.call(rbind, x), stringsAsFactors = FALSE)
        rownames(x) <- x[,'location_key']
        x
    })
    
#' Extracts time-layout information
#'
#' The <time-layout> element {dw:time-layoutType } [+] contains the start and 
#'  stop valid times and any associated period names for the data. Since 
#'  different environmental parameters have different time schemes (valid at 
#'  different interval and available for different lengths of time into the 
#'  future), there will be one <time-layout> element for each of these unique 
#'  temporal configurations. Each data parameter will reference exactly one of 
#'  these time layouts (R2.2.3).
#'
#' time-layout
#' <time-layout time-coordinate="local" summarization="none">
#'   <layout-key>k-p24h-n6-2</layout-key>
#'   <start-valid-time>2016-02-26T19:00:00-05:00</start-valid-time>
#'   <end-valid-time>2016-02-27T08:00:00-05:00</end-valid-time>
#'   <start-valid-time>2016-02-27T19:00:00-05:00</start-valid-time>
#'   <end-valid-time>2016-02-28T08:00:00-05:00</end-valid-time>
#'   ...
#'   <start-valid-time>2016-03-02T19:00:00-05:00</start-valid-time>
#'   <end-valid-time>2016-03-03T08:00:00-05:00</end-valid-time>
#' </time-layout>
#'
#' @name DWMLDataRefClass_extract_time_layout
#' @return a list of data.frames - one per layout-key
NULL
DWMLDataRefClass$methods(
    extract_time_layout = function(form = c("POSIXct", "character")[1]){
        tl <- .self$node[names(.self$node) %in% 'time-layout']
        if (is.null(tl)) return(list())
        xx <- lapply(tl,
            function(x, as_posixct = TRUE){
                starts <- sapply(x[names(x) %in% 'start-valid-time'], function(x) xml_value(x))
                ends <- sapply(x[names(x) %in% 'end-valid-time'], function(x) xml_value(x))
                if (as_posixct){
                    starts <- as.POSIXct(starts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
                    ends <- as.POSIXct(ends, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
                }
                x <- data.frame(start_valid_time = starts, 
                    end_valid_time = ends, stringsAsFactors = FALSE)
                
            }, 
            as_posixct = tolower(form[1]) == 'posixct')
        names(xx) <- sapply(tl, function(x) xml_value(x[['layout-key']]))
                    
            
        xx
    })
                        
#' Extracts paramaters information
#'
#' The <time-layout> element {dw:time-layoutType } [+] contains the start and 
#'  stop valid times and any associated period names for the data. Since 
#'  different environmental parameters have different time schemes (valid at 
#'  different interval and available for different lengths of time into the 
#'  future), there will be one <time-layout> element for each of these unique 
#'  temporal configurations. Each data parameter will reference exactly one of 
#'  these time layouts (R2.2.3).
#' <parameters applicable-location="point1">
#'  <temperature type="maximum" units="Fahrenheit" time-layout="k-p24h-n7-1">
#'   <name>Daily Maximum Temperature</name>
#'   <value>51</value>
#'    ...
#'   <value>53</value>
#'  </temperature>
#'   <temperature type="minimum" units="Fahrenheit" time-layout="k-p24h-n6-2">
#'   <name>Daily Minimum Temperature</name>
#'   <value>30</value>
#'   <value>34</value>
#'   ...
#'   <value>38</value>
#'  </temperature>
#' </parameters>
#'
#' @name DWMLDataRefClass_time_layout
#' @return a list of data.frames - one per appplicable-location
NULL
DWMLDataRefClass$methods(
    extract_parameters = function(){
        P <- .self$node[names(.self$node) %in% 'parameters']
        if (is.null(P)) return(list())
        applicable_location <- sapply(P, 
            function(x) { xml_atts(x)[['applicable-location']] } )
        
        pp <- lapply(P,
            function(x){
                xx <- lapply(XML::xmlChildren(x), extract_one_parameter)
                names(xx) <- sapply(xx, "[[", 'name')
                xx
            })
        names(pp) <- applicable_location
    
    })            
        

#' Extract one parameter from an XML::xmlNode
#' 
#' @param x XML::xmlNode
#' @return a list with at least 'name' and 'value'
extract_one_parameter <- function(x){
    a <- as.list(xml_atts(x))
    a[['name']] <- xml_value(x[['name']])
    names(a) <- gsub("-", "_", names(a))
    a[['value']] <- switch(a[['name']],
        'weather' = sapply(x[names(x) %in% 'weather-conditions'], xml_value),
        'conditions-icon' = sapply(x[names(x) %in% 'icon-link'], xml_value),
        'wordedForecast' = sapply(x[names(x) %in% 'text'], xml_value),
        as.numeric(sapply(x[names(x) %in% 'value'], xml_value)) )
    a
}
