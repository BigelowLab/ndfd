#' A DWML class for for the data section
#'
#' @field location a data frame of location information
#; @field data a data.frame of element data
#' @include Base.R
#' @export
DWMLDataRefClass <- setRefClass("DWMLDataRefClass",
    
    contains = 'DWMLBaseRefClass',
    
    fields = list(
        location = 'data.frame', # data.frame
        time_layout = "list",  # list of data.frames
        data_list = 'list'),   # list of character vectors
        
    methods = list(
        init = function(){
            .self$location <- .self$get_location()
            .self$time_layout <- .self$get_time_layout()
            .self$data_list <- .self$list_parameters()
        },
        
        show = function(prefix = ''){
            callSuper(prefix = prefix)
            nloc <- nrow(.self$location)
            if (nloc <= 12){
                cat(prefix, "locations: ",nloc, "\n", sep = "")
                print(.self$location)
            } else {
                cat(prefix, "locations (head and tail): ", nloc, "\n", sep = "")
                print(head(.self$location))
                print(tail(.self$location))   
            }
            tl <- names(.self$time_layout)
            if(nchar(tl[1]) > 0){
                cat(prefix, "timelayout(s): ",paste(tl, collapse = " "), "\n")
                
            }  # has timelayout
            
            if (length(.self$data_list) > 0){
                cat(prefix, "parameter(s): ", 
                    length(.self$data_list), "\n", sep = "")
                ss <- parameter_to_string(.self$data_list)
                for (s in ss) cat(prefix, prefix, s, "\n", sep = "")
            }  # has parameters
                    
        })
)

#' Extracts location information
#'
#' @name DWMLDataRefClass_get_location
#' @return data frame, possibly with zero rows
NULL
DWMLDataRefClass$methods(
    get_location = function(form = c("character", "numeric")[2]){
        loc <- xml2::xml_find_all(.self$node, 'location')
        if (length(loc) == 0) return(data.frame())
        
        is_point <- !inherits(xml2::xml_find_first(loc[[1]], "point"), 'xml_missing')
        is_city <- !inherits(xml2::xml_find_first(loc[[1]], "c"), 'xml_missing')
        if (is_point){
            x <- lapply(loc,
                function(x){
                    c(location_key = xml2::xml_text(xml2::xml_find_first(x, 'location-key')),
                        xml2::xml_attrs(xml2::xml_find_first(x, "point")) )

                })
        } else if (is_city) {
            x <- lapply(loc,
                function(x){
                    c(location_key = xml2::xml_text(xml2::xml_find_first(x, 'location-key')),
                        city = xml2::xml_text(xml2::xml_find_first(x, 'city')),
                        xml2::xml_attrs(xml2::xml_find_first('city')))
                })
        } else {
            warning("location type not known")
            return(data.frame())
        }
        
        x <- as.data.frame(do.call(rbind, x), stringsAsFactors = FALSE)
        if (tolower(form[1]) == 'numeric'){
            ix <- c("latitude", "longitude") %in% colnames(x)
            if (any(ix)){
                x[,'latitude'] <- as.numeric(x[,'latitude'])
                x[,'longitude'] <- as.numeric(x[,'longitude'])
            }
        }
        rownames(x) <- x[,'location_key']
        x
    })


#' Retrieves the common start times across all time-layouts
#'
#' @name DWMLDataRefClass_common_starts
#' @return a named vector of POSIXct or possibly empty
NULL
DWMLDataRefClass$methods(
    common_starts = function(key = names(.self$time_layout)){
        s = do.call(c,sapply(.self$time_layout[key], function(x) x[,'start_valid_time']))
        s[duplicated(s)]
    })


#' Retrieves a character vector of time-layout keys
#' 
#' @name DWMLDataRefClass_list_time_layout
#' @return character vector - possibly empty
NULL
DWMLDataRefClass$methods(
    list_time_layout = function(){
        tl <- xml2::xml_find_all(.self$node, "time-layout/layout-key")
        if (length(tl) == 0){
            r = ""
        } else {
            r <- sapply(tl, xml2::xml_text)
        }
        r
    })

#' Retrieves a list of character vectors describing the parameters
#' 
#' @name DWMLDataRefClass_list_time_layout
#' @return list of character vectors - possibly empty
NULL
DWMLDataRefClass$methods(
    list_parameters = function(){
            
        r <- list()
        p1 <- xml2::xml_find_first(.self$node, "parameters")
        if (length(p1) > 0) {
            r <- lapply(xml2::xml_children(p1),
                function(p){
                    c(  name = xml2::xml_name(p),
                        xml2::xml_attrs(p))
                    })
            names(r) <- sapply(r, "[[", "name")
        }

        r
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
#' @name DWMLDataRefClass_get_time_layout
#' @param key NULL or character, specifies which time-layout by key.  If NULL
#'  then all are returned.
#' @param form character specifies time-object type to return
#' @return a list of data.frames - one per layout-key
NULL
DWMLDataRefClass$methods(
    get_time_layout = function(key = NULL, form = c("POSIXct", "character")[1]){
        DWML_get_time_layout(.self, key=key, form=form)
    })
       
       
#' List the parameters
#'                  
#' Extracts parameters information
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
#' @name DWMLDataRefClass_get_parameters
#' @return a list of data.frames - one per appplicable-location
NULL
DWMLDataRefClass$methods(
    get_parameters = function(){
        P <- xml2::xml_find_all(.self$node, 'parameters')
        #P <- .self$node[names(.self$node) %in% 'parameters']
        if (length(P) == 0) return(list())
        applicable_location <- sapply(P, 
            function(x) {
                xml2::xml_attrs(x)
                #xml_atts(x)[['applicable-location']] 
            } )
        
        pp <- lapply(P,
            function(x){
                xx <- lapply(xml2::xml_children(x), get_one_parameter)
                names(xx) <- sapply(xx,'[[', 'name')
                xx
            })
        names(pp) <- applicable_location
        pp
    })            


#' Retrieve the data by name
#' 
#' @name DWMLDataRefClass_get_data
#' @param name character of the parameter to retrieve
#' @param key character the time-layout key to retrieve
#' @return data.frame with zero or more rows
NULL
DWMLDataRefClass$methods(
    get_data = function(
        name = .self$list_parameters()[[1]][['name']], 
        key = .self$list_parameters()[[1]][['time-layout']]){
            
        R <- data.frame()
        
        # does the key fit?
        if (!(key[1] %in% names(.self$time_layout))){
            cat("key not found:", key[1], "\n")
            return(R)
        }
        
        # we look at the parameter list for the the first point
        pnames <- sapply(.self$data_list, "[[", "name")
        playout <-  sapply(.self$data_list, "[[", "time-layout")
        ix <- pnames %in% name[1]
        if (!any(ix)){
            cat("parameter not found:", name[1], "\n")
            return(R)
        }
        
        iy <- playout %in% key[1]
        if (!any(ix & iy)){
            cat("key", key[1], "not matched for", name[1], "\n")
            return(R)
        }
      
        tl <- .self$node %>%
            xml2::xml_find_all(paste0('parameters/', name[1])) %>%
            xml2::xml_attr('time-layout')
        
        ix <- tl %in% key[1]
        
        points <- xml2::xml_find_all(.self$node, 'parameters')[ix] %>%
            xml2::xml_attr('applicable-location')
        
        PP <- xml2::xml_find_all(.self$node, paste0('parameters/', name[1]))[ix]  
              
        v <- do.call(cbind,
            lapply(PP, 
                function(P){
                    P %>%
                    xml2::xml_find_all("value") %>%
                    xml2::xml_text()
                }))
        mode(v) <- 'numeric'
                
        colnames(v) <- points
        data.frame(.self$time_layout[[key[1]]], v, stringsAsFactors = FALSE)   
    })


###########  methods above
###########  functions below


#' Extract one parameter from an xml_node
#' 
#' @param x xml_node
#' @return a list with at least 'name' and 'value'
get_one_parameter <- function(x){
    a <- as.list(xml2::xml_attrs(x))
    a[['name']] <- xml2::xml_name(x)
    #names(a) <- gsub("-", "_", names(a))
    
    a[['value']] <- switch(a[['name']],
        'weather' = sapply(xml2::xml_find_all(x, "weather-conditions"), xml2::xml_text),
        'conditions-icon' = sapply(xml2::xml_find_all(x, 'icon-link'), xml2::xml_text),
        'wordedForecast' = sapply(xml2::xml_find_all(x,'text'), xml2::xml_txt),
        sapply(xml2::xml_find_all(x,'value'), xml2::xml_double))
    a
} #get_one_parameter


#' Cast a parameter to character (suitable for DWMLDataRefClass$show)
#'
#' @param x a list of one or more parameter lists
#' @return character representation of the parameters
parameter_to_string <- function(x){
     sapply(x, 
        function(x){
            sprintf("%s, type = %s, units = %s, time-layout = %s", 
            x[['name']], x[['type']], x[['units']], x[['time-layout']])
        })
} #parameter_to_string


#' Retrieve a list of time layout keys for parameters
#'
#' @export
#' @param x DWMLDataRefClass or XMLNode
#' @return a named list of time-layout keys for parameters
list_parameter_keys <- function(x){
    if (inherits(x, 'DWMLBaseRefClass')){
        node <- X$node
    } else if (inherits(x, 'xml_node')){
        node <- x
    } else {
        stop('input must inherit from ndfd::DWMLBaseRefClass or xml2::xml_node')
    }
    pnodes <- xml2::xml_find_all(node, 'parameters')
    if (inherits(pnodes, "xml_missing")) return(NULL) else return(pnodes)
}

#' Retrieve a list of the time-layout keys in a DWMLDataRefClass
#' 
#' @export
#' @param x DWMLDataRefClass or XMLNode
#' @return character vector or NULL
list_time_layout_keys <- function(x){
    if (inherits(x, 'DWMLBaseRefClass')){
        node <- X$node
    } else if (inherits(x, 'xml_node')){
        node <- x
    } else {
        stop('input must inherit from ndfd::DWMLBaseRefClass or xml_node')
    }
    
    tlnodes <- xml2::xml_find_all(node,'time-layout')
    if (inherits(tlnodes, 'xml_missing')) return(NULL)
    
    sapply(tlnodes, function(x) {
        key <- xml2::xml_find_first(x, 'layout-key')
        if (!inherits(key, 'xml_missing')) xml2::xml_text(key) else NULL
        })
} #list_time_layout_keys

#' Retrieve a list of time layout data frames
#'
#' @param obj the DWMLDataRefClass object
#' @param key charcater vector of one or more time-layout-keys or NULL to get all
#' @param form character the time format to retrieve
#' @return a list of one or more data frames
DWML_get_time_layout <- function(obj, key = NULL, 
    form = c("POSIXct", "character")[1]){
        allkeys <- list_time_layout_keys(obj$node)
        if (!is.null(key)){
            ix <- key %in% allkeys
        } else {
            ix <- rep(TRUE, length(allkeys))
        }
        
        tl <- xml2::xml_find_all(obj$node, 'time-layout')
        #tl <- self$node[names(self$node) %in% 'time-layout']
        if (length(tl) ==0) return(list())
        xx <- lapply(tl[ix],
            function(x, as_posixct = TRUE){
                starts <- sapply(xml2::xml_find_all(x,'start-valid-time') ,xml2::xml_text)
                iend <- xml2::xml_find_all(x, "end-valid-time")
                if (length(iend) > 0){
                    ends <- sapply(iend, xml2::xml_text)
                } else {
                    ends <- rep(NA, length(starts))
                }   
                if (as_posixct){
                    starts <- as.POSIXct(starts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
                    ends <- as.POSIXct(ends, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
                }
                x <- data.frame(start_valid_time = starts, 
                    end_valid_time = ends, stringsAsFactors = FALSE)
                
            }, 
            as_posixct = tolower(form[1]) == 'posixct')
        
        #names(xx) <- sapply(tl, function(x) 
        #    xml2::xml_text(xml2::xml_find_first(x, 'layout-key')) )
        names(xx) <- allkeys[ix]
           
        return(xx)
    } #DWML_get_time_layout