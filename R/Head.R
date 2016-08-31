# http://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXMLclient.php?listLatLon=38.99,-77.02%2039.70,-104.80%2047.6,-122.30&product=time-series&begin=&end=&Unit=m&maxt=maxt&mint=mint

#' A DWML class for for the head section
#'
#' The head section doesn't provide much dynamic info so we have only a 
#' very simple behavior associated with it.  
#'
#' @include Base.R
#' @export
DWMLHeadRefClass <- setRefClass("DWMLHeadRefClass",
    
    contains = 'DWMLBaseRefClass',

    methods = list(
        show = function(prefix = ""){
            callSuper(prefix = prefix)
            prod <- xml2::xml_find_first(.self$node, 'product')
            a <- xml2::xml_attrs(prod)       
            srsName <- a[['srsName']]
            concise_name <- a[['concise-name']]
            operational_mode <- a[['operational-mode']]
            title <- xml2::xml_text(xml2::xml_find_first(prod, 'title'))
            field <- xml2::xml_text(xml2::xml_find_first(prod, 'field'))
            category <- xml2::xml_text(xml2::xml_find_first(prod, 'category'))
            creation_date <- 
                xml2::xml_text(xml2::xml_find_first(prod, 'creation-date'))
            a <- xml2::xml_attrs(xml2::xml_find_first(prod, 'creation-date'))
            refresh_frequency = a[['refresh-frequency']]

            cat(prefix, " Title: ", title, "\n", sep = "")
            cat(prefix, " Concise name: ", concise_name, "\n", sep = "")
            cat(prefix, " Creation date: ", creation_date, "\n", sep = "")
            cat(prefix, " Refresh frequency: ", refresh_frequency, "\n", sep = "")  
        })
)


#' Retrieve the concise name of the data set
#'
#' @name DWMLHeadRefClass_concise_name
#' @return character value of the concise-name attribute
NULL 
DWMLHeadRefClass$methods(
    concise_name = function(){
        prod <- xml2::xml_find_first(.self$node, 'product')
        a <- xml2::xml_attrs(xml2::xml_find_first(prod,'product'))
        a[['concise-name']]
    })

#' Retrieve the creation date of the data set ala '2016-02-26T13:25:37Z'
#'
#' @name DWMLHeadRefClass_creation_date
#' @param form character either 'character' or 'POSIXct'
#' @return character of POSIXct value of the creation date
NULL 
DWMLHeadRefClass$methods(
    creation_date = function(form = c("POSIXct", "character")[1]){
        #cd <- xml_value(.self$node[['product']][['creation-date']])
        cd <- xml2::xml_text(xml2::xml_find_first(.sefl$node, "product/creation-date"))
        switch(tolower(form[1]),
            'posixct' = as.POSIXct(cd, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            cd)
    })
    