# ndfd
A RESTful R client interface to the NWS [National Digital Forecast Database](http://graphical.weather.gov/xml/rest.php)

## Requirements
+ [R 3.2](https://www.r-project.org/)
+ [httr](https://cran.r-project.org/web/packages/httr/index.html) package
+ [XML](https://cran.r-project.org/web/packages/XML/index.html)

## Installation
```R
library(devtools)
install_github("BigelowLab/ndfd")
```

### Overview

You can use ndfd to both list available data lcoations as well as retrieve them. For each you must create a suitable query string suing either `query_this()` of `list_this()` which are each convenience wrappers.  Once you have a query string you can pass it to `NDFD()` to get the response form the server and store it in an `NDFDRefClass` object.  The contents of the `NDFDRefClass` will vary slightly depending upon your request.

Each `NDFDRefClass` has 4 fields...

    + uri character, the complete URI with your query string
    + version character, the repsonse version
    + head DWMLHeadRefClass an object to contain the header section (not always populated)
    + data DWMLDataRefClass an object to contain the data section (not always populated)
    + latLonList DWMLLonLatListRefClass  an object to contain the location listings (not always populated)
   

##### Example find the data locations within a region

In this example we simply want to list available resources that fall within a specified boundary.  Note that we only get a listing of locations where we can query for data.

```R
library(ndfd)
my_query <- list_this(what = "points_in_subgrid",  listLon1 = -72, listLon2 = -63, listLat1 = 39, listLat2 = 46)
X <- NDFD(my_query)
X
# URI:http://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXMLclient.php?listLon1=-72.0000&listLon2=-63.0000&listLat1=39.0000&listLat2=46.0000&resolutionList=20.0000
# Reference Class: "NDFDRefClass"
#   Reference Class: "DWMLLatLonListRefClass"
#    locations (head and tail):
#         lat        lon
# 1 39.008394 -71.990669
# 2 39.139234 -71.961769
# 3 39.269988 -71.932797
# 4 39.400657 -71.903753
# 5 39.531239 -71.874636
# 6 39.661734 -71.845447
#            lat        lon
# 2555 45.403962 -63.198672
# 2556 45.528282 -63.156282
# 2557 45.652481 -63.113782
# 2558 45.776558 -63.071170
# 2559 45.900512 -63.028446
# 2560 46.024344 -62.985609
```

![multiple_points](https://github.com/BigelowLab/ndfd/blob/master/inst/images/multiple_points.png)

##### Get the temperature at series of locations

There is a 200 point limit for this request, so we'll try down the number of points by increasing the value of resolutionList from the default (5km) to 75km.

```R
my_query <- list_this(what = "points_in_subgrid",  listLon1 = -72, listLon2 = -63, listLat1 = 39, listLat2 = 46, resolutionList = 75)
X <- NDFD(my_query)
loc <- X$latLonList$extract_location(form = 'as_is')
my_query <- query_this(what = 'multipoint', listLatLon = loc, element = 'temp')
Y <- NDFD(my_query)




##### Example - retrieve data by one or more zip codes


```R
library(ndfd)
my_query <- query_this(what = "zipcodes", zipCodeList = "04096+04539")
X <- NDFD(my_query)
X
# URI: http://graphical.weather.gov/xml/sample_products/browser_interface/ndfdXMLclient.php?zipCodeList=04096+04539&product=time-series&begin=&end=&Unit=m&mint=mint&maxt=maxt&temp=temp 
# Reference Class: "NDFDRefClass"
#   Reference Class: "DWMLHeadRefClass"
#    Title: NOAA's National Weather Service Forecast Data
#    Concise name: time-series
#    Creation date: 2016-03-02T16:53:29Z
#    Refresh frequency: PT1H
#   Reference Class: "DWMLDataRefClass"
#    locations:
#        location_key latitude longitude
# point1       point1    43.80    -70.20
# point2       point2    43.96    -69.51
#    timelayout(s):k-p24h-n7-1 k-p24h-n6-2 k-p3h-n35-3
#    parameter(s):
#    point1
#     Daily Maximum Temperature, type = maximum, units = Celsius, time_layout = k-p24h-n7-1
#     Daily Minimum Temperature, type = minimum, units = Celsius, time_layout = k-p24h-n6-2
#     Temperature, type = hourly, units = Celsius, time_layout = k-p3h-n35-3
#    point2
#     Daily Maximum Temperature, type = maximum, units = Celsius, time_layout = k-p24h-n7-1
#     Daily Minimum Temperature, type = minimum, units = Celsius, time_layout = k-p24h-n6-2
#     Temperature, type = hourly, units = Celsius, time_layout = k-p3h-n35-3
```
