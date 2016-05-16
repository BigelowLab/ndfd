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
    + version character, the response version
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

xy <- X$latLonList$get_location()
str(xy)
# 'data.frame':	1176 obs. of  2 variables:
#  $ lat: num  43.1 43.2 43.3 43.4 43.6 ...
#  $ lon: num  -72.1 -72 -72 -72 -72 ...
```

We can get [forecast values](http://graphical.weather.gov/xml/docs/elementInputNames.php) by passing the above locations to a subsequent query.  ALternatively, we can pass a bounding box instead of each of the subgrid locations.

```R

my_query <- 



![multiple_points](https://github.com/BigelowLab/ndfd/blob/master/inst/images/multiple_points.png)

##### Get the temperature at series of locations by zip code

We retrieve the data associated with zip codes in Cumberland County, Maine.
```
library(ndfd)
zips <- "04003+04009+04011+04013+04107+04015+04017+04019+04021+04110+04024+04105+04032+04038+04039+04079+04040+04050+04055+04260+04057+04097+04066+04108+04101+04069+04071+04070+04029+04075+04077+04078+04106+04082+04084+04085+04091+04092+04062+04096"
my_query <- query_this(what = 'zipcodes', zipCodeList = zips, element = 'temp')
X <- NDFD(my_query)

x <- X$data$get_data(name = 'Temperature', by = 'location')

# each column 'Vn' is the parameter value at the time specified in the 
# time-layout values.  
head(x[1:5])
# location_key latitude longitude  V1 V2
#       point1    43.73    -70.00  -8 -6
#       point2    44.07    -70.72 -10 -6
#       point3    43.91    -69.97  -9 -7
#       point4    43.80    -70.07  -9 -6
#       point5    43.56    -70.20  -8 -6
#       point6    44.01    -70.52 -11 -7

y <- X$data$get_data(name = 'Temperature', by = 'time')

# column 'pointN' is the parameter value at the listed time-layout window
# in this case, 'Temperature' is valid only at the start times listed 
# (once every three hours) 
head(y[1:5])
#    start_valid_time end_valid_time point1 point2 point3
# 2016-03-03 10:00:00           <NA>     -8    -10     -9
# 2016-03-03 13:00:00           <NA>     -6     -6     -7
# 2016-03-03 16:00:00           <NA>     -7     -8     -8
# 2016-03-03 19:00:00           <NA>     -8    -11    -11
# 2016-03-03 22:00:00           <NA>     -9    -13    -12
# 2016-03-04 01:00:00           <NA>     -9    -15    -12
```

