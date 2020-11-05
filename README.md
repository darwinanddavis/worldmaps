# Some spatial data analysis projects 

### Matt Malishev      

Versions:    
 - R 3.5.0    
 - RStudio 1.1.453        
  
File extensions:     
.R    
.Rmd    
.html      

******      

## 30 day map challenge     

Map entries for the #30dayMapChallenge for November 2020.                 

### [Day 1 – Points](https://darwinanddavis.github.io/worldmaps/30daymap2020/day1)       

An interactive map of my favourite coffee spots around the world using Mapbox Studio and `mapdeck` in `R`.       

<div align="center"; text-align:center>
  <img src="img/coffee.jpg", width = "100%", height = "25%">  
</div>  

### Tools   

R       
R packages: `dplyr`, `mapdeck`, `readr`, `purrr`, `stringr`, `tibble`, `htmltools`, `sf`, `sfheaders`, `data.table`, `stringr`, `tigris`, `sp` , `here`    

### Day 2: Lines      

Using geolocation data to map a roadtrip across the southern US with `R`.  

### Tools      
```  
pacman::p_load(here,sf,RColorBrewer,dplyr,ggmap,sp,maptools,scales,rgdal,ggplot2,jsonlite,readr,devtools,colorspace,mapdata,ggsn,mapview,mapproj,ggthemes,reshape2,grid,rnaturalearth,rnaturalearthdata,ggtext,purrr)            
```
  
<div align="center"; text-align:center>
  <img src="img/day2.png", width = "100%", height = "25%">  
</div>  

### Links      
[`R` code](https://github.com/darwinanddavis/worldmaps/tree/gh-pages/docs/30daymap2020)        

 <!--  -->
 <!--  -->  
 <!--  -->

### Day 4: Hexagons

Mapping my Lyft ride activity from June 2018 to March 2020. 

Using geolocation data for my Lyft rides as a passenger to create an interactive map that shows my destination and origin locations. The data covers the USA.       

* Zoom out to see the cities where I used Lyft to get around. Cities with labels contain data, sometimes only a few points.       
* Note the legend in the below images in case the legend in the link is chopped off.          

### [Mapping Lyft ride data](https://darwinanddavis.github.io/worldmaps/30daymap2020/day4)  

Atlanta, USA (where I lived during this time)        
<div align="center"; text-align:center>
  <img src="img/day4.png", width = "100%", height = "25%">    
</div>  

### Tools     
  
R         
R packages: `dplyr`, `mapdeck`, `tibble`, `htmltools`, `sf`, `sfheaders`, `data.table`, `stringr`, `tigris`, `sp` , `here`,`maps`, `colorspace`   

 <!--  -->
 <!--  -->  
 <!--  -->

## Realtime interactive map of coronavirus 2019-nCov global distribution   

[![eRum2020::CovidR](https://badgen.net/https/runkit.io/erum2020-covidr/badge/branches/master/malishev-covid19?cache=300)](https://milano-r.github.io/erum2020-covidr-contest/malishev-covid19.html)    

### [Coronavirus 2019-nCov global distribution map](https://darwinanddavis.github.io/worldmaps/coronavirus.html)          

Realtime updates of 2019-nCov global distribution from live scraped [data from the European Centre for Disease Prevention and Control (ECDC)](https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases).      

### Tools  

R, HTML, CSS    
R packages: `maps`,`readr`,`dplyr`,`leaflet`,`xml2`,`rvest`,`ggmap`,`geosphere`,`htmltools`,`mapview`,`rnaturalearth`,`purrr`          


<div align="center"; text-align:center>
  <img src="img/coronavirus.jpg", width = "100%", height = "25%">  
</div>  
  
## Visualising Airbnb open data    

### [**San Francisco property type and ratings**](https://darwinanddavis.github.io/worldmaps/airbnb_sf.html)          

Spatial analysis of Airbnb listing and ratings for the San Francisco area.    

### Tools  

R, HTML, CSS  
R packages: `readr` `dplyr`, `leaflet`, `colorspace`   

<div align="center"; text-align:center>
  <img src="img/airbnb_sf.jpg", width = "100%", height = "25%">  
</div>


## Maintainer    

**Matt Malishev**     
:mag: [Website](https://darwinanddavis.github.io/DataPortfolio/)      
:bird: [@darwinanddavis](https://twitter.com/darwinanddavis)    
:email: matthew.malishev [at] gmail.com        

