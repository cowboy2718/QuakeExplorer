### Travis Badge

The following indicates the status of the most recent build with Travis:

[![Build Status](https://travis-ci.org/cowboy2718/QuakeExplorer.svg?branch=master)](https://travis-ci.org/cowboy2718/QuakeExplorer)

## QuakeExplorer
R package to explore historical earthquake data found in the (National Oceanic and Atmospheric Administration) NOAA Significant Earthquakes dataset.  
Tony Gojanovic   
September 2018  

### Background

This package provides tools for the exploration of earthquakes from the NOAA (National Oceanic and Atmospheric Administration) Significant Earthquake dataset.  Data can be obtained from the NOAA at https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1.

### What is an Earthquake?

From Wikipedia, we know that an "earthquake (also known as a quake, tremor or temblor) is the shaking of the surface of the Earth, resulting from the sudden release of energy in the Earth's lithosphere that creates seismic waves. Earthquakes can range in size from those that are so weak that they cannot be felt to those violent enough to toss people around and destroy whole cities. The seismicity, or seismic activity, of an area is the frequency, type and size of earthquakes experienced over a period of time. The word tremor is also used for non-earthquake seismic rumbling."

The Significant Earthquake Database contains information on destructive earthquakes from 2150 B.C. to the present that meet at least one of the following criteria: Moderate damage (approximately $1 million or more), 10 or more deaths, Magnitude 7.5 or greater, Modified Mercalli Intensity X or greater, or the earthquake generated a tsunami.

### What Does this Package Do?

This package, **QuakeExplorer**, is used to visually explore a specific dataset collected by the National Oceanic and Atmospheric Adminstration.  It is expected to be used as a preliminary method or jumping point for further studies.  This program can be used by geologists, archeologists, astronomers, anthropologists to name but a few disciplines that would find this data useful.

Two types of displays are shown below.

### Leaflet Map of Earthquakes

This package will create leaflet maps of earthquakes for a given region and time period.  Below is an example of a leaflet map of Greek earthquakes between 1900 and 1950 (note the example is not interactive).

![Greek earthquakes](greece.png)

### Time Series of Earthquakes

This package will create a time series map of earthquakes for a given regions and time periods.  Below is an example of a earthquakes occurring between 1900 and 1950 for the USA, Greece and China.

![Time Series](Timeseries.png)

### Package Name and Location

To install this package and begin exploring earthquakes, use devtools and the following:

```r
devtools::install_github('cowboy2718/QuakeExplorer')
library(QuakeExplorer)
```

### Package Details

Specific details on the package QuakeExplorer may be obtained through the vignette files.  The vignette files show specific usage and syntax.

### Package Details

Specific details on the package QuakeExplorer may be obtained through the vignette files.  The vignette files show specific usage and syntax of the functions used in this package.

### References

The following are useful resources.

National Geophysical Data Center / World Data Service (NGDC/WDS): Significant Earthquake Database. National Geophysical Data Center, NOAA. doi:10.7289/V5TD9V7K

https://en.wikipedia.org/wiki/Earthquake

### Useful Links in the Development of this Package:

The following were very useful links used in the development of this project.  RStudio functionality was used in both package development and GitHub integration.

* [Building R Packages in R Studio, John Muschelli](https://www.youtube.com/watch?v=OIirKRgIsdc) 
* [Putting your R Package on Github, Karl Broman](http://kbroman.org/pkg_primer/pages/github.html) 
* [RStudio & Github Integration, James Dayhuff](https://www.youtube.com/watch?v=E2d91v1Twcc&t=597s) 
* [Creating RStudio projects from GitHub Repositories, Nicholas Reich ](https://www.youtube.com/watch?v=YxZ8J2rqhEM) 
* [Developing R Packages | Part 7: Unit Testing, Colin Pistell](https://www.youtube.com/watch?v=u2KDSY_8Ay4) 
* [Travis CI Tutorial - How to Use Travis CI with Github for Continuous Integration, Full Stack Academy](https://www.youtube.com/watch?v=Uft5KBimzyk)
