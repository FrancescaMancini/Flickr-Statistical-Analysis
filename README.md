# Flickr-Statistical-Analysis
R code for temporal and spatial validation of geotagged pictures from Flickr as a proxy for wildlife tourism in Scotland.


WaveletAnalysis.R contains R code to conduct validation of temporal trends of geotagged pictures uploaded on Flickr as a proxy for temporal trends of visitation to the Cairngorrms National Park. The analysis uses two datasets: CairngormsFVD.txt and CairngormsSVD.txt, respectively collected from Flickr and the National Park authority.


SpatialValidation.R contains code to conduct validation of spatial distribution of geotagged pictures uploaded on Flickr as a proxy for spatial trends of wildlife watching activities in Scotland. The analysis is repeated 3 times at 3 different spatial resolutions, 20, 10 and 5 km. This analysis uses 3 datasets: flickr_survey_pop20KM.txt, flickr_survey_pop10KM.txt, flickr_survey_pop5KM.txt.

DensityMaps.R contains R code to produce density maps of wildlife watching hotspots. The same graphs are produced for bird watching (with and without pictures taken in Edinburgh and Glasgow), seal watching and whale and dolphin watching. The code uses 3 datasets: birdwatchingmined.txt, sealwatching.txt and Whale&Dolphinwatching.txt.
