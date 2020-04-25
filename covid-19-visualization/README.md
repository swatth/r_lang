Lots of COVID-19 resource recently so I just picked up one from [GitHub (hosted by CCSE JohnS Hopkins Unversity)](https://github.com/CSSEGISandData/COVID-19) featuring a set of .csv files so getting the data and wrangling around is pretty straightforward (the data has been thru quite a bit of clean up already :-D )

In response to the article from ESRI: [Improve Your COVID-19 Cases Map](https://storymaps.arcgis.com/stories/1cbce9094e88438fa75148cb35f99caf), I experiment of cramming a bit more of info to the visualization, focusing on the state of the "spread" based on _active cases_ (`confirmed - deaths - recovered`) and the _growth_ relative to the number on the previous day and come up with a form of heat map. 

The graphics is created with R with only basic R commands, ggplot2, and ggmap.

Sample below is created by stacking up a plot for each day with data snapshop on April 22 to create an animated gif file - with some free on-line tool available on the Internet: [ezgif.com](https://ezgif.com/maker)

I hope it gives a sense of progression of the spread since the very beginning and emphasizes more on the situation based on the growth of active cases relative to the total active cases of the previous day.

![Active cases progression](https://github.com/swatth/r_lang/blob/master/covid-19-visualization/assets/Sample.gif)
