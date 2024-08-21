**_World Justice Project EUROVOICES_** provides new data that captures the perceptions and 
experiences of people living in 110 subnational regions across the 27 EU Member States in the 
areas of justice, democracy and the rule of law. It is based on the surveys of more than 8,000
local and independent legal experts, as well as household surveys to more than 64,000 respondents.
With this data, the World Justice Project (WJP) seeks to contribute to evidence-based decision-
making processes at all government levels and help decision makers in identifying strengths,
weaknesses, and policy priorities in their regions. 

The data is organized into three _thematic reports_:
(1) Democracy & Fundamental Rights
(2) Justice & Safety
(3) Corruption & Transparency

This Github repository contains the code used to wrangle, clean and transform all necessary data
as well as produce the html reports and all of its data vizualizations. In order to achieve this,
the DAU followed the specifications set by the communications and design teams. 

## Files description
The repository is divided into to primary directories ['data-viz'] 
(https://github.com/WJP-DAU/EU-thematic-reports/tree/main/data-viz) and ['html'] 
(https://github.com/WJP-DAU/EU-thematic-reports/tree/main/html).

The 'data-viz' directory contains all routines used to produce the data
visualizations for all three reports. All final charts were produced using a 
combination of R and Javascript, which was employed to add dynamic tooltips to the 
static SVG's produced by ggplot. For the static charts, the [WJP-R Package]
(https://github.com/ctoruno/WJPr) developed by [ctoruno] 
(https://github.com/ctoruno) was used. This directory follows the DAU's usual
modular programming style.

The data used in this project is a subset from an organization-managed merged .dta
file. Because of the privacy guidelines followed by the DAU (see the 
[DAU R Coding Handbook](https://ctoruno.quarto.pub/wjp-r-handbook/)), the contents of 
the 'Outputs' directory is minimal on the public Github repository. Its content can be 
accessed through WJP Sharepoint.

The 'html' directory contains all the code used to produce the HTML version of the report.


## Code Logic
This repository is split into two major directories: ['data-viz'] 
(https://github.com/WJP-DAU/EU-thematic-reports/tree/main/data-viz) and ['html']
(https://github.com/WJP-DAU/EU-thematic-reports/tree/main/html). 

For the data-viz
directory, the general framework which dictates how the process is conducted is 
contained in the 'report_outline.xlsx' file. This document contains one row for each
visualization produced by the workflow. For each visualization, the outline keeps 
track of which report, chapter and section the chart will appear in, whether it is 
a general population or expert indicator, whether the indicator plotted exists in 
the merged .dta file or additional wrangling is necessary, the geographic level of 
the chart (regional or national), the type of chart to be produced, the report 
value that this visualization seeks to capture, and other pertinent information like
the title and subtitle of the chart. 

The 'data-viz' process follows a similar modular programming approach to previous 
country reports. The routine can be run from top to bottom through the 'RunMe.R' 
script. The routine is divided into three steps:
1. Presettings: In this section, we load the additional code modules included in the
workflow. We also preprocess the data, calculate demographic information, and manually
join with the original general population indicators any indicators which are a direct
combination of others. We also load the color palettes and style themes for our
visualizations.
2. Data Wrangling: The first portion of wrangling is done in the presettings when
 feautures which indicate some combination of other predictors are engineered. There
 are three other wrangling functions which are applied to the data depending on the
 visualization needed. For the implementation, we create a named list to loop over
which keeps track of the _source_ of chart (General Population or Expert Survey - these
come from different data sets) and whether additional "special" wranglig is necessary. Based
on the chart id and the source we apply the required wrangling function and return the combined
list of the data points for each chart. We then bind the rows of this list and impute the
values for which sample size in insufficient to include the value in our report. Then we
apply an averaging function which calculates weighted means for each chart at the national leven
and a simple mean for the EU level.
3. Data Visualization: In this section of the code we call the appropriate visualization
function by extracting the relevant parameters from the outline and grabbing the data
points for that chart. The resulting charts are saved to the outputs directory in an SVG format.

Finally, the static charts are passed to the SVG handler, which uses a class to 
assign interactivity to the visualizations depending on their attributes. 

The 'html' process is still being constructed!

## Contact
For inqueries please contact _Carlos Toruño_ (ctoruno@worldjusticeproject.org) or 
_Isabella Coddington_ (icoddington@worldjusticeproject.org). For general information
on the EU Thematic Reports, please contact _Ana María Montoya_ 
(amontoya@worldjusticeproject.org).




