Calculate statistics on literature review for forage fishes in
standardized method and with standardized output
================
Chris Rooper, Jennifer Boldt
October 15, 2020

\#\#PURPOSE The purpose of this package is to provide a set of functions
that convert the forage fish literature review template into data that
can be processed into a standard set of tables and figures.

\#\#INSTALLATION To install the package you must use the “devtools”
package from CRAN. If you have not already installed it use:

``` r
# Install and load devtools package
install.packages("devtools")
library("devtools")
```

Next install the ForageFishLitReview package from GitHub using the
devtools package function “install\_github”.

``` r
#Install the ForageFishLitReview package
install_github("rooperc4/ForageFishLitReview")
```

    ## Downloading GitHub repo rooperc4/ForageFishLitReview@master

    ##          checking for file 'C:\Users\rooperc\AppData\Local\Temp\2\RtmpEpR4oJ\remotes3a6c4e5836ce\rooperc4-ForageFishLitReview-bbecf1f/DESCRIPTION' ...     checking for file 'C:\Users\rooperc\AppData\Local\Temp\2\RtmpEpR4oJ\remotes3a6c4e5836ce\rooperc4-ForageFishLitReview-bbecf1f/DESCRIPTION' ...   v  checking for file 'C:\Users\rooperc\AppData\Local\Temp\2\RtmpEpR4oJ\remotes3a6c4e5836ce\rooperc4-ForageFishLitReview-bbecf1f/DESCRIPTION' (563ms)
    ##       -  preparing 'ForageFishLitReview':
    ##      checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   v  checking DESCRIPTION meta-information
    ##       -  checking for LF line-endings in source and make files and shell scripts
    ##       -  checking for empty or unneeded directories
    ##       -  building 'ForageFishLitReview_0.0.0.9000.tar.gz'
    ##      
    ## 

``` r
library(ForageFishLitReview)
```

\#\#DATA The example data set included in this package is made up, but
follows the specified template. In the package it is called
litdata.xlsm. It is the template data sheet into which the literature
review has been compiled. Importantly, the excel workbook must include
at least two worksheets: one containing the data and one containing the
drop-down menus.

Load the data from the example file (included in the package).

``` r
ExampleFilePath<-system.file("extdata","Literature_Data.xlsm",package="ForageFishLitReview")

ff_table<-read_excel(ExampleFilePath,sheet="Example 2")

dropdowns<-read_excel(ExampleFilePath,sheet="Do Not Edit dropdown options")
```

``` r
pressures<-c(unique(dropdowns[,21]))
                  
outcomes<-colnames(ff_table)[22:42]
```

\#\#EXAMPLE As an example of calculating the condition index, we take
Pacific Cod in the eastern Bering Sea. The code is designed to loop
through a number of species (e.g. see the EBS\_Groundfish\_COndition.Rmd
file which computes the index for a group of species used in the
Ecosystem Considerations SAFE Chapter). For this example, we are doing
only a single species, but have left the architecture for multiple
species intact. Also, please note that some of the steps such as the
STRATUM definitions are specific to the RACEBASE EBS data and may not be
needed for other applications.

Code to make figure 13

``` r
data1<-ff_table[,22:42]

print(pressure_table(pressures,data1))
```

    ##   DistributionHabitatUse
    ## 1                      0
    ##   GrowthLifeHistory_MassOrSize
    ## 1                            0
    ##   GrowthLifeHistory_Condition
    ## 1                           0
    ##   GrowthLifeHistory_Foraging
    ## 1                          0
    ##   GrowthLifeHistory_Other
    ## 1                       0
    ##   Survival_Survival&Mortality Performance_Stress
    ## 1                           0                  0
    ##   Performance_Energy Performance_Swimming
    ## 1                  0                    0
    ##   Performance_Other Migration_Spawning
    ## 1                 0                  0
    ##   Migration_Other Reproduction_Maturation
    ## 1               0                       0
    ##   Reproduction_Spawning Reproduction_EggAbundance
    ## 1                     0                         0
    ##   Reproduction_EggSizeOrWeight
    ## 1                            0
    ##   Reproduction_Fecundity Reproduction_Other
    ## 1                      0                  0
    ##   Productivity_BiomassOrAbundance
    ## 1                               0
    ##   Productivity_Recruitment Productivity_Other
    ## 1                        0                  0
    ##                                                                                                                                                                                                                                                                       Pressure
    ## 1 Climate (NAO, NPGO, etc), transport (currents, etc), temperature, salinity, oxygen, upwelling (magnitude or timing), phytoplankton (primary productivity), zooplankton (secondary productivity), predators, competitors, fisheries, other pressure, not linked to a pressure
