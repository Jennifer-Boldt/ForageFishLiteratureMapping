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

Here we read the unique values that can be chosen in the drop down menus
as pressures and we turn them into a vector of the pressures. Next we
take the column names for the outcomes portion of the table (these are
for example the growth\_rate) that will be influenced by the pressures.

``` r
pressures<-c(unique(unlist(dropdowns[,21])))
                  
outcomes<-colnames(ff_table)[22:42]
```

Here we make the data using the ForageFishLitReview function
“pressure\_table” by subsetting the outcomes columns and counting how
many times each of the pressures occurs in each column. Then we make the
table a bit prettier to look like Figure 13.

``` r
data1<-ff_table[,which(colnames(ff_table)%in%outcomes)]

pressure_data<-pressure_table(pressures,data1)
  
                      
                      
ggplot(pressure_data, aes(x=outcome, y=pressure, col = count, fill = count, label = count)) +
  geom_tile() +
  geom_text(col = "black") +
  theme_minimal() +
  scale_fill_gradient2(low = "white", mid = "orange", high = "red") +
  scale_color_gradient2(low = "white", mid = "orange", high = "red")
```

![](Code_files/figure-gfm/grep%20the%20words-1.png)<!-- -->
