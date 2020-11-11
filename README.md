Calculate statistics on literature review for forage fishes in standardized method and with standardized output
================
Chris Rooper, Jennifer Boldt
October 15, 2020

PURPOSE
-------

The purpose of this package is to provide a set of functions that convert the forage fish literature review template into data that can be processed into a standard set of tables and figures.

INSTALLATION
------------

To install the package you must use the "devtools" package from CRAN. If you have not already installed it use:

``` r
# Install and load devtools package
#install.packages("devtools")
library("devtools")
# only run if you change a function:
#library(roxygen2)
#library(digest)
#document()
```

Next install the ForageFishLitReview package from GitHub using the devtools package function "install\_github".

``` r
#Install the ForageFishLitReview package
install_github("rooperc4/ForageFishLitReview")
library(ForageFishLitReview)
```

DATA
----

The example data set included in this package is made up, but follows the specified template. In the package it is called litdata.xlsm. It is the template data sheet into which the literature review has been compiled. Importantly, the excel workbook must include at least two worksheets: one containing the data and one containing the drop-down menus.

Load the data from the example file (included in the package).

``` r
library(readxl)
ExampleFilePath<-system.file("extdata","Literature_Data.xlsm",package="ForageFishLitReview")

ff_table<-read_excel(ExampleFilePath,sheet="Example 2")
```

    ## New names:
    ## * `` -> ...43

``` r
dropdowns<-read_excel(ExampleFilePath,sheet="Do Not Edit dropdown options")
```

Here we read the unique values that can be chosen in the drop down menus as pressures and we turn them into a vector of the pressures. Next we take the column names for the outcomes portion of the table (these are for example the growth\_rate) that will be influenced by the pressures.

``` r
pressures<-c(unique(unlist(dropdowns[,21])))
                  
outcomes<-colnames(ff_table)[22:42]
```

Here we make the data using the ForageFishLitReview function "pressure\_table" by subsetting the outcomes columns and counting how many times each of the pressures occurs in each column. Then we make the table a bit prettier to look like Figure 13.

``` r
library(ggplot2)

data1<-ff_table[,which(colnames(ff_table)%in%outcomes)]

pressure_data<-pressure_table(pressures,data1)
  
library(splitstackshape)
```

    ## Warning: package 'splitstackshape' was built under
    ## R version 3.6.3

``` r
pressure_data$outcomeforsplit<-pressure_data$outcome
pressure_data<-cSplit(pressure_data, "outcomeforsplit", sep="_", type.convert=FALSE)
colnames(pressure_data)<-c("pressure","outcome","count","outcome1","outcome2")

#change the order in which the pressures are plotted 
pressure_data$pressureOrdered = factor(pressure_data$pressure, levels=rev(c(unique(pressures))))
                                                                          
#still need to work on how to label using outcome1 and outcome2
ggplot(pressure_data, aes(x=outcome, y=pressureOrdered, col = count, fill = count, label = count)) +
  geom_tile(color="grey") +
  #geom_text(col = "black") +
  geom_text(data=subset(pressure_data,count != 0),col="black")+
  theme_minimal() +
  scale_fill_gradient2(low = "white", high = "red") +
  scale_color_gradient2(low = "white", high = "red")+
  scale_x_discrete(position = "top")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

![](Code_files/figure-markdown_github/grep%20the%20words-1.png)

``` r
###     ***

#then need to create species specific / region specific plots at some point #in the future


#number of references by report type and publication year across all species
library(dplyr)
StudiesByYr_Type <- ff_table %>%
  group_by(`Year published`,`Report type`) %>%
  count()
colnames(StudiesByYr_Type)<-c("Year","ReportType","count")
#colour blind palette
cb1Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")

ggplot(StudiesByYr_Type)+
  geom_bar(aes(y=count, x=Year, fill=ReportType),stat="identity")+
  scale_fill_manual(values=cb1Palette)
```

![](Code_files/figure-markdown_github/grep%20the%20words-2.png)

``` r
#life history stage by life history stage and species
library(dplyr)
#get unique list of life history stages
lifestage<-c(unique(unlist(dropdowns[,12])))

#get unique list of species
LHS_outcomes<-c(unique(unlist(ff_table$`Focus species Common Name`)))

#put species into columns with life history stages as values for the rows in those columns
data2<-pivot_wider(ff_table,id_cols=NULL,names_from = "Focus species Common Name",values_from="Life stage studied (Adult, Juvenile, Larval, Egg)")
#subset the data to include only the species columns
data2<-data2[,c(which(colnames(data2)%in%LHS_outcomes))]

#Here we make the data using the ForageFishLitReview function "lifestage_table" by subsetting the outcome column and counting how many times each of the lifestages occurs in the column. 
#use the function lifestage_table
lifestage_data<-lifestage_table(lifestage,data2)

#rename columns for ease
colnames(lifestage_data)<-c("Lifestage","Fish", "Count")

#barplot of the count of references by species and life history stage
#remove NAs for plotting
lifestage_data<-na.omit(lifestage_data)
#change the order in which the life history stages are plotted and listed in the legend
lifestage_data$LifestageOrdered = factor(lifestage_data$Lifestage, levels=c("Adult","Juvenile","Larval","Egg","not specified"))
#colour blind palette
cb2Palette <- c("#0072B2","#56B4E9","#009E73","#E69F00", "#999999")
#stacked barplot
ggplot(lifestage_data)+ 
  geom_bar(aes(x=Fish, y=Count, fill=LifestageOrdered), stat="identity")+
  scale_colour_discrete(na.translate = F) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values=cb2Palette)
```

![](Code_files/figure-markdown_github/grep%20the%20words-3.png)
