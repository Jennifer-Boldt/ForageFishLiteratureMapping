Procedure for duplicate removal
================
Elisabeth
October 27, 2020

REVTOOLS
--------

Revtools is an Rpackage that has a build in application to help with duplicate removal. Informormation can be found here: <https://revtools.net/deduplication.html#locating-duplicates>.

A detailed description is provided on how to remove the duplicates.

INSTALLATION
------------

If this is your first time using this, copy-pase the code below into R:

``` r
# Install and load revtools package
install.packages("revtools")
```

USE
---

The application works with a user interface. The 4 lines of code below allow to open it and save the output. Much of it is descirbed in the link above.

Note that the app does not allow to check similarity between e.g. titles, while accounting for year and authors simultaniously.

Once the app is opened, duplicates can be removed;

1.  data tab: brows and select bib or ris files
2.  matching: select fuzzdist and yes to lower cases and removing punctuation info: <https://www.datacamp.com/community/tutorials/fuzzy-string-python>
3.  matching: calculate duplicates
4.  select entries you wish to keep in the main pannel, or indicate that these are no duplicates
5.  once finished, click cancel when asked to save results as bib/ris again (unless csv is desired)
6.  !! data: EXIT APP (this is to save the new object into the R space)
7.  save output with command below

``` r
# load the pacakge in R
library("revtools")                

# open the application. Once finished and closed, the results will be stored in 'refs_new'
# refs_new <- screen_duplicates()   
# nrow(refs_new)
# write_bibliography(refs_new,'this_is_the_file_name.bib',format = 'bib') # save as bib/ris
```
