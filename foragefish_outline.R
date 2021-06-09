#========================================
# title: "Brooke pressure map.R"
# author: Brooke Hackett
# packages:
library("devtools")
library(ForageFishLitReview)
library(readxl)
library(ggplot2)
library(splitstackshape)
library(grid)
library(tidyr)
library(dplyr)
library(admisc)
library(patchwork)
#=======================================
# Import data
#=======================================

# only need to read from database excel file in folder

ff_excel<-read_excel("draft_database_pacific.xlsm",sheet="Example 2")

dropdowns<-read_excel("draft_database_pacific.xlsm",sheet="Do Not Edit dropdown options")

#=========================================
# Prep data
#=========================================

# remove species that weren't completed

ff_table <- ff_excel %>%
  filter(., `Focus species Common Name` %in% c("Eulachon", "Pacific Herring", "Surf Smelt", "Whitebait Smelt"))


# Here we read the unique values that can be chosen in the drop down menus as pressures and we turn them into a vector of the pressures.
# Next we take the column names for the outcomes portion of the table (these are for example the growth_rate) that will be influenced by the pressures.

pressures<-c(unique(unlist(dropdowns[,21])))

# when using actual database file was getting an additional 'NA' row from the drop down list which caused an additional pressure row on graph
# labeled 'NA' and the count for every outcome column in the 'NA' row was the number of entries in the database.
# Found 'na.omit' function which returns the object with incomplete cases removed.

pressures1<-na.omit(pressures)

outcomes<-colnames(ff_table)[22:42]

# Here we make the data using the ForageFishLitReview function "pressure_table" by subsetting the outcomes columns and counting
# how many times each of the pressures occurs in each column. Then we make the table a bit prettier to look like Figure 13.

data1<-ff_table[,which(colnames(ff_table)%in%outcomes)]

# changed (pressures,data1) to (pressures1,data1) to remove 'NA' pressure row

pressure_data<-pressure_table(pressures1,data1)

# split outcome data into sub category

pressure_data$outcomeforsplit<-pressure_data$outcome
pressure_data<-cSplit(pressure_data, "outcomeforsplit", sep="_", type.convert=FALSE)
colnames(pressure_data)<-c("pressure","outcome","count","outcome1","outcome2")

# change the order in which the pressures are plotted

pressure_data$pressureOrdered = factor(pressure_data$pressure, levels=rev(c(unique(pressures))))

#=============================================
# Plotting the pressure map
#=============================================

#changing x axis labels to just the outcomes(outcome2), not outcome category_outcome(outcome)
#changed hjust to = 0 because format was messed up when separating into faucets

pmap2<-ggplot(pressure_data, aes(x=outcome2, y=pressureOrdered, col = count, fill = count, label = count)) +
  geom_tile(color="grey") +
  #geom_text(col = "black") +
  geom_text(data=subset(pressure_data,count != 0),col="black")+
  theme_minimal() +
  scale_fill_gradient2(low = "white", high = "red") +
  scale_color_gradient2(low = "white", high = "red")+
  scale_x_discrete(position = "top")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
        axis.title.y = element_text(face = "bold", vjust = 3))+
  ylab("Pressure")+
  xlab(NULL)

pmap2

#separating new plot format (pmap2) into the outcome subgroup facets

pmap2 + facet_grid(~ outcome1)

# removing blank columns from outcome subgroup facets (scales = "free_x") and
# adjusting size of each pressure count box so all are the same size (space = "free")

pmap2 + facet_grid(~ outcome1, scales = "free_x", space = "free")

# adjusting subcategory labels

pressure_outcome1 <- as_labeller(c(
  Survival = "Survival",
  Reproduction = "Reprod.",
  Productivity = "Prod.",
  Performance = "Performance",
  Migration = "Migration",
  GrowthLifeHistory = "Growth Life History",
  Distribution = "Distr."
), default = label_wrap_gen(width = 15))

# plotting pressure map with adjusted labels

pmap3 <- pmap2 +
  facet_grid(~ outcome1,
             scales = "free_x",
             space = "free",
             labeller = pressure_outcome1)

pmap3

# removing grid lines between outcome subgroup facets and moving facet labels above outcome labels

pmap3 +
  theme(panel.grid = element_blank(), strip.placement = "outside",
        panel.spacing.x = unit(0, "line"),
        strip.text.x = element_text(face = "bold"))


# adding lines between facet labels

pmap4<-pmap3 +
  theme(panel.grid = element_blank(), strip.placement = "outside",
        panel.spacing.x = unit(0, "line"),
        strip.text.x = element_text(face = "bold"))


gline = linesGrob(y = c(9, 6.68),  gp = gpar(col = "black", lwd = 1))

pmap5<-pmap4 + annotation_custom(gline, xmin=.4, xmax=.4, ymin=.5, ymax=2.9)

pmap5

g <- ggplotGrob(pmap5)



#g$layout$clip[g$layout$name=="panel-1-1"] <- "off"

for (k in grep("panel",g$layout$name)) {
  g$layout$clip[g$layout$name==g$layout$name[k]] <- "off"
}
grid.draw(g)

#ggsave("final_map.png", dpi = 600, width = 16, height = 10, units = "in")

# png("final_pressure_map.png",width = 16, height = 10, units = "in", res = 600)




#=====================================================
# 1. search results and screening table
#=====================================================


citation_table<-read_excel("foragefish_citation_table.xlsx",sheet="Sheet1")


#=====================================================
# 2. Number (count) of cases per decade by report type
#=====================================================

# colour blind palette

cb1Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7","#999999")


# colour blind palette for report types

report_colour <- c("book chapter"="#000000", "conference"= "#E69F00",
                "journal paper"= "#56B4E9", "organization report"= "#009E73",
                "other" = "#F0E442", "thesis" = "#0072B2")

# adding decade column

ff_table_decade <- ff_table %>% mutate(., Decade = `Year published`
                              , .after = 'Year published')

ff_table_decade$Decade <- admisc::recode(ff_table_decade$Decade,
                                 "1900:1909 = 1900-1909;
       1910:1919 = 1910-1919;
       1920:1929 = 1920-1929;
       1930:1939 = 1930-1939;
       1940:1949 = 1940-1949;
       1950:1959 = 1950-1959;
       1960:1969 = 1960-1969;
       1970:1979 = 1970-1979;
       1980:1989 = 1980-1989;
       1990:1999 = 1990-1999;
       2000:2009 = 2000-2009;
       2010:2019 = 2010-2019;
       2020:2021 = 2020-2021;
       else=NA")

# all species

StudiesByDecade_Type <- ff_table_decade %>%
  group_by(`Decade`,`Report type`) %>%
  count()

StudiesByDecade_Type<- na.omit(StudiesByDecade_Type)

colnames(StudiesByDecade_Type)<-c("Decade","ReportType","count")


study_decade_plot <- ggplot(StudiesByDecade_Type)+
  geom_bar(aes(y=count, x=Decade, fill=ReportType),stat="identity")+
  scale_fill_manual(values=report_colour) +
  #coord_flip() +
  xlab("Year of Publication")

study_decade_plot

### eulachon

eulachon_table_decade <- ff_table_decade %>%
  filter(`Focus species Common Name` %in% "Eulachon")

StudiesByDecade_Type_eulachon <- eulachon_table_decade %>%
  group_by(`Decade`,`Report type`) %>%
  count()

StudiesByDecade_Type_eulachon<- na.omit(StudiesByDecade_Type_eulachon)

colnames(StudiesByDecade_Type_eulachon)<-c("Decade","ReportType","count")


study_decade_plot_eulachon <- ggplot(StudiesByDecade_Type_eulachon)+
  geom_bar(aes(y=count, x=Decade, fill=ReportType),stat="identity")+
  scale_fill_manual(values=report_colour) +
  #coord_flip() +
  xlab("Year of Publication") +
  ggtitle("Pacific/Eulachon")

study_decade_plot_eulachon

### Pacific Herring

herring_table_decade <- ff_table_decade %>%
  filter(`Focus species Common Name` %in% "Pacific Herring")

StudiesByDecade_Type_herring <- herring_table_decade %>%
  group_by(`Decade`,`Report type`) %>%
  count()

StudiesByDecade_Type_herring<- na.omit(StudiesByDecade_Type_herring)

colnames(StudiesByDecade_Type_herring)<-c("Decade","ReportType","count")


study_decade_plot_herring <- ggplot(StudiesByDecade_Type_herring)+
  geom_bar(aes(y=count, x=Decade, fill=ReportType),stat="identity")+
  scale_fill_manual(values=report_colour) +
  #coord_flip() +
  xlab("Year of Publication") +
  ggtitle("Pacific/Pacific Herring")

study_decade_plot_herring

### Surf Smelt

surf_table_decade <- ff_table_decade %>%
  filter(`Focus species Common Name` %in% "Surf Smelt")

StudiesByDecade_Type_surf <- surf_table_decade %>%
  group_by(`Decade`,`Report type`) %>%
  count()

StudiesByDecade_Type_surf<- na.omit(StudiesByDecade_Type_surf)

colnames(StudiesByDecade_Type_surf)<-c("Decade","ReportType","count")


study_decade_plot_surf <- ggplot(StudiesByDecade_Type_surf)+
  geom_bar(aes(y=count, x=Decade, fill=ReportType),stat="identity")+
  scale_fill_manual(values=report_colour) +
  #coord_flip() +
  xlab("Year of Publication") +
  ggtitle("Pacific/Surf Smelt")

study_decade_plot_surf

### Whitebait Smelt

whitebait_table_decade <- ff_table_decade %>%
  filter(`Focus species Common Name` %in% "Whitebait Smelt")

StudiesByDecade_Type_whitebait <- whitebait_table_decade %>%
  group_by(`Decade`,`Report type`) %>%
  count()

StudiesByDecade_Type_whitebait<- na.omit(StudiesByDecade_Type_whitebait)

colnames(StudiesByDecade_Type_whitebait)<-c("Decade","ReportType","count")


study_decade_plot_whitebait <- ggplot(StudiesByDecade_Type_whitebait)+
  geom_bar(aes(y=count, x=Decade, fill=ReportType),stat="identity")+
  scale_fill_manual(values=report_colour) +
  #coord_flip() +
  xlab("Year of Publication") +
  ggtitle("Pacific/Whitebait Smelt")

study_decade_plot_whitebait

# Combining plots

library(patchwork)

(study_decade_plot_eulachon + study_decade_plot_herring)/
  (study_decade_plot_surf + study_decade_plot_whitebait)

#========================================================
# 3.	Number (count) of cases per study life stage type
#========================================================

# get unique list of life history stages

lifestage <- na.omit(c(unique(unlist(dropdowns[,12]))))

# get unique list of species

ff_species <- na.omit(c(unique(unlist(ff_table$`Focus species Common Name`))))

# put species into columns with life history stages as values for the rows in those columns

LHS_data <- pivot_wider(ff_table,id_cols=NULL,names_from = "Focus species Common Name",
                   values_from="Life stage studied (Adult, Juvenile, Larval, Egg)")

# subset the data to include only the species columns

LHS_data <- LHS_data[,c(which(colnames(LHS_data)%in%ff_species))]

# subset the outcome column and counting how many times each of the lifestages occurs in the column.
# use the function lifestage_table

lifestage_data <- lifestage_table(lifestage,LHS_data)

# rename columns

colnames(lifestage_data)<-c("Lifestage","Fish", "Count")

#change the order in which the life history stages are plotted and listed in the legend

lifestage_data$LifestageOrdered = factor(lifestage_data$Lifestage, levels=c("Adult","Juvenile","Larval","Egg","not specified"))

# colour blind palette

cb2Palette <- c("#0072B2","#56B4E9","#009E73","#E69F00", "#999999")

# stacked barplot

ggplot(lifestage_data)+
  geom_bar(aes(x=Fish, y=Count, fill=LifestageOrdered), stat="identity")+
  scale_colour_discrete(na.translate = F) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values=cb2Palette)

#==========================================
# 5.	Case counts by other factors
#==========================================

yes_no_table <- ff_table %>%
  select(#Ocean,
         'Focus species Common Name',
         'Temperature information recorded',
         'Depth distribution information was recorded',
         'Role in ecosystem: Predator species identified',
         'Role in ecosystem: Prey species identified')


yes_no_table$`Temperature information recorded`<-factor(yes_no_table$`Temperature information recorded`,
                                                        levels =c("yes", "no"), ordered = TRUE)
yes_no_table$`Depth distribution information was recorded`<-factor(yes_no_table$`Depth distribution information was recorded`,
                                                                   levels =c("yes", "no"), ordered = TRUE)
yes_no_table$`Role in ecosystem: Predator species identified`<-factor(yes_no_table$`Role in ecosystem: Predator species identified`,
                                                                      levels =c("yes", "no"), ordered = TRUE)
yes_no_table$`Role in ecosystem: Prey species identified`<-factor(yes_no_table$`Role in ecosystem: Prey species identified`,
                                                                  levels =c("yes", "no"), ordered = TRUE)
#### Temp

temp_count <- yes_no_table %>%
  filter(., !is.na(`Temperature information recorded`))%>%
  group_by(`Focus species Common Name`,`Temperature information recorded`) %>%
  summarise(., Temp = n())


temp_wide <- pivot_wider(temp_count, names_from = `Temperature information recorded`, values_from = Temp)


temp_wide$Temp <- paste(temp_wide$yes, "(", temp_wide$no, ")", sep = "")

temp_wide <- temp_wide[,-(2:3)]

### Depth

depth_count <- yes_no_table %>%
  filter(., !is.na(`Depth distribution information was recorded`))%>%
  group_by(`Focus species Common Name`,`Depth distribution information was recorded`) %>%
  summarise(., Depth = n())


depth_count <- yes_no_table %>%
  filter(., !is.na(`Depth distribution information was recorded`))%>%
  group_by(`Focus species Common Name`,`Depth distribution information was recorded`) %>%
  summarise(., Depth = n())


depth_wide <- pivot_wider(depth_count, names_from = `Depth distribution information was recorded`, values_from = Depth)


depth_wide$Depth <- paste(depth_wide$yes, "(", depth_wide$no, ")", sep = "")

depth_wide <- depth_wide[,-(2:3)]

### predator

predator_count <- yes_no_table %>%
  filter(., !is.na(`Role in ecosystem: Predator species identified`))%>%
  group_by(`Focus species Common Name`,`Role in ecosystem: Predator species identified`) %>%
  summarise(., Predator = n())

predator_count <- yes_no_table %>%
  filter(., !is.na(`Role in ecosystem: Predator species identified`))%>%
  group_by(`Focus species Common Name`,`Role in ecosystem: Predator species identified`) %>%
  summarise(., Predator = n())


predator_wide <- pivot_wider(predator_count, names_from = `Role in ecosystem: Predator species identified`, values_from = Predator)


predator_wide$Predator <- paste(predator_wide$yes, "(", predator_wide$no, ")", sep = "")

predator_wide <- predator_wide[,-(2:3)]

### Prey

prey_count <- yes_no_table %>%
  filter(., !is.na(`Role in ecosystem: Prey species identified`))%>%
  group_by(`Focus species Common Name`,`Role in ecosystem: Prey species identified`) %>%
  summarise(., Prey = n())


prey_count <- yes_no_table %>%
  filter(., !is.na(`Role in ecosystem: Prey species identified`))%>%
  group_by(`Focus species Common Name`,`Role in ecosystem: Prey species identified`) %>%
  summarise(., Prey = n())


prey_wide <- pivot_wider(prey_count, names_from = `Role in ecosystem: Prey species identified`, values_from = Prey)


prey_wide$Prey <- paste(prey_wide$yes, "(", prey_wide$no, ")", sep = "")

prey_wide <- prey_wide[,-(2:3)]

### combine

yes_no_count1 <- merge(temp_wide, depth_wide, by= "Focus species Common Name")

yes_no_count2 <- merge(yes_no_count1, predator_wide, by= "Focus species Common Name")

yes_no_counts <- merge(yes_no_count2, prey_wide, by= "Focus species Common Name")

yes_no_counts
#==================================================
# 6.a)	Case count by gear type
#==================================================

# get unique list of gear types

gear_types <- na.omit(c(unique(unlist(dropdowns[,17]))))

# get unique list of species-same as above

ff_species <- na.omit(c(unique(unlist(ff_table$`Focus species Common Name`))))

# put species into columns with gear type as values for the rows in those columns

gear_data <- pivot_wider(ff_table,id_cols=NULL,names_from = "Focus species Common Name",
                        values_from="Sampling gear used")

# subset the data to include only the species columns

gear_data <- gear_data[,c(which(colnames(gear_data)%in%ff_species))]

# subset the outcome column and counting how many times each of the gears occur in the column.
# use the function lifestage_table

gear_count_data <- lifestage_table(gear_types,gear_data)

# rename columns

colnames(gear_count_data)<-c("GearType","Fish", "Count")

# stacked barplot

mycolour1 <- c( "#000000","#80FFFF","#FF0000" , "#008000", "#808000", "#FF8000",
                "#000080" , "#80FF00", "#FFFF00",'#dcbeff', "#800080", "#FF0080",
                "#008080", "#808080", "#FF8080",'#9A6324',"#80FF80", "#FFFF80",
                "#0000FF", "#8000FF", "#FF00FF", "#0080FF","#8080FF", "#FF80FF",
                "#00FFFF", "#800000")

counts_a <- ggplot(gear_count_data)+
  geom_bar(aes(x=Fish, y=Count, fill=GearType), stat="identity")+
  scale_colour_discrete(na.translate = F) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_manual(values=mycolour1)

#==================================================
# 6.b)	Case count by type of stock structure info
#==================================================

# get unique list of stock structure info

stock_structure <- na.omit(c(unique(unlist(dropdowns[,19]))))

# put species into columns with stock structure info as values for the rows in those columns

stock_data <- pivot_wider(ff_table,id_cols=NULL,names_from = "Focus species Common Name",
                         values_from="StockStructure")

# subset the data to include only the species columns

stock_data <- stock_data[,c(which(colnames(stock_data)%in%ff_species))]

# subset the outcome column and counting how many times each of the stock structure types occur in the column.
# use the function lifestage_table

stock_count_data <- lifestage_table(stock_structure,stock_data)

# rename columns

colnames(stock_count_data)<-c("StockStructure","Fish", "Count")

# stacked barplot

counts_b <- ggplot(stock_count_data)+
  geom_bar(aes(x=Fish, y=Count, fill=StockStructure), stat="identity")+
  scale_colour_discrete(na.translate = F) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_manual(values=mycolour1)

#==================================================
# 6.c)	Case count by distribution data type
#==================================================

# get unique list of distribution data types

distribution_types <- na.omit(c(unique(unlist(dropdowns[,20]))))

# put species into columns with gear type as values for the rows in those columns

distribution_data <- pivot_wider(ff_table,id_cols=NULL,names_from = "Focus species Common Name",
                         values_from="Distribution_DataType")

# subset the data to include only the species columns

distribution_data <- distribution_data[,c(which(colnames(distribution_data)%in%ff_species))]

# subset the outcome column and counting how many times each of the distribution data types occur in the column.
# use the function lifestage_table

distribution_count_data <- lifestage_table(distribution_types,distribution_data)

# rename columns

colnames(distribution_count_data)<-c("Distribution_DataType","Fish", "Count")

# stacked barplot

counts_c <- ggplot(distribution_count_data)+
  geom_bar(aes(x=Fish, y=Count, fill=Distribution_DataType), stat="identity")+
  scale_colour_discrete(na.translate = F) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values=mycolour1)

#==================================================
# 6.	Case count graphs combined
#==================================================

counts_a/counts_b/counts_c








