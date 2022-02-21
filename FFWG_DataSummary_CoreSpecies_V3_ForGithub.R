##        DFO National Forage Fish Working Group 
##        Literature mapping data summary
##        Bringing together literature mapping data from all DFO Regions to summarize
##        Based on some code for the Pacific Region developed by Brooke Hackett,
##        Jennifer Boldt edited and developed code to finalize data and make figures for all Regions
##        And for core species only (i.e., those species for which directed searches were completed)
##        Data are summarized by ocean; which means that when summarizing by ocean, the data had to be 'expanded' -i.e., if an article had more than one ocean identified, it was repeated for different oceans
##        Feb. 2022

#============================================
##  Files needed:
#1. Data_ExpandedByOcean_corespecies_nodupl_1970_2020_Feb19,2022.xlsx
#2. CoreSpecies.xlsx
#3. DatabaseDropDownOptions.xlsx

#============================================
#SET UP#
#============================================
#Pathname
Pathname<-"C:/pathname/"

##  set working directory
setwd(Pathname)

#create folders for output
subfolder_names<-c(
                   "PressureMatrixFigures_ByOcean",
                   "PressureMatrixFigures_ByOcean_SpeciesCombined",
                   "ReportCount_ByLifeHistoryStage",
                   "ReportTypeByYear_ByOcean",
                   "ReportTypeByYear_ByOceanSpeciesCombined",
                   "TempDepthPredPrey_Counts",
                   "GearStockDistr_Counts"
                   )
for (j in 1:length(subfolder_names)){
  folder<-dir.create(paste0(Pathname,subfolder_names[j]))
}

##  load libraries
# package installation and data import 
#library("devtools")
#install_github("https://github.com/rooperc4/ForageFishLitReview")
library(ForageFishLitReview)
library(readxl)
library(ggplot2)
library(splitstackshape)
library(rio)
library(plyr)
library(dplyr)
library(grid)
library(readxl)
library(tidyr)
library(patchwork)
library(RColorBrewer)

#============================================
#DATA#
#============================================

#to summarize only core species 
speciesRegionOcean<-data.frame(read_excel("CoreSpecies.xlsx",sheet="CoreSpecies"))
speciesOcean<-speciesRegionOcean[,c(1,3)]  

#To expand data by ocean:
#the expand_ocean function repeats papers that have multiple oceans listed (for each ocean); for example if a paper lists the Arctic and the Pacific for one species, the row is replaced by two rows - one for the Pacific and one for the Atlantic
#This code has already been used on a data file "ALLdata", to create the data in the file "Data_ExpandedByOcean_corespecies_nodupl_1970_2020_Feb19,2022.csv"
#data2<-expand_ocean(ALLdata,"Ocean")

#Read in the data:
data2<-read_excel("Data_ExpandedByOcean_corespecies_nodupl_1970_2020_Feb19,2022.xlsx", sheet="Data")

#============================================
#PLOTS#
#============================================

#============================================
##    A. PRESSURE MATRIX PLOTS
#============================================

# extract list of unique pressures and outcomes from dataset 
# this is the same for all species 

##  import dropdown menus from all regions
dropdowns<-read_excel("DatabaseDropDownOptions.xlsx",sheet="Do Not Edit dropdown options")
pressures <- na.omit(c(unique(unlist(dropdowns[,22]))))
outcomes <- colnames(data2)[22:42]

###A.  Productivity Matrix plots and R data objects: 
#1.for each 'Focus species Common Name' in each 'Ocean'; data2, data_table2, data3
#2.all 'Focus species Common Name' combined for each 'Ocean'; data6, data_table4, data7 

##########################################
##########
##     1. pressure response matrix BY species and OCEAN; 
##        this means that if a paper had info for both the Arctic and Pacific, it was counted towards both the Arctic and the Pacific (i.e., repeated for each ocean)
########## core species    
##########################################

for(i in 1:dim(speciesOcean)[1]){
  
  data_table2 <- data2 %>%
    filter(paste0(`Focus species Common Name`,`Ocean`) %in% paste0(speciesOcean[i,1],speciesOcean[i,2]))
  data3 <- data_table2[,which(colnames(data_table2)%in%outcomes)]
  
  # Prepare the pressure table 
  ## prep data using the ForageFishLitReview function "pressure_table" by subsetting 
  ## the outcomes columns and counting how many times each of the pressures occurs in each column

  pressure_data <- pressure_table(pressures,data3)
  ##split outcome data into sub category 
  pressure_data$outcomeforsplit<-pressure_data$outcome
  pressure_data<-cSplit(pressure_data, "outcomeforsplit", sep="_", type.convert=FALSE)
  colnames(pressure_data)<-c("pressure","outcome","count","outcome1","outcome2")
  ##change the order in which the pressures are plotted 
  pressure_data$pressureOrdered = factor(pressure_data$pressure, levels=rev(c(unique(pressures))))
  write.csv(pressure_data,paste0("./PressureMatrixFigures_ByOcean/pressure_data",speciesOcean[i,2]," ",speciesOcean[i,1],".csv"))

    # Plot the pressure map
  ## plotting the pressure_data
  pressure_map <- ggplot(pressure_data, aes(x=outcome2, y=pressureOrdered, col = count, fill = count, label = count)) +
    geom_tile(color="grey") +
    geom_text(data=subset(pressure_data,count != 0),col="black", size=3)+
    theme_minimal() +
    scale_fill_gradient2(low = "white", high = "red") +
    scale_color_gradient2(low = "white", high = "red")+
    scale_x_discrete(position = "top")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
          axis.title.y = element_text(face = "bold", vjust = 3))+
    ylab("Pressure")+
    xlab(NULL)
  ## adjusting subcategory labels 
  pressure_outcome1 <- as_labeller(c(
    Survival = "Survival",
    Reproduction = "Reprod.",
    Productivity = "Prod.",
    Performance = "Performance",
    Migration = "Migration",
    GrowthLifeHistory = "Growth Life History",
    Distribution = "Distr."
  ), default = label_wrap_gen(width = 15))
  ## separating plot into the outcome subgroup facets with adjusted labels 
  pressure_map1 <- pressure_map + 
    ggtitle(paste0(speciesOcean[i,2],", ",speciesOcean[i,1]))+
    facet_grid(~ outcome1, 
               scales = "free_x", 
               space = "free",
               labeller = pressure_outcome1) +
    theme(panel.grid = element_blank(), 
          strip.placement = "outside", 
          panel.spacing.x = unit(0, "line"), 
          strip.text.x = element_text(face = "bold", hjust=.3, angle=45, size=7),
          #plot.title = element_text(hjust = -0.95, vjust=-20)) this doesn't left-justify text, so get overlap with figure
          plot.title.position = "plot")
  pressure_map1
  # adding lines between facet labels 
  gline = linesGrob(y = c(12, 6.68),  gp = gpar(col = "grey", lwd = 1)) 
  pressure_map2 <- pressure_map1 + annotation_custom(gline, xmin=.4, xmax=.4, ymin=.5, ymax=2.9)
  g <- ggplotGrob(pressure_map2)
  g$layout$clip[g$layout$name=="panel-1-1"] <- "off"
  for (k in grep("panel",g$layout$name)) {
    g$layout$clip[g$layout$name==g$layout$name[k]] <- "off" 
  }
  
  png(paste0("./PressureMatrixFigures_ByOcean/",speciesOcean[i,2],speciesOcean[i,1],"pressurematrix.png"),res=300, width=7,height=5, units="in")
  grid.draw(g)
  dev.off()
  
}
#####

  ##########################################
  ##########################################
  ##########
  ##     2. pressure response matrix BY OCEAN all species combined; 
  ##        this means that if a paper had info for both the Arctic and Pacific, it was counted towards both the Arctic and the Pacific (i.e., repeated for each ocean)
  ########## core species   
  ##########################################
  #the expand_ocean function repeats papers that have multiple oceans listed (for each ocean); for example if a paper lists the Arctic and the Pacific for one species, the row is replaced by two rows - one for the Pacific and one for the Atlantic
  #data6<-expand_ocean(data,"Ocean")
  ocean_df<-data.frame(c("Pacific","Atlantic","Arctic"))
  colnames(ocean_df)<-"Ocean"
  data6a<-data2 %>%
    filter(paste0(`Focus species Common Name`,`Ocean`) %in% paste0(speciesOcean[,1],speciesOcean[,2]))
  
  for(i in 1:dim(ocean_df)[1]){
    data_table4 <- data6a %>%
      filter(`Ocean` %in% ocean_df[i,1])
    data7 <- data_table4[,which(colnames(data_table4)%in%outcomes)]

    # Prepare the pressure table 
    #```{r, ptable}
    ## prep data using the ForageFishLitReview function "pressure_table" by subsetting 
    ## the outcomes columns and counting how many times each of the pressures occurs in each column
    
    #  !!!!
    ###FYI this will overwrite previous objects###
    #  !!!!
    
    pressure_data <- pressure_table(pressures,data7)
    ##split outcome data into sub category 
    pressure_data$outcomeforsplit<-pressure_data$outcome
    pressure_data<-cSplit(pressure_data, "outcomeforsplit", sep="_", type.convert=FALSE)
    colnames(pressure_data)<-c("pressure","outcome","count","outcome1","outcome2")
    ##change the order in which the pressures are plotted 
    pressure_data$pressureOrdered = factor(pressure_data$pressure, levels=rev(c(unique(pressures))))
    write.csv(pressure_data,paste0("./PressureMatrixFigures_ByOcean_SpeciesCombined/pressure_data",ocean_df[i,1],".csv"))


    # Plot the pressure map
    ## plotting the pressure_data
    pressure_map <- ggplot(pressure_data, aes(x=outcome2, y=pressureOrdered, col = count, fill = count, label = count)) +
      geom_tile(color="grey") +
      geom_text(data=subset(pressure_data,count != 0),col="black", size=3)+
      theme_minimal() +
      scale_fill_gradient2(low = "white", high = "red") +
      scale_color_gradient2(low = "white", high = "red")+
      scale_x_discrete(position = "top")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
            axis.title.y = element_text(face = "bold", vjust = 3))+
      ylab("Pressure")+
      xlab(NULL)
    ## adjusting subcategory labels 
    pressure_outcome1 <- as_labeller(c(
      Survival = "Survival",
      Reproduction = "Reprod.",
      Productivity = "Prod.",
      Performance = "Performance",
      Migration = "Migration",
      GrowthLifeHistory = "Growth Life History",
      Distribution = "Distr."
    ), default = label_wrap_gen(width = 15))
    ## separating plot into the outcome subgroup facets with adjusted labels 
    pressure_map1 <- pressure_map + 
      ggtitle(paste0(ocean_df[i,1]))+
      facet_grid(~ outcome1, 
                 scales = "free_x", 
                 space = "free",
                 labeller = pressure_outcome1) +
      theme(panel.grid = element_blank(), 
            strip.placement = "outside", 
            panel.spacing.x = unit(0, "line"), 
            strip.text.x = element_text(face = "bold", hjust=.3, angle=45, size=7),
            #plot.title = element_text(hjust = -0.95, vjust=-20)) this doesn't left-justify text, so get overlap with figure
            plot.title.position = "plot")
    pressure_map1
    # adding lines between facet labels 
    gline = linesGrob(y = c(12, 6.68),  gp = gpar(col = "grey", lwd = 1)) 
    pressure_map2 <- pressure_map1 + annotation_custom(gline, xmin=.4, xmax=.4, ymin=.5, ymax=2.9)
    g <- ggplotGrob(pressure_map2)
    g$layout$clip[g$layout$name=="panel-1-1"] <- "off"
    for (k in grep("panel",g$layout$name)) {
      g$layout$clip[g$layout$name==g$layout$name[k]] <- "off" 
    }
    
    png(paste0("./PressureMatrixFigures_ByOcean_SpeciesCombined/",ocean_df[i,1],"pressurematrix.png"),res=300, width=7,height=5, units="in")
    grid.draw(g)
    dev.off()
    
  }
  
  
  ###########################################################
  ##      END OF PRESSURE MATRIX PLOTS
  ##      THE FOLLOWING CODE IS FOR OTHER FIGURES
  
  ##########################################

 
  #=====================================================
  # B. Number (count) of cases per decade or year by report type
  #=====================================================
  
  # colour blind palette
  
  cb1Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                  "#0072B2", "#D55E00", "#CC79A7","#999999")
  
  
  # colour blind palette for report types
  
  report_colour <- c("book chapter"="#000000", "conference"= "#E69F00",
                     "journal paper"= "#56B4E9", "organization report"= "#009E73",
                     "other" = "#F0E442", "thesis" = "#0072B2")
  
 
  ######################################  
  #1. report type count by year and by Ocean and species
  ###################################### 
  ##  This depends on data2, ocean, and speciesOceanspeciesRegion, which were created for pressure matrix plots

    for(i in 1:dim(speciesOcean)[1]){
      data_table6 <- data2 %>%
        filter(paste0(`Focus species Common Name`,`Ocean`) %in% paste0(speciesOcean[i,1],speciesOcean[i,2]))
    
    data9 <- data_table6 %>%
      dplyr::group_by(`Year published`,`Report type`) %>%
      dplyr::count()
    
    data9<- na.omit(data9)
    
    colnames(data9)<-c("Year","ReportType","Count")
    
    study_year_ocean_plot <- ggplot(data9)+
      ggtitle(paste0(speciesOcean[i,2],", ",speciesOcean[i,1]))+
      geom_bar(aes(y=Count, x=Year, fill=ReportType),stat="identity")+
      scale_fill_manual(values=report_colour) +
      #coord_flip() +
      xlab("Year of Publication")+
      theme(axis.text.y=element_text(size=14),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            axis.title.x=element_text(size=14),
            axis.text.x=element_text(size=14, angle =90, vjust=0.5)
      )
    
    png(paste0("./ReportTypeByYear_ByOcean/",speciesOcean[i,2],speciesOcean[i,1],"ReportTypeByYearOceanSpecies.png"),res=600, width=7,height=5, units="in")
    print(study_year_ocean_plot)
    dev.off()
  }
  
      ######################################  
    #2. report type count by year and by Ocean 
    ###################################### 
    ##  This depends on data2, ocean, and speciesOceanspeciesRegion, data6, ocean_df, which were created for pressure matrix plots
    ##  This also depends on data2_decade, created in another figure here
    data_table8<-data2 %>%
      filter(paste0(`Focus species Common Name`,`Ocean`) %in% paste0(speciesOcean[,1],speciesOcean[,2]))
    for(i in 1:dim(ocean_df)[1]){
      data_table8a <- data_table8 %>%
        filter(`Ocean` %in% ocean_df[i,1])
      
      data11 <- data_table8a %>%
        dplyr::group_by(`Year published`,`Report type`) %>%
        dplyr::count()
      
      data11<- na.omit(data11)
      
      colnames(data11)<-c("Year","ReportType","Count")
      
      study_year_ocean_plot <- ggplot(data11)+
        ggtitle(paste0(ocean_df[i,1]))+
        geom_bar(aes(y=Count, x=Year, fill=ReportType),stat="identity")+
        scale_fill_manual(values=report_colour) +
        scale_y_continuous(limits = c(0,75), breaks = seq(0, 75, 25))+
        #coord_flip() +
        xlab("Year of Publication")+
        theme(axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              axis.title.x=element_text(size=14),
              axis.text.x=element_text(size=14, angle =90, vjust=0.5)
        )
         
      png(paste0("./ReportTypeByYear_ByOceanSpeciesCombined/",ocean_df[i,1],"ReportTypeByYearOcean.png"),res=600, width=7,height=5, units="in")
      print(study_year_ocean_plot)
      dev.off()
    }
    
    
    ###########################################################
    ##      END OF REPORT TYPE PLOTS
    ##########################################
    
    
    #=====================================================
    # C. Number (count) of cases per life history stage 
    #=====================================================
 
   ##1. By Ocean

   # get unique list of life history stages
   lifestage <- na.omit(c(unique(unlist(dropdowns$"Life stage studied (Adult, Juvenile, Larval, Egg)"))))
   
   #This depends on objects created for pressure matrix figures 

   #subset by Oceans
   data2_PacificOcean<-subset(data2,data2$Ocean=="Pacific")
   data2_ArcticOcean<-subset(data2,data2$Ocean=="Arctic")
   data2_AtlanticOcean<-subset(data2,data2$Ocean=="Atlantic")

   # get unique list of species
   ff_species_PacificOcean <- na.omit(c(unique(unlist(data2_PacificOcean$`Focus species Common Name`))))
   ff_species_ArcticOcean <- na.omit(c(unique(unlist(data2_ArcticOcean$`Focus species Common Name`))))
   ff_species_AtlanticOcean <- na.omit(c(unique(unlist(data2_AtlanticOcean$`Focus species Common Name`))))

   # put species into columns with life history stages as values for the rows in those columns
   LHS_data_PacificOcean <- pivot_wider(data2_PacificOcean,id_cols=NULL,names_from = "Focus species Common Name",
                               values_from="Life stage studied (Adult, Juvenile, Larval, Egg)")
   LHS_data_ArcticOcean <- pivot_wider(data2_ArcticOcean,id_cols=NULL,names_from = "Focus species Common Name",
                               values_from="Life stage studied (Adult, Juvenile, Larval, Egg)")
   LHS_data_AtlanticOcean <- pivot_wider(data2_AtlanticOcean,id_cols=NULL,names_from = "Focus species Common Name",
                                 values_from="Life stage studied (Adult, Juvenile, Larval, Egg)")

   # subset the data to include only the species columns
   LHS_data_sub_PacificOcean <- LHS_data_PacificOcean[,c(which(colnames(LHS_data_PacificOcean)%in%ff_species_PacificOcean))]
   LHS_data_sub_ArcticOcean <- LHS_data_ArcticOcean[,c(which(colnames(LHS_data_ArcticOcean)%in%ff_species_ArcticOcean))]
   LHS_data_sub_AtlanticOcean <- LHS_data_AtlanticOcean[,c(which(colnames(LHS_data_AtlanticOcean)%in%ff_species_AtlanticOcean))]

   # subset the outcome column and counting how many times each of the lifestages occurs in the column.
   # use the function lifestage_table
   lifestage_data_PacificOcean <- lifestage_table(lifestage,LHS_data_sub_PacificOcean)
   lifestage_data_ArcticOcean <- lifestage_table(lifestage,LHS_data_sub_ArcticOcean)
   lifestage_data_AtlanticOcean <- lifestage_table(lifestage,LHS_data_sub_AtlanticOcean)

   # rename columns
   colnames(lifestage_data_PacificOcean)<-c("Lifestage","Fish", "Count")
   colnames(lifestage_data_ArcticOcean)<-c("Lifestage","Fish", "Count")
   colnames(lifestage_data_AtlanticOcean)<-c("Lifestage","Fish", "Count")

   #change the order in which the life history stages are plotted and listed in the legend
   lifestage_data_PacificOcean$LifestageOrdered = factor(lifestage_data_PacificOcean$Lifestage, levels=c("Adult","Juvenile","Larval","Egg","not specified"))
   lifestage_data_ArcticOcean$LifestageOrdered = factor(lifestage_data_ArcticOcean$Lifestage, levels=c("Adult","Juvenile","Larval","Egg","not specified"))
   lifestage_data_AtlanticOcean$LifestageOrdered = factor(lifestage_data_AtlanticOcean$Lifestage, levels=c("Adult","Juvenile","Larval","Egg","not specified"))

   #add Ocean column
   lifestage_data_PacificOcean$Ocean = "PacificOcean"
   lifestage_data_ArcticOcean$Ocean = "ArcticOcean"
   lifestage_data_AtlanticOcean$Ocean = "AtlanticOcean"

   #put all ocean counts together
   lifestage_data_allOceans<-rbind(lifestage_data_PacificOcean,lifestage_data_ArcticOcean,lifestage_data_AtlanticOcean)
   lifestage_data_allOceans$Fish<- gsub('\\.', ' ', lifestage_data_allOceans$Fish)
   
   write.csv(lifestage_data_allOceans,file=paste("./ReportCount_ByLifeHistoryStage/lifestage_data_allOceans",".csv",sep=''))
   # colour blind palette
   cb2Palette <- c("#0072B2","#56B4E9","#009E73","#E69F00", "#999999")
   lifestage_data_allOceans$Fish<-as.factor(lifestage_data_allOceans$Fish)
   # stacked barplots - facet wrap of Oceans
   ReportCount_ByLifeHistoryStage_allOceans<-ggplot(lifestage_data_allOceans)+
     geom_bar(aes(x=Fish, y=Count, fill=LifestageOrdered), stat="identity")+
     scale_colour_discrete(na.translate = F) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     scale_fill_manual(values=cb2Palette)+
     facet_wrap(vars(lifestage_data_allOceans$Ocean),nrow=1, ncol=3, scales="free", labeller="label_value")
   
   png(paste0("./ReportCount_ByLifeHistoryStage/ReportCount_ByLifeHistoryStage_AllOceans.png"),res=600, width=7,height=5, units="in")
   print(ReportCount_ByLifeHistoryStage_allOceans)
   dev.off() 
   
   
   # stacked barplots for individual regions
   ReportCount_ByLifeHistoryStage_PacificOcean<-ggplot(lifestage_data_PacificOcean)+
     geom_bar(aes(x=Fish, y=Count, fill=LifestageOrdered), stat="identity")+
     scale_colour_discrete(na.translate = F) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     scale_fill_manual(values=cb2Palette)+
     ggtitle(paste0("Pacific"))
   png(paste0("./ReportCount_ByLifeHistoryStage/","ReportCount_ByLifeHistoryStage_PacificOcean.png"),res=600, width=7,height=5, units="in")
   print(ReportCount_ByLifeHistoryStage_PacificOcean)
   dev.off()
   
   ReportCount_ByLifeHistoryStage_ArcticOcean<-ggplot(lifestage_data_ArcticOcean)+
     geom_bar(aes(x=Fish, y=Count, fill=LifestageOrdered), stat="identity")+
     scale_colour_discrete(na.translate = F) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     scale_fill_manual(values=cb2Palette)+
     ggtitle(paste0("Arctic"))
   png(paste0("./ReportCount_ByLifeHistoryStage/","ReportCount_ByLifeHistoryStage_ArcticOcean.png"),res=600, width=7,height=5, units="in")
   print(ReportCount_ByLifeHistoryStage_ArcticOcean)
   dev.off()
   
   ReportCount_ByLifeHistoryStage_AtlanticOcean<-ggplot(lifestage_data_AtlanticOcean)+
     geom_bar(aes(x=Fish, y=Count, fill=LifestageOrdered), stat="identity")+
     scale_colour_discrete(na.translate = F) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     scale_fill_manual(values=cb2Palette)+
     ggtitle(paste0("NLMar"))
   png(paste0("./ReportCount_ByLifeHistoryStage/","ReportCount_ByLifeHistoryStage_AtlanticOcean.png"),res=600, width=7,height=5, units="in")
   print(ReportCount_ByLifeHistoryStage_AtlanticOcean)
   dev.off()
   
  
   #================================================
   # END OF LIFE HISTORY STAGE PLOTS
   #=================================================
   
   
   #==========================================
   # D.	Case counts by other factors
   #==========================================

# 1. By Ocean
     yes_no_table <- data2 %>%
     select('Ocean',
            'Focus species Common Name',
            'Temperature information recorded',
            'Depth distribution information was recorded',
            'Role in ecosystem: Predator species identified',
            'Role in ecosystem: Prey species identified')
   
   #replace NAs with "NA" ; these are cases where the "yes/no" column was not completed
   yes_no_table[is.na(yes_no_table)]<-"NA"
   
   #create dummy column
   yes_no_table$freq<-1
   
   #aggregate by Ocean, Species, yes, no, NA for Temperature
   yes_no_table_Temp<-aggregate(yes_no_table$freq,by=list(yes_no_table$'Ocean',yes_no_table$'Focus species Common Name',yes_no_table$'Temperature information recorded'),FUN="sum")
   colnames(yes_no_table_Temp)<-c("Ocean","Focus species Common Name","Temperature information recorded","Count")
   
   #aggregate by Ocean, Species, yes, no, NA for Depth
   yes_no_table_Depth<-aggregate(yes_no_table$freq,by=list(yes_no_table$'Ocean',yes_no_table$'Focus species Common Name',yes_no_table$'Depth distribution information was recorded'),FUN="sum")
   colnames(yes_no_table_Depth)<-c("Ocean","Focus species Common Name","Depth distribution information was recorded","Count")
   
   #aggregate by Ocean, Species, yes, no, NA for Predator (NA here could mean that it doesn't exist or that cell was not completed during the review)
   yes_no_table_Pred<-aggregate(yes_no_table$freq,by=list(yes_no_table$'Ocean',yes_no_table$'Focus species Common Name',yes_no_table$'Role in ecosystem: Predator species identified'),FUN="sum")
   colnames(yes_no_table_Pred)<-c("Ocean","Focus species Common Name","Role in ecosystem: Predator species identified","Count")
   
   #aggregate by Ocean, Species, yes, no, NA for Prey
   yes_no_table_Prey<-aggregate(yes_no_table$freq,by=list(yes_no_table$'Ocean',yes_no_table$'Focus species Common Name',yes_no_table$'Role in ecosystem: Prey species identified'),FUN="sum")
   colnames(yes_no_table_Prey)<-c("Ocean","Focus species Common Name","Role in ecosystem: Prey species identified","Count")
   
   #make a wide table
   temp_wide <- pivot_wider(yes_no_table_Temp, names_from = 'Temperature information recorded', values_from = 'Count')
   
   #replace NAs with 0, because these are no longer NAs, they are true zeros
   temp_wide[is.na(temp_wide)] <- 0
   
   #prior to altering the wide table, create a long table for plotting
   temp_long<-pivot_longer(temp_wide,cols=3:5,values_to="Count", names_to="Response")
   temp_long$Var<-"Temp"
   colnames(temp_long)<-c("Ocean","Fish","Response","Count","Var")
   
   #continue with altering wide table; create a column that concatenates the yes, no, NA columns
   temp_wide$Temp <- paste(temp_wide$yes, ", ", temp_wide$no, ", ", temp_wide$'NA', sep = "")
   
   #just keep the columns DFO_Region, Species, and Temp
   temp_wide <- temp_wide[,-(3:5)]
   
   #repeat the above for depth, predator, prey
   depth_wide <- pivot_wider(yes_no_table_Depth, names_from = 'Depth distribution information was recorded', values_from = 'Count')
   depth_wide[is.na(depth_wide)] <- 0
   depth_long<-pivot_longer(depth_wide,cols=3:5,values_to="Count", names_to="Response")
   depth_long$Var<-"Depth"
   colnames(depth_long)<-c("Ocean","Fish","Response","Count","Var")
   depth_wide$Depth <- paste(depth_wide$yes, ", ", depth_wide$no, ", ", depth_wide$'NA', sep = "")
   depth_wide <- depth_wide[,-(3:5)]
   
   pred_wide <- pivot_wider(yes_no_table_Pred, names_from = 'Role in ecosystem: Predator species identified', values_from = 'Count')
   pred_wide[is.na(pred_wide)] <- 0
   pred_long<-pivot_longer(pred_wide,cols=3:5,values_to="Count", names_to="Response")
   pred_long$Var<-"Pred"
   colnames(pred_long)<-c("Ocean","Fish","Response","Count","Var")
   pred_wide$Pred <- paste(pred_wide$yes, ", ", pred_wide$no, ", ", pred_wide$'NA', sep = "")
   pred_wide <- pred_wide[,-(3:5)]
   
   prey_wide <- pivot_wider(yes_no_table_Prey, names_from = 'Role in ecosystem: Prey species identified', values_from = 'Count')
   prey_wide[is.na(prey_wide)] <- 0
   prey_long<-pivot_longer(prey_wide,cols=3:5,values_to="Count", names_to="Response")
   prey_long$Var<-"Prey"
   colnames(prey_long)<-c("Ocean","Fish","Response","Count","Var")
   prey_wide$Prey <- paste(prey_wide$yes, ", ", prey_wide$no, ", ", prey_wide$'NA', sep = "")
   prey_wide <- prey_wide[,-(3:5)]
   
   ### combine wide tables of temp, depth, pred, prey
   yes_no_count1 <- merge(temp_wide, depth_wide, by= c("Ocean","Focus species Common Name"))
   yes_no_count2 <- merge(yes_no_count1, pred_wide, by= c("Ocean","Focus species Common Name"))
   yes_no_counts_all <- merge(yes_no_count2, prey_wide, by= c("Ocean","Focus species Common Name"))
   
   #this table has counts for yes, no, NA (i.e., the cell was not filled in)
   View(yes_no_counts_all)   
   write.csv(yes_no_counts_all, file=paste("C:/Users/boldtj/Documents/VariousProposals/NationalForageFishWorkingGroup/2020_21/Figures&Tables/DataSummary/TempDepthPredPrey_Counts/TempDepthPredPrey_YesNoCounts_By_Ocean",".csv",sep=''))
   
   ### combine long tables for plotting
   yes_no_long<-rbind(temp_long,depth_long,pred_long,prey_long)
   write.csv(yes_no_long, file=paste("./TempDepthPredPrey_Counts/TempDepthPredPrey_YesNoCounts_By_Ocean_LongFormat",".csv",sep=''))
   
   #colour palette
   cb3Palette <- c("grey", "slateblue", "salmon")
   #library(viridis)
   # stacked barplots - facet wrap of Oceans
   ReportCount_ByVar_Oceans<-ggplot(yes_no_long)+
     geom_bar(aes(x=Fish, y=Count, fill=Response), stat="identity")+
     scale_colour_discrete(na.translate = F) +
     #scale_fill_viridis(discrete = TRUE,option = "D") +
     theme_bw()+
     #theme_classic()+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     scale_fill_manual(values=cb3Palette)+
     #facet_wrap(vars(prey_long$Ocean),nrow=3, ncol=4, scales="free", labeller="label_value")
     facet_grid(Var~Ocean, scales="free")
   png(paste0("./TempDepthPredPrey_Counts/ReportCount_ByVar_Oceans.png"),res=600, width=7,height=5, units="in")
   print(ReportCount_ByVar_Oceans)
   dev.off() 
   
  
   ###
   #==================================================
   # 2.a)	Case count by gear type
   #==================================================
   #=================================================================
   #By Ocean
   #subset by Oceans
   gear_Pac<-subset(data2,data2$Ocean=="Pacific")
   gear_Arc<-subset(data2,data2$Ocean=="Arctic")
   gear_Atl<-subset(data2,data2$Ocean=="Atlantic")

   # get unique list of species
   ff_species_PacificOcean <- na.omit(c(unique(unlist(gear_Pac$`Focus species Common Name`))))
   ff_species_ArcticOcean <- na.omit(c(unique(unlist(gear_Arc$`Focus species Common Name`))))
   ff_species_AtlanticOcean <- na.omit(c(unique(unlist(gear_Atl$`Focus species Common Name`))))
   
   # get unique list of gear types
   gear_types <- na.omit(c(unique(unlist(dropdowns[,18]))))
   
   # put species into columns with gear type as values for the rows in those columns
   gear_data_Pac <- pivot_wider(gear_Pac,id_cols=NULL,names_from = "Focus species Common Name",
                                values_from="Sampling gear used")
   gear_data_Arc <- pivot_wider(gear_Arc,id_cols=NULL,names_from = "Focus species Common Name",
                                values_from="Sampling gear used")
   gear_data_Atl <- pivot_wider(gear_Atl,id_cols=NULL,names_from = "Focus species Common Name",
                                  values_from="Sampling gear used")

   # subset the data to include only the species columns
   gear_data_Pac <- gear_data_Pac[,c(which(colnames(gear_data_Pac)%in%ff_species_PacificOcean))]
   gear_data_Arc <- gear_data_Arc[,c(which(colnames(gear_data_Arc)%in%ff_species_ArcticOcean))]
   gear_data_Atl <- gear_data_Atl[,c(which(colnames(gear_data_Atl)%in%ff_species_AtlanticOcean))]

   # subset the outcome column and counting how many times each of the gears occur in the column.
   # use the function lifestage_table
   gear_count_data_Pac <- lifestage_table(gear_types,gear_data_Pac)
   gear_count_data_Arc <- lifestage_table(gear_types,gear_data_Arc)
   gear_count_data_Atl <- lifestage_table(gear_types,gear_data_Atl)

   # rename columns
   colnames(gear_count_data_Pac)<-c("GearType","Fish", "Count")
   colnames(gear_count_data_Arc)<-c("GearType","Fish", "Count")
   colnames(gear_count_data_Atl)<-c("GearType","Fish", "Count")

   #change the order in which the gear types are plotted and listed in the legend
   gear_count_data_Pac$Gear = factor(gear_count_data_Pac$GearType, levels=c("bottom trawl" ,"commercial bottom trawl" ,"pelagic trawl" ,"commercial pelagic trawl","hydroacoustics" ,"dipnet","purse seine" ,"gillnets" ,"trap","line and handline" ,"tuck seine" ,"beach seine" ,"fixed gear" ,"mobile gear" ,"plankton net","tagging","not specified"))
   gear_count_data_Arc$Gear = factor(gear_count_data_Arc$GearType, levels=c("bottom trawl" ,"commercial bottom trawl" ,"pelagic trawl" ,"commercial pelagic trawl","hydroacoustics" ,"dipnet","purse seine" ,"gillnets" ,"trap","line and handline" ,"tuck seine" ,"beach seine" ,"fixed gear" ,"mobile gear" ,"plankton net","tagging","not specified"))
   gear_count_data_Atl$Gear = factor(gear_count_data_Atl$GearType, levels=c("bottom trawl" ,"commercial bottom trawl" ,"pelagic trawl" ,"commercial pelagic trawl","hydroacoustics" ,"dipnet","purse seine" ,"gillnets" ,"trap","line and handline" ,"tuck seine" ,"beach seine" ,"fixed gear" ,"mobile gear" ,"plankton net","tagging","not specified"))

   #remove "other" species from Pacific (one record)
   gear_count_data_Pac<- subset(gear_count_data_Pac,gear_count_data_Pac$Fish!="other")
   other<-subset(gear_count_data_Pac,gear_count_data_Pac$Fish=="other")
   
   #add Ocean column
   gear_count_data_Pac$Ocean = "Pacific"
   gear_count_data_Arc$Ocean = "Arctic"
   gear_count_data_Atl$Ocean = "Atlantic"

   #put all region counts together
   gear_count_data_allOceans<-rbind(gear_count_data_Pac,gear_count_data_Arc,gear_count_data_Atl)
   write.csv(gear_count_data_allOceans, file=paste("./GearStockDistr_Counts/gear_count_data_allOceans",".csv",sep=''))
   
   # colours for gear types
   mycolour1 <- c( "#000033","#000099","#0000FF" , "#008000", "#990099", "#FFCCFF",
                   "#33FFFF" , "#FF9933", "#66FF33",'#999966', "#CCCC99", "#FFFFCC",
                   "#008080", "#99CCCC", "#FF8080",'#9A6324',
                   "#666666", "#CCCCCC")
   
   # stacked barplots - facet wrap of all four DFO Regions
   GearType_allOceans<-ggplot(gear_count_data_allOceans)+
     geom_bar(aes(x=Fish, y=Count, fill=Gear), stat="identity")+
     scale_colour_discrete(na.translate = F) +
     theme_bw()+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     scale_fill_manual(values=mycolour1)+
     theme(legend.key=element_blank(), legend.key.size = unit(10,"point"))+
     facet_wrap(vars(gear_count_data_allOceans$Ocean),nrow=1, ncol=3, scales="free", labeller="label_value")
   
   png(paste0("./GearStockDistr_Counts/ReportCount_ByGear_allOceans.png"),res=600, width=7,height=5, units="in")
   print(GearType_allOceans)
   dev.off() 
 
   #===================================================
   
   
   #==================================================
   # 2.b)	Case count by type of stock structure info
   #==================================================
    #========================================================
   #i. subset by Oceans
   stock_Pac<-subset(data2,data2$Ocean=="Pacific")
   stock_Arc<-subset(data2,data2$Ocean=="Arctic")
   stock_Atl<-subset(data2,data2$Ocean=="Atlantic")

   # get unique list of species
   ff_species_PacificOcean <- na.omit(c(unique(unlist(stock_Pac$`Focus species Common Name`))))
   ff_species_ArcticOcean  <- na.omit(c(unique(unlist(stock_Arc$`Focus species Common Name`))))
   ff_species_AtlanticOcean <- na.omit(c(unique(unlist(stock_Atl$`Focus species Common Name`))))

   # get unique list of stock structure info
   stock_structure <- na.omit(c(unique(unlist(dropdowns[,20]))))
   
   # put species into columns with stock structure info as values for the rows in those columns
   stock_data_Pac <- pivot_wider(stock_Pac,id_cols=NULL,names_from = "Focus species Common Name",
                                 values_from="StockStructure")
   stock_data_Arc <- pivot_wider(stock_Arc,id_cols=NULL,names_from = "Focus species Common Name",
                                 values_from="StockStructure")
   stock_data_Atl <- pivot_wider(stock_Atl,id_cols=NULL,names_from = "Focus species Common Name",
                                   values_from="StockStructure")
   
   # subset the data to include only the species columns
   stock_data_Pac <- stock_data_Pac[,c(which(colnames(stock_data_Pac)%in%ff_species_PacificOcean))]
   stock_data_Arc <- stock_data_Arc[,c(which(colnames(stock_data_Arc)%in%ff_species_ArcticOcean))]
   stock_data_Atl <- stock_data_Atl[,c(which(colnames(stock_data_Atl)%in%ff_species_AtlanticOcean))]

   # subset the outcome column and counting how many times each of the stock structure types occur in the column.
   # use the function lifestage_table
   stock_count_data_Pac <- lifestage_table(stock_structure,stock_data_Pac)
   stock_count_data_Arc <- lifestage_table(stock_structure,stock_data_Arc)
   stock_count_data_Atl <- lifestage_table(stock_structure,stock_data_Atl)

   # rename columns
   colnames(stock_count_data_Pac)<-c("StockStructure","Fish", "Count")
   colnames(stock_count_data_Arc)<-c("StockStructure","Fish", "Count")
   colnames(stock_count_data_Atl)<-c("StockStructure","Fish", "Count")

   #add Ocean column
   stock_count_data_Pac$Ocean = "Pacific"
   stock_count_data_Arc$Ocean = "Arctic"
   stock_count_data_Atl$Ocean = "Atlantic"

   #put all region counts together
   stock_count_data_allOceans<-rbind(stock_count_data_Pac,stock_count_data_Arc,stock_count_data_Atl)
   stock_count_data_allOceans$Fish<- gsub('\\.', ' ', stock_count_data_allOceans$Fish)
   write.csv(stock_count_data_allOceans,file=paste("./GearStockDistr_Counts/stock_count_data_allOceans",".csv",sep=''))
   
   #change the order in which the gear types are plotted and listed in the legend
   stock_count_data_allOceans$Stock = factor(stock_count_data_allOceans$StockStructure,levels=c("genetics","otoliths","tagging","other"))
   
   mycolour3 <- c( "#6600CC","#6699FF" , "#339900","#666666")
   
   # stacked barplots - facet wrap of all four DFO Regions
   StockType_allOceans<-ggplot(stock_count_data_allOceans)+
     geom_bar(aes(x=Fish, y=Count, fill=Stock), stat="identity")+
     scale_colour_discrete(na.translate = F) +
     theme_bw()+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     scale_fill_manual(values=mycolour3)+
     theme(legend.key=element_blank(), legend.key.size = unit(10,"point"))+
     facet_wrap(vars(stock_count_data_allOceans$Ocean),nrow=1, ncol=3, scales="free", labeller="label_value")
   
   png(paste0("./GearStockDistr_Counts/ReportCount_ByStock_AllOceans.png"),res=600, width=7,height=5, units="in")
   print(StockType_allOceans)
   dev.off() 
   
   
   #==================================================
   # 2.c)	Case count by distribution data type
   #==================================================
     #============================================================
   #ii. subset by Oceans
   distrib_Pac<-subset(data2,data2$Ocean=="Pacific")
   distrib_Arc<-subset(data2,data2$Ocean=="Arctic")
   distrib_Atl<-subset(data2,data2$Ocean=="Atlantic")

   #subset by core species
   distrib_Pac<-distrib_Pac%>%
     filter(paste0(`Focus species Common Name`,`Ocean`) %in% paste0(speciesOcean[,1],speciesOcean[,2]))
   distrib_Arc<-distrib_Arc%>%
     filter(paste0(`Focus species Common Name`,`Ocean`) %in% paste0(speciesOcean[,1],speciesOcean[,2]))
   distrib_Atl<-distrib_Atl%>%
     filter(paste0(`Focus species Common Name`,`Ocean`) %in% paste0(speciesOcean[,1],speciesOcean[,2]))
   
   # get unique list of species
   ff_species_PacificOcean <- na.omit(c(unique(unlist(distrib_Pac$`Focus species Common Name`))))
   ff_species_ArcticOcean  <- na.omit(c(unique(unlist(distrib_Arc$`Focus species Common Name`))))
   ff_species_AtlanticOcean <- na.omit(c(unique(unlist(distrib_Atl$`Focus species Common Name`))))
   
   # get unique list of distribution data types
   distribution_types <- na.omit(c(unique(unlist(dropdowns[,21]))))
   
   # put species into columns with gear type as values for the rows in those columns
   distribution_data_Pac <- pivot_wider(distrib_Pac,id_cols=NULL,names_from = "Focus species Common Name",
                                        values_from="Distribution_DataType")
   distribution_data_Arc <- pivot_wider(distrib_Arc,id_cols=NULL,names_from = "Focus species Common Name",
                                        values_from="Distribution_DataType")
   distribution_data_Atl <- pivot_wider(distrib_Atl,id_cols=NULL,names_from = "Focus species Common Name",
                                          values_from="Distribution_DataType")
   
   # subset the data to include only the species columns
   distribution_data_Pac <- distribution_data_Pac[,c(which(colnames(distribution_data_Pac)%in%ff_species_PacificOcean))]
   distribution_data_Arc <- distribution_data_Arc[,c(which(colnames(distribution_data_Arc)%in%ff_species_ArcticOcean))]
   distribution_data_Atl <- distribution_data_Atl[,c(which(colnames(distribution_data_Atl)%in%ff_species_AtlanticOcean))]

   # subset the outcome column and counting how many times each of the distribution data types occur in the column.
   # use the function lifestage_table
   distribution_count_data_Pac <- lifestage_table(distribution_types,distribution_data_Pac)
   distribution_count_data_Arc <- lifestage_table(distribution_types,distribution_data_Arc)
   distribution_count_data_Atl <- lifestage_table(distribution_types,distribution_data_Atl)

   # rename columns
   colnames(distribution_count_data_Pac)<-c("Distribution_DataType","Fish", "Count")
   colnames(distribution_count_data_Arc)<-c("Distribution_DataType","Fish", "Count")
   colnames(distribution_count_data_Atl)<-c("Distribution_DataType","Fish", "Count")
  
   #add Ocean column
   distribution_count_data_Pac$Ocean = "Pacific"
   distribution_count_data_Arc$Ocean = "Arctic"
   distribution_count_data_Atl$Ocean = "Atlantic"

   #put all region counts together
   distribution_count_data_allOceans<-rbind(distribution_count_data_Pac,distribution_count_data_Arc,distribution_count_data_Atl)
   distribution_count_data_allOceans$Fish<- gsub('\\.', ' ', distribution_count_data_allOceans$Fish)
   write.csv(distribution_count_data_allOceans, file=paste("./GearStockDistr_Counts/distribution_count_data_allOceans",".csv",sep=''))
   
   #change the order in which the gear types are plotted and listed in the legend
   distribution_count_data_allOceans$Distribution = factor(distribution_count_data_allOceans$Distribution_DataType,levels=c("abundance", "standardized abundance", "catch","standardized biomass", "CPUE", "size","age", "DNA", "TEK or LEK" ,"other"))
   
   # stacked barplots - facet wrap of all four DFO Regions
   DistributionData_allOceans<-ggplot(distribution_count_data_allOceans)+
     geom_bar(aes(x=Fish, y=Count, fill=Distribution), stat="identity")+
     scale_colour_discrete(na.translate = F) +
     theme_bw()+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     #scale_fill_manual(values=mycolour4)+
     scale_fill_brewer(palette = "Spectral")+
     theme(legend.key=element_blank(), legend.key.size = unit(10,"point"))+
     facet_wrap(vars(distribution_count_data_allOceans$Ocean),nrow=1, ncol=3, scales="free", labeller="label_value")
   
   png(paste0("./GearStockDistr_Counts/DistributionData_allOceans.png"),res=600, width=7,height=5, units="in")
   print(DistributionData_allOceans)
   dev.off() 
   
   
   
   #================================================================================
   #===============================================================================
   #      END
   #===============================================================================
   #===============================================================================
