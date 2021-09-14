#' A function to make a table of the number of times a pressure occurs in a set of columns
#'
#' This function to make a table of the number of times a pressure occurs in a set of columns.
#' @param pressures The name of the pressure to be counted
#' @param parameter_columns The column(s) where the occurences of the pressure will be counted
#' @keywords forage fish, pressures, drivers, literature summary
#' @export
#' @examples
#' pressure_table(c("saliity","temperature"),data[,22:32])


  pressure_table<-function(pressures,parameter_columns){
  require(tidyr)
    parameter_columns<-data.frame(parameter_columns)
    t2<-data.frame(array(dim=c(length(pressures),length(parameter_columns))))
    colnames(t2)<-colnames(parameter_columns)
    for(i in 1:length(pressures)){
      t2[i,]<-apply(parameter_columns,MARGIN=2,function(x){how_many_times(pressures[i],x)})
    }
    t2$pressure<-pressures
    t2<-pivot_longer(t2,colnames(parameter_columns),names_to="outcome",values_to="count")
    return(t2)
  }


#' A function to determine how many times a pressure occurs in a column
#'
#' This function counts the number of instances of a pressure (word match) in a column where each row can contain
#' multiple pressures
#' @param pressures The name of the pressure to be counted
#' @param parameter_columns The column(s) where the occurences of the pressure will be counted
#' @keywords forage fish, pressures, drivers, literature summary
#' @export
#' @examples
#' how_many_times("salinity",data[,22:35])

how_many_times<-function(pressures,parameter_columns){
  hmt<-length(grep(pressures,parameter_columns,fixed=TRUE))
  return(hmt)}
#########

#########

#########

#' A function to make a table of the number of times a life history stage occurs in the column "Life stage studied (Adult, Juvenile, Larval, Egg)"

#'
#' This function to make a table of the number of times a life history stage occurs in that column.
#' @param lifestage The name of the life stage to be counted
#' @param parameter_column The column where the occurences of the the life history stage will be counted
#' @keywords forage fish, life stage, species common name, literature summary
#' @export
#' @examples


lifestage_table<-function(lifestage,parameter_columns){
  require(tidyr)
  parameter_columns<-data.frame(parameter_columns)
  t3<-data.frame(array(dim=c(length(lifestage),length(parameter_columns))))
  colnames(t3)<-colnames(parameter_columns)
  for(i in 1:length(lifestage)){
    t3[i,]<-apply(parameter_columns,MARGIN=2,function(x){how_many_times(lifestage[i],x)})
  }
  t3$lifestage<-lifestage
  t3<-pivot_longer(t3,colnames(parameter_columns),names_to="outcome",values_to="count")
  return(t3)
}


#' A function to determine how many times a lifestage occurs in a column
#'
#' This function counts the number of instances of a lifestage (word match) in a column where each row can contain
#' multiple lifestages
#' @param lifestage The name of the lifestage to be counted
#' @param parameter_columns The column(s) where the occurences of the lifestage will be counted
#' @keywords forage fish, life stage, species common name, literature summary
#' @export
#' @examples

LHS_how_many_times<-function(lifestage,parameter_columns){
  LHS_hmt<-length(grep(lifestage,parameter_columns))
  return(LHS_hmt)}

#' A function to to add a row for each instance where a string occurs in a column
#'
#' A function to to add a row for each instance where a comma-seperated string occurs in a column (e.g. if a cell contains the string "Atlantic, Pacific, Arctic
#' this suction will replace the row with three rows containing "Atlantic", "Pacific" and "Arctic"). This is useful when compiling summary
#' statistics for species that occur in multiple oceans.
#' @param data The data frame containing the rows to expand
#' @param column_name The column name holding the string to expand the data over in quotes (e.g. "Ocean").
#' @keywords forage fish, life stage, species common name, literature summary
#' @export
#' @examples
#' expand_ocean(data, "ocean")

expand_ocean<-function(data,column_name){
  require(dplyr)
  # data<-read.csv("C:/Users/rooperc/Desktop/Oceans.csv")
  #  column_name<-"ocean"

  datae<-NULL
  for(i in 1:dim(data)[1]){
    d1<-unlist(strsplit(data[i,column_name],","))
    df<-data[i,]

    if(length(d1)>1){
      d2<-df[rep(seq_len(nrow(df)), each = length(d1)), ]
      d2[,column_name]<-d1
      #    data<-data[-i,]
      datae<-rbind(datae,d2)}

    if(length(d1)==1){
      datae<-rbind(datae,df)}
  }
  return(datae)

}




