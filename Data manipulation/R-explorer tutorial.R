library(DataExplorer)

################ VISUALIZE DATA STRUCTURE ##########################################
#visualizzare la struttura di pi?? tabelle, con due grafici
#install.packages("nycflights13")
library(nycflights13) #libreria che contiene dataset d'esempio
#library(mlbench) un'altra libreria che contiene molti dataset


data_list <- list(airlines, airports, flights, planes, weather) #name of different dataset
plot_str(data_list)
plot_str(data_list, type = "r") #radiant version


#unire le tabelle: merge all tables for a consistent full dataset
merge_airlines <- merge(flights, airlines, by = "carrier", all.x = TRUE)  #all.x mantiene tutte le righe che non mergiano, mettendo NA dove mancano dati
merge_planes <- merge(merge_airlines, planes, by = "tailnum", all.x = TRUE, suffixes = c("_flights", "_planes"))
merge_airports_origin <- merge(merge_planes, airports, by.x = "origin", by.y = "faa", all.x = TRUE, suffixes = c("_carrier", "_origin"))
final_data <- merge(merge_airports_origin, airports, by.x = "dest", by.y = "faa", all.x = TRUE, suffixes = c("_origin", "_dest"))

################################# BASIC ###########################################

#basic descriptive stats
introduce(final_data)

########################### MISSING VALUE #########################################
#missing data profile
mv <- plot_missing(final_data)

#replace MV
final_data <- set_missing(final_data, value = 0, exclude = "speed") #continuos vars
final_data <- set_missing(final_data, value = "unknown") #categorical vars

#################################### DISTRIBUZIONE ######################################
#bar chart for all discrete features
plot_bar(final_data)
plot_bar(final_data, with = "arr_delay") #condizionatamente a una var continua
#correggere le categorie duplicate con nomi leggermente differenti
plot_bar(final_data$manufacturer)
final_data[which(final_data$manufacturer == "AIRBUS INDUSTRIE"),]$manufacturer <- "AIRBUS"
final_data[which(final_data$manufacturer == "CANADAIR LTD"),]$manufacturer <- "CANADAIR"
final_data[which(final_data$manufacturer %in% c("MCDONNELL DOUGLAS AIRCRAFT CO", "MCDONNELL DOUGLAS CORPORATION")),]$manufacturer <- "MCDONNELL DOUGLAS"
plot_bar(final_data$manufacturer)

#distributions for all continuous features
plot_histogram(final_data)

################################## SCATTERPLOT e BOXPLOT ####################################
#reduce data for demonstration
arr_delay_df <- final_data[, c("arr_delay", "month", "day", "hour", "minute", "dep_delay", "distance", "year_planes", "seats", "speed")]

plot_boxplot(arr_delay_df, by = "arr_delay")
plot_scatterplot(arr_delay_df, by = "arr_delay", size = 0.5)

########################## CORRELAZIONE ###########################################

#correlation heatmap for all features
plot_correlation(final_data, maxcat = 5L, use = "pairwise.complete.obs")
#correlation heatmap for continuous features
plot_correlation(final_data, type = "c", use = "pairwise.complete.obs")
#correlation heatmap for discrete features
plot_correlation(final_data, type = "d", use = "pairwise.complete.obs")

########################## PCA #######################################################

#prima si puliscono i dati degli NA: get cleaner dataset
#select a group of interesting variables

#na.omit = Remove rows with missing values on columns specified
clean_df <- na.omit(final_data[, c("origin", "name_carrier", "type", "engine", "dep_delay", "arr_delay", "air_time", "month", "hour", "year_planes", "seats", "speed")])

#perform PCA
#ottengo %variance explained e relative importance delle variabili nelle componenti
plot_prcomp(clean_df)

########################## GROUP SPARSE CATEGORIES #####################################
#we want to collapse a discrete variable with multiple categories
#if we want to create a new category from all low-frequency categories
final_data <- group_category(data = final_data, feature = "manufacturer", threshold = 0.2, update = T,
               category_name = "ALTRO")  # < 20%freq
plot_bar(final_data$manufacturer)

#if we want to create a new category by measure of another variable
final_data <- group_category(data = final_data, feature = "name_carrier", threshold = 0.2, measure = "distance", update = T)
plot_bar(final_data$name_carrier)

########################### DUMMIFY for ML ALGORITHMS ###################################
#transform data into binary format
plot_str(
  list(
    "original" = final_data,
    "dummified" = dummify(final_data, maxcat = 5L)
  )
)


######################### REPORTING #############################
#output a html file

#supply a response variable (if applicable) to automate various bivariate analyses
create_report(final_data, y = "arr_delay")


## Customize report configuration
config <- list(
  "introduce" = list(),
  "plot_str" = list(
    "type" = "diagonal",
    "fontSize" = 35,
    "width" = 1000,
    "margin" = list("left" = 350, "right" = 250)
  ),
  "plot_missing" = list(),
  "plot_histogram" = list(),
  "plot_bar" = list(),
  "plot_correlation" = list("use" = "pairwise.complete.obs"),
  "plot_prcomp" = list(),
  "plot_boxplot" = list(),
  "plot_scatterplot" = list()
)
## Create final report
create_report(final_data, y = "arr_delay", config = config)
