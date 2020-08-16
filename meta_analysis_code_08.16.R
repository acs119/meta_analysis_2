###Install packages##
install.packages("funModeling")

##Library###
library(magrittr) # for piping (%>%)
library(dplyr) # for data manipulation
library(stringr) # string manipulation
library(tidyr)
library(reshape2)
library(Matrix)
library(data.table)
library(metafor) #meta-analysis##
library(Formula)
library(ggplot2)
library(histogram)
library(grid)
library(funModeling)
library(frequency)
library(forestplot)
library(rlang)

##Set working directory##
setwd("C:/Users/acs119/OneDrive - Imperial College London/ICL/Thesis/R_analysis")

#################### ANALYSIS OF THE LANDSCAPE METRICS #######################################################################################

##Function to calculate the percentaje of natural and arable area in the landscape analysis
##a= area_natural_m2 or area_arable_m2; b = area_m2
percentage <- function (a, b) {  
  result<- ((a/b)* 100)
  return(result)
}

### Import landscape analysis data
landscape2011_2019<-read.csv("data2011_2019_07_18.csv", header = TRUE, sep = ",")%>%
  ##Calculate the polygon area by classNames (natural = 0; arable = 2; others= 3)
  group_by(classNames, Landscape_)%>%
  mutate(area_natural_m2 = if_else(classNames== 0, sum(area_m2), 0),
         area_arable_m2 = if_else(classNames == 2, sum(area_m2), 0),
         area_others_m2 = if_else(classNames == 3, sum(area_m2), 0),
         ##calculate the number of patches per cover class
         number_patches_natural = length(classNames))%>%
  mutate(min_distance = if_else(min_distance >= 1000, 1000, min_distance))%>%
  ungroup()%>%
  ##Get only the number of natural patches    
  mutate(number_patches_natural= as.numeric(number_patches_natural),
         number_patches_natural = if_else(classNames != 0, 0, number_patches_natural))%>%
  ##To summarise by classNames and Landscape_
  distinct(classNames, Landscape_, .keep_all = TRUE)%>%
  ##Regroup by Landsca and mutate again to get one row per study point
  group_by(Landscape_)%>%
  mutate(area_natural_m2 = sum(area_natural_m2), 
         area_arable_m2 = sum(area_arable_m2), 
         area_others_m2 = sum(area_others_m2),
         number_patches_natural = sum(number_patches_natural))%>%
  ungroup()%>%
  distinct(Landscape_, .keep_all = TRUE)%>%
  ##Calculate the total area per study point
  mutate(area_m2 = (area_natural_m2 + area_arable_m2 + area_others_m2))%>%
  ##Calculate the percentaje of natural and arable area
  mutate(natural_percentage = percentage(area_natural_m2, area_m2),
         arable_percentage = percentage(area_arable_m2, area_m2),
         ##Calculate the patch density (# of natural patches in 100 ha)
         ##(10,000 m2 * #of natural patches found in the classification / total area analysed in the classification)(CHECK)
         density_patches_natural = ((number_patches_natural * 10000)/ area_m2))

landscape2001_2010<-read.csv("data2001_2010_07_18.csv", header = TRUE, sep = ",")%>%
  ##Calculate the polygon area by classNames (natural = 0; arable = 2; others= 3)
  group_by(classNames, Landscape_)%>%
  mutate(area_natural_m2 = if_else(classNames== 0, sum(area_m2), 0),
         area_arable_m2 = if_else(classNames == 2, sum(area_m2), 0),
         area_others_m2 = if_else(classNames == 3, sum(area_m2), 0),
         ##calculate the number of patches per cover class
         number_patches_natural = length(classNames))%>%
  mutate(min_distance = if_else(min_distance >= 1000, 1000, min_distance))%>%
  ungroup()%>%
  ##Get only the number of natural patches    
  mutate(number_patches_natural= as.numeric(number_patches_natural),
         number_patches_natural = if_else(classNames != 0, 0, number_patches_natural))%>%
  ##To summarise by classNames and Landscape_
  distinct(classNames, Landscape_, .keep_all = TRUE)%>%
  ##Regroup by Landsca and mutate again to get one row per study point
  group_by(Landscape_)%>%
  mutate(area_natural_m2 = sum(area_natural_m2), 
         area_arable_m2 = sum(area_arable_m2), 
         area_others_m2 = sum(area_others_m2),
         number_patches_natural = sum(number_patches_natural))%>%
  ungroup()%>%
  distinct(Landscape_, .keep_all = TRUE)%>%
  ##Calculate the total area per study point
  mutate(area_m2 = (area_natural_m2 + area_arable_m2 + area_others_m2))%>%
  ##Calculate the percentaje of natural and arable area
  mutate(natural_percentage = percentage(area_natural_m2, area_m2),
         arable_percentage = percentage(area_arable_m2, area_m2),
         ##Calculate the patch density (# of natural patches in 100 ha)
         ##(10,000 m2 * #of natural patches found in the classification / total area analysed in the classification)(CHECK)
         density_patches_natural = ((number_patches_natural * 10000)/ area_m2))

landscape1986_2000<-read.csv("data1986_2000_07_18.csv", header = TRUE, sep = ",")%>%
  ##Calculate the polygon area by classNames (natural = 0; arable = 2; others= 3)
  group_by(classNames, Landscape_)%>%
  mutate(area_natural_m2 = if_else(classNames== 0, sum(area_m2), 0),
         area_arable_m2 = if_else(classNames == 2, sum(area_m2), 0),
         area_others_m2 = if_else(classNames == 3, sum(area_m2), 0),
         ##calculate the number of patches per cover class
         number_patches_natural = length(classNames))%>%
  mutate(min_distance = if_else(min_distance >= 1000, 1000, min_distance))%>%
  ungroup()%>%
  ##Get only the number of natural patches    
  mutate(number_patches_natural= as.numeric(number_patches_natural),
         number_patches_natural = if_else(classNames != 0, 0, number_patches_natural))%>%
  ##To summarise by classNames and Landscape_
  distinct(classNames, Landscape_, .keep_all = TRUE)%>%
  ##Regroup by Landsca and mutate again to get one row per study point
  group_by(Landscape_)%>%
  mutate(area_natural_m2 = sum(area_natural_m2), 
         area_arable_m2 = sum(area_arable_m2), 
         area_others_m2 = sum(area_others_m2),
         number_patches_natural = sum(number_patches_natural))%>%
  ungroup()%>%
  distinct(Landscape_, .keep_all = TRUE)%>%
  ##Calculate the total area per study point
  mutate(area_m2 = (area_natural_m2 + area_arable_m2 + area_others_m2))%>%
  ##Calculate the percentaje of natural and arable area
  mutate(natural_percentage = percentage(area_natural_m2, area_m2),
         arable_percentage = percentage(area_arable_m2, area_m2),
         ##Calculate the patch density (# of natural patches in 100 ha)
         ##(10,000 m2 * #of natural patches found in the classification / total area analysed in the classification)(CHECK)
         density_patches_natural = ((number_patches_natural * 10000)/ area_m2))

landscape_data<- rbind(landscape2011_2019, landscape2001_2010, landscape1986_2000)

###############################################################################################################################

###Equations to calculate the SD from SE, IC and IQR
##Equation to calculate the SD from SE (Higgins & Green 2011)(a= B_error_value; b= N_samples)
##http://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
SE_SD <- function (a, b) {  
  result<- a * sqrt(b)
  return(result)
}

##Equation to calculate the SD from M_IQR (Hozo et al., 2005) 
##(a= N_samples; b=B_error_range; c=B_error_value; d= B_error_range.1)
M_IQR_SD<- function (a, b,c,d) {  
  result<- sqrt(((a + 1)/(48 * a*((a-1)^2))) * (((a^2) + 3) * ((b - (2*c) + d)^2) + (4* (a^2)) * ((d - b)^2)))
  return(result)
}

##Equation to calculate the SD from CI (Higgins & Green 2011) (a= N_samples; b= B_error_value)
##http://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
CI_SD<- function (a, b) {  
  result<- (sqrt(a) * (b/((qt((1-(0.05/2)), (a - 1)))*2)))
  return(result)
}

### Complete data set and list dataset ##
list<-read.csv("Meta-analysis_list_08.09.csv", header = TRUE,  sep = ",")%>%
  rename("ID"="ï..ID",
         "Exclusion_reason" = "Database.local...exclusion.reason", 
         "Inclusion_yes_no" = "Database.local...included...Yes.No.To.do.")%>%
  filter(Exclusion_reason != "Duplicate")%>% ##exclude duplicate studies
  filter(Article_source != "From google scholar search")%>% ##exclude the study included using google scholar search
  subset(!is.na(ID))%>%mutate(Title = str_to_lower(Title, locale = "en"))%>%
  mutate(ID = as.character(ID),
         Article_source = if_else(str_detect(Article_source, "Beillouin et al", negate = FALSE), "meta_analysis", 
                                  if_else(str_detect(Article_source, "Scopus", negate = FALSE)| str_detect(Article_source, "Landscape Structure", negate = FALSE), "Scopus_Web",
                                          if_else(str_detect(Article_source, "From references", negate = FALSE)|str_detect(Article_source, "From table", negate = FALSE) , "references",
                                                  Article_source))))

meta <- read.csv("Meta-analysis biodiversity data_08.09.csv", header = TRUE,  sep = ",")%>%
  rename("ID"="ï..ID")%>%
  mutate(ID= as.character(ID))
  
###Join the meta-analysis list (year of publication);
data<- right_join(x= meta, y=list, by="ID")%>%
  filter(!is.na(Comparison_ID))%>%
  ##To transform factors to numeric, and lower case##  
  mutate(B_SD = as.numeric(as.character(B_SD)), 
         B_value = as.numeric(as.character(B_value)),
         B_error_value = as.numeric(as.character(B_error_value)),
         B_error_range = as.numeric(as.character(B_error_range)),
         B_error_range.1 = as.numeric(as.character(B_error_range.1)),
         Lat= as.numeric(as.character(Lat)),
         Long = as.numeric(as.character(Long)),
         N_samples = as.numeric(as.character(N_samples)),
         Functional_group = as.character(Functional_group),
         Taxa_Class = as.character(Taxa_Class),
         Country = as.factor(Country),
         Crop_main = as.character(Crop_main),
         System = as.character(as.factor(System)),
         B_error_measure = str_to_lower(B_error_measure, locale = "en"),
         Functional_group = str_to_lower(Functional_group, locale = "en"),
         Taxa_Class = str_to_lower(Taxa_Class, locale = "en"),
         Crop_main = str_to_lower(Crop_main, locale = "en"),
         System_raw = str_to_lower(System_raw, locale = "en"),
         ID= as.character(ID),
         System = str_to_lower(System, locale = "en"),
         Lat_2 = round(Lat, digits = 0))%>%
  ##to eliminate white spaces at the end of country and crop##
  mutate(Country = gsub(" $", "", Country, perl=T),
         #Crop_main= gsub(" $", "", Crop_main, perl = T),
         System= gsub(" $", "", System, perl = T),
         Lat= gsub("^(.*?),.*", "\\1", Lat), # when several entries, take the first
         Lat = gsub("^(.*?);.*", "\\1", Lat),
         Long = gsub("^(.*?),.*", "\\1", Long), 
         Long = gsub("^(.*?);.*", "\\1", Long))%>%
  
  ### Check through frequency of country and crop, and clean data again to fix spelling errors or inconsistencies###
  mutate(Country = if_else(Country == "California, USA", "USA",
                           if_else(Country== "Washington, USA", "USA",
                                   if_else(Country== "Kenia", "Kenya",
                                           if_else(Country == "Sweeden", "Sweden",
                                                   if_else(Country== "England", "UK",
                                                           if_else(Country== "Scotland", "UK",
                                                                   if_else(Country== "Wales", "UK",
                                                                           Country))))))))%>%
  # select only the necessary columns # 
  select(ID, Comparison_ID, Crop_main, System_raw, Management_system_raw, Management_system, 
         Taxa, Taxa_details, Taxa_Class, Functional_group, Crop_clean, System, Comparison_class,
         Taxa_group, B_measure,B_value, B_error_measure, B_error_value, B_error_range, B_error_range.1,
         B_SD, N_samples, B_ground, Location, Country, Lat, Long, Experiment_stage, Data_entry, 
         Lat_2, Year)%>%
  #filter natural treatment and diversity measurement != species richness and abundance####
filter(Comparison_class != "Natural", Comparison_class != "NA - exclude")%>%
  filter(B_measure == "Abundance" | B_measure == "Species Richness")%>%
  
  ###Filter studies that have NA in the mean, SD and N column###
  filter(!is.na(B_error_measure))%>%
  filter(!is.na(N_samples))%>%
  filter(!is.na(B_error_value))%>%
  filter(!is.na(B_value))%>%
  
  ##count the number of B_measures per Study ID (now that biodiversity metrics different than Abundance and richness are
  ##filtered out, we need to make sure that at least two B_measures are in each ID to keep it in the data)
  group_by(ID, B_measure) %>%
  add_tally()%>%
  ungroup()%>%
  # filter out the studies that has only treatment and no control-because I filtered Abundance and species richness only
  filter(n > 1)%>%
  
  ### Check through frequency of system and re-classificate###
  mutate(System = if_else(System== "silvo-pasture dense" | System=="silvo-pasture sparse", "agroforestry",
                          if_else(System_raw == "cover crops", "cover crops",
                                  if_else(System_raw == "associated plant species", "associated plant species",
                                          if_else(System_raw == "vegetation strip of crops", "intercropping",
                                                  if_else(System_raw == "mulching", "cover crops",
                                                          if_else(System_raw == "no hedgerow", "monoculture",
                                                                  System))))))) %>%
  
  ## Check through frequency of Functional group and clean data again to fix spelling errors or inconsistencies###
  mutate(Functional_group = if_else(Functional_group == "decomposer" | Functional_group== "descomposers", "decomposers",
                                    if_else(Functional_group== "frugivores" | Functional_group== "frugivore", "frugivorous",
                                            if_else(Functional_group== "other", "others",
                                                    if_else(Functional_group== "herbivore", "herbivores",
                                                            if_else(Functional_group=="granivore", "granivores",
                                                                    if_else(Functional_group== "plant" | Functional_group=="plants", "autotrophs",
                                                                            if_else(Functional_group== "pollinator", "pollinators",
                                                                                    if_else(Functional_group== "predator", "predators",
                                                                                            if_else(Functional_group== "weed", "weeds",
                                                                                                    if_else(Functional_group=="omnivore", "omnivores",
                                                                                                            Functional_group))))))))))) %>%
  
  ##Re-classification functional groups##
  mutate(FG_recla = if_else(Functional_group== "frugivorous" | Functional_group == "granivores" | Functional_group== "herbivores" | Functional_group== "weeds" | Functional_group == "pest control", "pest",
                            if_else(Functional_group== "predators" | Functional_group== "insectivores" | Functional_group == "pest parasitoid", "natural enemies",
                                    if_else(Functional_group=="omnivores", "omnivores",
                                            Functional_group))))%>%
  ##Re-classification taxa group##
  
  mutate(Taxa_group = if_else(Taxa_Class== "amphibian", "amphibians",
                              if_else(Taxa_Class=="arachnida" |Taxa_Class=="arthropods"|Taxa_Class=="bee"|Taxa_Class=="insect"|Taxa_Class=="diplopoda"|Taxa_Class=="chilopoda"|Taxa_Class=="pauropoda"|Taxa_Class=="entognatha"| Taxa_Class =="symphyla","arthropods",
                                      if_else(Taxa_Class=="herb"|Taxa_Class=="plants"|Taxa_Class=="tree"|Taxa_Class=="weed", "plants",
                                              Taxa_Class))))%>%
  ###Classified Countries in Continents###
  mutate(Continent= if_else(Country == "Argentina"| Country =="Brazil" | Country =="Ecuador" | Country =="Uruguay" |  Country == "Peru"| Country =="Colombia", "south america",
                            if_else(Country == "Canada"|Country =="USA"| Country =="Mexico", "north america", 
                                    if_else(Country =="Germany"| Country =="Belgium"|Country == "France"|Country == "Swiss"| Country == "Sweden"| Country == "Poland"| Country =="Spain"| Country =="Italy"| Country =="Portugal"| Country ==  "UK" | Country =="Finland" | Country =="Hungary"| Country =="Switzerland"| Country== "Netherlands", "europe",
                                            if_else(Country == "China" | Country =="Turkey"| Country =="India"| Country =="Indonesia"| Country =="Vietnam"| Country =="Japan"| Country =="Malaysia", "asia",
                                                    if_else(Country =="Cameroon"| Country =="Egypt"| Country == "Ghana"| Country == "Nigeria"| Country =="South Africa"| Country =="Kenya"| Country =="Malawi" | Country =="Uganda"|Country == "Ethiopia"| Country =="Sao Tome"| Country =="Republic of Benin"| Country == "Zambia", "africa",
                                                            if_else(Country =="Costa Rica"| Country == "Panama"| Country =="Guatemala"| Country =="Nicaragua", "central america",
                                                                    if_else(Country =="Dominican Republic" | Country == "Jamaica", "caribbean",
                                                                            if_else(Country == "New Zealand"|Country =="Australia", "oceania",
                                                                                    Country)))))))))%>%
  ##Re-classified the Data entry column
  mutate(Data_entry = if_else(Data_entry == "EC / SKJ/AS", "AS",
                              if_else(Data_entry == "SDJ, AS", "AS",
                                      Data_entry)))%>%
  
  # Equations to calculate the SD from SE, SEM, inter quartil-range and CI##
  mutate(SE_SD = SE_SD(B_error_value, N_samples),
         M_IQR_SD = M_IQR_SD( N_samples, B_error_range, B_error_value, B_error_range.1),
         CI_SD = CI_SD(N_samples, B_error_value)) %>%
  
  # Re-calculation of SD##
  mutate(B_SD_recal = if_else(B_error_measure == "standard error", (SE_SD), 
                              if_else(B_error_measure == "square error of the mean", (SE_SD),
                                      if_else(B_error_measure == "median,iqr", (M_IQR_SD),
                                              if_else(B_error_measure == "confidence intervals", (CI_SD),
                                                      (B_error_value)))))) %>%
  mutate(Experiment_stage = if_else(ID == "733", "1", Experiment_stage))%>%
  ##Create a new colum with the identification of every row to join the observations (simplified vs diversified)
  mutate(Effect_ID = paste(ID, Taxa, Taxa_details, Taxa_Class, Functional_group, 
                           FG_recla, B_measure, Experiment_stage, Country, Continent,sep="_"))%>%
  ##Create a new colum with the identification of every row for the landscape complexity analysis
  mutate(Landscape_ID= paste(ID, Country, Continent, Lat, Long, sep="_"))%>%
  subset(FG_recla != "others")%>% ##Filter Functional groups recla= OTHERS
  filter(!is.na(FG_recla))%>% ## Filter the rows that are empty
  filter(B_value!=0)%>% ##Filter the means that are = to zero
  filter(!is.na(Lat))%>% ##Filter the studies that does not have geographic coordinates
  ###Join landscape data
  full_join(y=landscape_data, by= c("Landscape_ID" = "Landscape_"))%>%
  filter(!is.na(ID))
  
###clean data###
names(data)[1]<-"ID"
data[data=="ND"] <- "nd"
data[data=="check"] <- NA
data[data=="Check"] <- NA
data[data=="na"] <- NA
data[data==""] <- NA
data[data=="."] <- NA
data[data=="N/A"] <- NA
data[data=="#N/A"] <- NA

#-----------------------------------## DATABASE META-ANALYSIS -----------------------------------------------------------------#
###Subset data by Comparison_class (i.e., simplified and diversified systems)
###IMPORTANTE: REVISAR QUE TODAS LAS POSIBILIDADES DE COMBINACIONES T Y C ESTEN HECHAS###
data_T<- data %>% subset(Comparison_class == "Diversified") %>% 
  rename(Comparison_ID_T = Comparison_ID, mean_T = B_value, SD_T = B_SD_recal, N_samples_T = N_samples)

data_C<- data %>% subset(Comparison_class == "Simplified") %>% ##I need to decide if I will include NA - exclude##
  rename(Comparison_ID_C = Comparison_ID, mean_C = B_value, SD_C = B_SD_recal, N_samples_C = N_samples)

##Data table with the format to run the meta-analysis T vs C##
data_MA <- full_join(x = data_T, y= data_C, by = c("Effect_ID", "Taxa", "Taxa_details","Taxa_Class",
                                                   "Functional_group", "Taxa_group", "FG_recla", "Experiment_stage",
                                                   "B_measure", "Country", "Year", "Continent"), 
                     suffix = c("_T", "_C"))%>%
  filter(!is.na(ID_C))%>%
  filter(!is.na(ID_T))%>%
  mutate(prueba_logRR_C = sqrt((N_samples_C*mean_C)/SD_C),
         prueba_logRR_T = sqrt((N_samples_T*mean_T)/SD_T),
         df = (N_samples_T+N_samples_C -2))%>%
  mutate(natural_percentage_mean = ((natural_percentage_T+natural_percentage_C)/2),
         arable_percentage_mean = ((arable_percentage_T+arable_percentage_C)/2),
         density_patches_natural_mean = ((density_patches_natural_T+density_patches_natural_C)/2),
         min_distance_mean = ((min_distance_T + min_distance_C)/2))%>%
  mutate(natural_percentage_mean = round(natural_percentage_mean, digits = 0),
         arable_percentage_mean = round(arable_percentage_mean, digits = 0),
         density_patches_natural_mean = round(density_patches_natural_mean, digits = 0),
         min_distance_mean = round(min_distance_mean, digits = 0))

###To use the log response ratio the sqrt((N_samples_C*mean_C)/SD_C) should be ~ 3 (Hedges et al. 1999)
length(which(data_MA$prueba_logRR_C > 3))/length(data_MA$prueba_logRR_C)
length(which(data_MA$prueba_logRR_T > 3))/length(data_MA$prueba_logRR_C)

###Calculate the effect size using log response ratio Reference: Hedge et al. 1999 (vtype=LS)#####
#I am not going to use the formula proposed by LAJEUNESSE 2011 (HO), because I need the correlation between ES###
### see = https://rstudio-pubs-static.s3.amazonaws.com/28456_ea0b1faf0f4645cc8af81d81aaf0c1af.html####
##LRR= response ratio, LRR_var= variance###
### Filter the studies that have LRR_var = 0, because the meta-analysis does not run with values = to 0###
###Calculate the effect size for the whole database
##To calculate the log response ratio I shoud exclude means = 0
effectsize_logRR <- escalc(measure = "ROM", m1i= mean_T, m2i= mean_C, sd1i= SD_T,sd2i= SD_C,n1i= N_samples_T, 
                           n2i= N_samples_C, data= data_MA, var.names=c("LRR","LRR_var"),vtype="LS",digits=4)%>%
  mutate(LRR_se = sqrt(LRR_var),
         LRR_se_1 = (1/LRR_se))%>%
  filter(LRR_var != 0)%>% #exclude the variance = 0, because is not posible to do the meta-analysis R: https://stat.ethz.ch/pipermail/r-help/2014-August/421166.html
  select(ID_C, Taxa_Class, Taxa_group, Taxa_details, Comparison_class_C, B_measure, mean_C, N_samples_C, Country, Lat_2_C, Year,
         FG_recla, Continent, SD_C, ID_T,Comparison_class_T, mean_T, N_samples_T, Lat_2_T, Continent,
         SD_T, natural_percentage_mean, arable_percentage_mean, density_patches_natural_mean, min_distance_mean,
         LRR, LRR_var, LRR_se)%>%
  mutate(Year = as.numeric(Year),
         min_log_distance_mean = log(min_distance_mean+1)) #add a new column with the log min distance

hist(effectsize_logRR$LRR) ##Frequency of Effect sizes##

##SUBSET DATA ABUNDANCE (with ES = log response ratio calculated)
abundance_logRR <- effectsize_logRR %>% subset(B_measure == "Abundance")
abundance_logRR$ES_ID <- as.numeric(1:nrow(abundance_logRR)) #add a new column with the effect size ID number
hist(abundance_logRR$LRR) ##Frequency of ES##

##SUBSET DATA SPECIES RICHNESS (with ES = log response ratio calculated)
richness_logRR <- effectsize_logRR %>% subset(B_measure == "Species Richness")
richness_logRR$ES_ID <- as.numeric(1:nrow(richness_logRR)) #add a new column with the effect size ID number
hist(richness_logRR$LRR) ##Frequency of ES##

### EFFECT SIZE DATABASE CHARACTERISTIC ####
###Check if  I have more than one ES per study (TG, FG, AS, Country)##
###Lopez-Lopez et al 2018 recomend to do this###
addmargins(table(abundance_logRR$Taxa_group, abundance_logRR$FG_recla))
addmargins(table(abundance_logRR$FG_recla, abundance_logRR$Continent))

##################################################################################################################
###### LITERATURE SEARCH
# Details of the search process of studies with the potential to be included in the meta-analysis
addmargins(table(list$Article_source))

###### STUDY SELECTION 
# Appendix B1. Number of included and excluded studies after the selection process
length(sort(unique(list$ID))) ## number of total studies with potential to be included
(length(sort(unique(list$ID)))-length(sort(unique(effectsize_logRR$ID_C)))) ## number of excluded studies
length(sort(unique(effectsize_logRR$ID_C))) ## number of total included studies
length(sort(unique(abundance_logRR$ID_C))) ## number of inclued studies for abundance studies
length(sort(unique(richness_logRR$ID_C))) ## number of inclueded studies for richness studies

# Appendix B3. Number of excluded studies and the reason of exclusion
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

excluded<- anti_join(list, effectsize_logRR, by= c("ID" ="ID_C"))%>%
  mutate(Title = firstup(Title))%>%
  mutate(Exclusion_reason_recla = if_else(Exclusion_reason == "Management system comparison only" | Exclusion_reason ==  "Effect on yield only" | Exclusion_reason == "Cropping system description only" | Exclusion_reason == "Compare one plant species in different settings", "Not compare biodiversity",
                                          if_else(Exclusion_reason == "No comparison" | Exclusion_reason == "Compare natural vs. monoculture" | Exclusion_reason == "Unsuitable reference system used", "Not compare simplifiedd vs diversified",
                                                  if_else(Exclusion_reason == "Compare forest plantations" | Exclusion_reason == "Natural land comparison only", "Forest plantation or natural land",
                                                          Exclusion_reason))))%>%
  ##Studies excluded because did not analysed Abundance or Richness
  mutate(Exclusion_reason_recla =if_else(ID == "1038" |ID == "107" |ID == "1135"|ID == "1137"|ID == "1142"|ID == "1150" |ID =="128"|ID ==  "136"|ID ==  "1361"|ID == "1393" |ID =="1398"|ID == "1505"|ID == "153" |ID == "204" |ID == "250" 
                                         |ID == "299" |ID == "356" |ID == "402" |ID == "426" |ID == "429"|ID ==  "430" |ID == "431"  |ID =="535"|ID ==  "544"|ID ==  "549" |ID == "55" |ID ==  "561" |ID == "586"|ID ==  "594" 
                                         |ID =="61" |ID ==  "690", "Not abundance or richness",
                                         ##Studies excluded becaused did not compared simplified vs diversified
                                         if_else(ID =="1153" | ID =="1168" |ID =="1287" |ID =="1391" |ID =="147" |ID == "151" |ID == "152" |ID == "169" |ID == "237"|ID ==  "274" |ID == "329"  |ID =="330"|ID ==  "331" |ID =="568" 
                                                 |ID == "573" |ID == "574" |ID == "583" |ID == "587" |ID == "606" |ID == "609" |ID == "611"  |ID =="613" |ID == "78"  |ID == "91" |ID == "244"|ID == "283"|ID == "287"| ID =="694"|ID =="697"|ID =="1073"|ID =="1076"|ID =="1083"|ID =="582","Not compare simplifiedd vs diversified",
                                                 ##Studies excluded becaused was not possible classify by functional group
                                                 if_else(ID =="1098" |ID =="290" |ID =="61" |ID =="693" |ID =="703"|ID == "125"|ID =="705"|ID =="365", "Not functional group",
                                                         ##Study excluded because of the lack of coordinate points
                                                         if_else(ID == "486", "Missing data", 
                                                                 ##Study excluded because the biodiversity mean was less than 0
                                                                 if_else(ID == "361", "Mean<0", Exclusion_reason_recla))))))%>%
  ##Put a different number to every exclusion reason
  mutate(exclusion_ID = if_else(Exclusion_reason_recla == "Unpublished studies", 1,
                                if_else(Exclusion_reason_recla == "Unavailable study", 2,
                                        if_else(Exclusion_reason_recla == "Not English", 3,
                                                if_else(Exclusion_reason_recla == "Secondary data", 4,
                                                        if_else(Exclusion_reason_recla == "It's a meta-analysis or review", 5,
                                                                if_else(Exclusion_reason_recla == "Qualitative ", 6,
                                                                        if_else(Exclusion_reason_recla == "Not compare biodiversity", 7,
                                                                                if_else(Exclusion_reason_recla == "Not abundance or richness",8,
                                                                                        if_else(Exclusion_reason_recla == "Not compare simplifiedd vs diversified", 9,
                                                                                                if_else(Exclusion_reason_recla == "Different Output ", 10,
                                                                                                        if_else(Exclusion_reason_recla =="Forest plantation or natural land", 11,
                                                                                                                if_else(Exclusion_reason_recla == "Not functional group", 12,
                                                                                                                        if_else(Exclusion_reason_recla =="No conducted in the field" , 13,
                                                                                                                                if_else(Exclusion_reason_recla == "Missing data", 14,
                                                                                                                                        if_else(Exclusion_reason_recla == "Irrelevant ", 15,
                                                                                                                                                if_else(Exclusion_reason_recla == "Mean<0", 16,
                                                                                                                                                        0)))))))))))))))))
addmargins(table(excluded$exclusion_ID)) #reason of exclusion

# Appendix B4.	List of excluded studies 
write.csv(excluded, "Excluded_studies_08.16.csv")

###### DATA EXTRACTION
# Appendix C4.	Details of the individual observations recorded and the responsible of recording them.
included<- data_MA%>%select(ID_T)%>%group_by(ID_T)%>%summarise()
data_2<- right_join(x=data, y=included, by=c("ID" = "ID_T"))#%>%filter(B_measure =="Species Richness")
length(unique(data_2$Lat)) #number of geographic points
addmargins(table(data_2$B_measure)) ##number of individual observations extracted by biodiversity measure
addmargins(table(data_2$Data_entry, data_2$B_measure)) #Recorded persons

###### DATA PREPARATION FOR THE ANALYSIS
#Before filtering variance = 0
length(which(data_MA$B_measure == "Abundance"))#number effect sizes for abundance
length(which(data_MA$B_measure == "Species Richness"))#number effect sizes for species richness

#After filtering variance = 0
length(abundance_logRR$B_measure)#number effect sizes for abundance
length(richness_logRR$B_measure)#number effect sizes for species richness


#---------------------------##########  META-ANALYSIS ################----------------------------------------------------#
#####Equation: Cheung (2014) Formula to calculate the estimate sampling variance (formula 14)####
#b= LRR_var
estimated.sampling.variance.func <- function (b) {  
  result<- ((length(b)-1) * sum(1/b))/ (((sum(1/b))^2)-(sum(1/(b^2))))
  return(result)
}

##########################################################################################################3
############################## A B U N D A N C E ######################################################################
###########################################################################################################3

########---- INTERCEPT ONLY MODEL -------###########
#Estimate the overall mean effect size by fitting an intercept-only model.
#See:  Lopez-Lopez et al. 2018; Assink and Wibbelink (2016)
#tdist=TRUE = the argument specifying that test statistics and confidence intervals must be based on the 
#t-distribution (Assink and Wibbelink 2016)
#For the methodology read Li, Y., Shi, L., & Roth, D. (1994) to see the problem of not use tdis .
#For the methodology read Knapp, G. & Hartung, J. (2003), they proposed to use tdist.
#LEVEL 2: ES_ID (the variable containing the unique identifiers of all ef- fect sizes in the data set)
#LEVEL 3: ID_C (study ID)
#RESULTS: k= number of ES comprised; REML(REstricted Maximum Likelihood estimation method): method is in some 
#ways superior to other methods (see, Hox, 2010; Viechtbauer, 2005), but has restrictions (see Cheung, 2014; Van den Noortgate et al., 2013).
#RESULTS: sigma^2.1 (estim): variance between ES within studies (level 2) if it is small indicates that the ES are similar within studies (Cheung 2014); 
#sigma^2.2(estim): variance between studies (level 3): if it is large, indicates the population effect sizes vary across Level 3, so study characteristics can be included to explain the heterogeneity at level 3 (Cheung 2014)
#RESULTS: Test for heterogeneity: p-val<0.001 significant variation between all effect sizes in the data set (However, these results are not very informative, as we are interested in within-study variance (level 2) as well as between- study variance (level 3) and not in variance between all effect sizes in the data set.)
#RESULTS: estimate = the overall effect size; se = standard error; tval = t value; pval = p value; ci.lb = lower bound of the confidence in- terval; and ci.ub = upper bound of the confidence in- terval.
abun.overall <- rma.mv(y= LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                       tdist= TRUE, data=abundance_logRR, method="REML")
summary(abun.overall, digits=3)

##Sensitivity analysis
#Standardized residuals
rs.abun.overall<- rstandard(abun.overall)
rs.abun.overall
rs.abun.overall<- as.data.frame(rs.abun.overall)
rs.abun.overall$ES_ID <- as.numeric(1:nrow(rs.abun.overall)) #add a new column with the effect size ID number
#Hat values
hat.abun.overall<- hatvalues.rma.mv(abun.overall)
hat.abun.overall<- as.data.frame(hat.abun.overall)
hat.abun.overall$ES_ID <- as.numeric(1:nrow(hat.abun.overall)) #add a new column with the effect size ID number
names(hat.abun.overall)
#Plot hat values agains residual values
plot(x=hat.abun.overall$hat.abun.overall, y= rs.abun.overall$resid, 
     ylab= "Standardized residuals", xlab= "Hat values", main= "A")
abline(v = 0.0027, lty=2, lwd=2, col="grey50") #0.027 estimate 0.071;
abline(h = 5, lty=2, lwd=2, col="grey50")
abline(h = -5, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the abundance database
abundance_logRR_sensitivity.overall<- left_join(hat.abun.overall,rs.abun.overall, by="ES_ID")%>%
  left_join(abundance_logRR, by ="ES_ID")%>%
  filter(hat.abun.overall<0.0027)%>%
  filter(resid < 5)%>%
  filter(resid >-5)%>%
  select(!resid & !hat.abun.overall& !resid&!se&!z)

##Meta-analysis model without outlier effect sizes
abun.overall.sensitivity <- rma.mv(y= LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                   tdist= TRUE, data=abundance_logRR_sensitivity.overall, method="REML")
summary(abun.overall.sensitivity, digits=3)
summary(abun.overall, digits=3)

#####Heterogeneity of within-study variance (level 2)###
#Build a two-level model without within-study variance.
#If the test results provide support for rejecting the null hypothesis, we can conclude that the fit of the 
#original three-level model is statistically better than the fit of the two-level model, and consequently, that 
#there is significant variability between effect sizes within studies.
#sigma2=c(0,NA) = the argument is taken by the rma.mv function when the user wants to fix a specific 
#variance component to a user-defined value. The first parameter (0) states that the within-study variance is 
#fixed to zero (i.e., no within-study variance will be modeled), and the second parameter (NA) states that the 
#between-study variance is estimated.
#The variance at the first level (ÃÂÃÂ¡ÃÂÃÂ¹ÃÂÃÂ½) was not included in the model, because it is assumed to be known.
abun.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), sigma2=c(0,NA), tdist=TRUE, 
                           data=abundance_logRR, method="REML")
summary(abun.modelnovar2)

# Perform a likelihood-ratio-test to determine the
# significance of the within-study variance (level2).
#RESULTS: Full= represents the three-level model stored in the object abun.overall; 
#Reduced= represents the two- level model stored in the object abun.modelnovar2
#df = degrees of freedom= The reduced model has one degree less than the full model, since within-study vari- ance is not present in the reduced model;
#LRT = likelihood-ratio test. In this column, the value of the test statistic is presented;
#pval = the two-sided p value of the test statistic
#QE= resembles the test for heterogeneity in all effect sizes in the data set, and the value of the test statistic is given in this column
#CONCLUSION: LRT<pval = I found significant variability between effect sizes within studies
anova(abun.overall,abun.modelnovar2) #ABUNDANCE

###Heterogeneity of between-study variance (level 3)
# Build a two-level model without between-study variance;
#In this last model, between-study variance is not modeled. 
#If the null hypothesis should be rejected based on the test results, we can conclude that the fit of the 
#original three-level model is statistically better than the fit of the two-level model, and consequently, 
#that there is significant variability between studies.
#sigma2=c(NA,0): Since we want to fix the between-study variance to zero and freely estimate the within-study variance
abun.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C),
                           sigma2=c(NA,0), tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.modelnovar3)

# Perform a likelihood-ratio-test to determine the significance of the between-study variance.
#CONCLUSION: LRT<pval = I found significant variability between studies
anova(abun.overall,abun.modelnovar3) #ABUNDANCE

#CONCLUSION: In our database, there is significant within-study variance (at level 2) as well as significant 
#between-study variance (at level 3). This implies that there is more variability in effect sizes (within and 
#between studies) than may be expected based on sampling variance alone. Therefore, moderator analyses can be 
#performed in order to examine variables that may explain within- and/or between-study variance. However, before 
#turning to moderator analyses, we will first examine how the total variance is distributed over the three levels 
#of the meta-analytic model.

###The distribution of the variance over the three levels of the meta-analytic model
#Recall that three different sources of variance are modeled in our meta-analytic model: sampling variance at the
#first level; within-study variance at the second level; and between-study variance at the third level.
#To determine how much variance can be attributed to differences between effect sizes within studies (level 2)
#and to differences between studies (level 3), formulas given by Cheung (2014 - formula 14 on page 2015) can be used
# Determining how the total variance is distributed over the three levels of the meta-analytic model;
# Print the results in percentages on screen.
abun.estimated.sampling.variance<- estimated.sampling.variance.func(abundance_logRR$LRR_var)

###Each of the three variance components (I2_1, I2_2, I2_3) is divided by the total amount of variance, 
###so that a proportional estimate of each variance component is stored in an object.
###overall$sigma2[1]: refers to the amount of within-study variance in the object abun.overall 
###overall$sigma2[2]: refers to the amount of between-study variance in the object abun.overall
###The proportional estimates of the three variance com- ponents are multiplied by 100 (%), so that a percentage 
#estimate of each variance component is stored in an object

#Sampling variance (Amount of variance at level 1)
((abun.estimated.sampling.variance)/(abun.overall$sigma2[1]+abun.overall$sigma2[2]+abun.estimated.sampling.variance))*100

#Within-study variance (Amount of variance at level 2)
((abun.overall$sigma2[1]) / (abun.overall$sigma2[1] + abun.overall$sigma2[2] + abun.estimated.sampling.variance))*100

#Between-study variance (Amount of variance at level 3)
((abun.overall$sigma2[2]) / (abun.overall$sigma2[1] + abun.overall$sigma2[2] + abun.estimated.sampling.variance))*100

#CONCLUSION: If less than 75% of the total amount of variance is attributed to sampling variance (at level 1), 
#it would be fruitful to examine the potential moderating effect of study and/or effect size characteristics on 
#the over- all effect. In our database,less than 1% of the total amount of variance could be attributed to 
#sampling variance (see abun.I2_1), and based on the rule of Hunter and Schmidt (1990), we can once again 
#conclude that there is substantial variation between effect sizes within studies and/or between studies, 
#making it relevant to perform moderator analyses.

#-------------------------##########  META-REGRESSION (MODERATOR: FUNCTIONAL GROUPS) ################----------------------------------------------------#
###Because the variance within and between studies was substantial, we have to fit a moderator analysis
### Determine the potential moderating effect of functional group;
# Autotrophs is chosen as the reference category;
#------- ABUNDANCE
abun.FG <- rma.mv(y=LRR, V=LRR_var, mods = ~ FG_recla, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                  tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.FG, digits=3)

##Sensitivity analysis
#Standardized residuals
rs.abun.FG<- rstandard(abun.FG)
rs.abun.FG<- as.data.frame(rs.abun.FG)
rs.abun.FG$ES_ID <- as.numeric(1:nrow(rs.abun.FG)) #add a new column with the effect size ID number

#Hat values
hat.abun.FG<- hatvalues.rma.mv(abun.FG)
hat.abun.FG<- as.data.frame(hat.abun.FG)
hat.abun.FG$ES_ID <- as.numeric(1:nrow(hat.abun.FG)) #add a new column with the effect size ID number

#Plot hat values agains residual values
plot(x=hat.abun.FG$hat.abun.FG, y= rs.abun.FG$resid, 
     ylab= "Standardized residuals", xlab= "Hat values", main= "B")
abline(v = 0.1, lty=2, lwd=2, col="grey50") #0.027 estimate 0.071;
abline(h = 5, lty=2, lwd=2, col="grey50")
abline(h = -5, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the abundance database
abundance_logRR_sensitivity.FG<- left_join(hat.abun.FG,rs.abun.FG, by="ES_ID")%>%
  left_join(abundance_logRR, by ="ES_ID")%>%
  filter(hat.abun.FG<0.1)%>%
  filter(resid < 5)%>%
  filter(resid >-5)%>%
  select(!resid & !hat.abun.FG& !resid&!se&!z)

# Meta-analysis model without outlier effect sizes
abun.FG.sensitivity <- rma.mv(y=LRR, V=LRR_var, mods = ~ FG_recla, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                              tdist=TRUE, data=abundance_logRR_sensitivity.FG, method="REML")
summary(abun.FG.sensitivity, digits=3)
summary(abun.FG, digits=3)

#Heterogeneity analysis
#Heterogeneity: within-study variance (level 2)###
#ABUNDANCE
abun.FG.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, mods = ~ FG_recla, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                              sigma2=c(0,NA), tdist=TRUE,data=abundance_logRR, method="REML")
summary(abun.FG.modelnovar2)

# Perform a likelihood-ratio-test to determine the significance of the within-study variance.
anova(abun.FG,abun.FG.modelnovar2)

###Heterogeneity of between-study variance (level 3)
# Build a two-level model without between-study variance;
abun.FG.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, mods = ~ FG_recla, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                              sigma2=c(NA,0),tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.FG.modelnovar3)

#Perform a likelihood-ratio-test to determine the significance of the between-study variance.
anova(abun.FG, abun.FG.modelnovar3)

###The distribution of the variance over the three levels of the meta-analytic model
abun.FG.estimated.sampling.variance<- estimated.sampling.variance.func(abundance_logRR$LRR_var)

#Sampling variance (Amount of variance at level 1)
((abun.FG.estimated.sampling.variance)/(abun.FG.autotrophs$sigma2[1]+abun.FG.autotrophs$sigma2[2]+abun.FG.estimated.sampling.variance))*100
#Within-study variance (Amount of variance at level 2) (Functional groups)
((abun.FG$sigma2[1]) / (abun.FG$sigma2[1] + abun.FG$sigma2[2] + abun.FG.estimated.sampling.variance))*100
#Between-study variance (Amount of variance at level 3) (Studies ID)
((abun.FG$sigma2[2]) / (abun.FG$sigma2[1] + abun.FG$sigma2[2] + abun.FG.estimated.sampling.variance))*100

#------- PLOT RESULTS INTERCEPT ONLY MODEL AND META-REGRESSION (MODERATORS = Functional groups)
#---- RESULTS (BEFORE SENSITIVITY ANALYSIS)
abun.FG.out.int<-rma.mv(y=LRR, V=LRR_var, mods = ~ (FG_recla)-1, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                        tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.FG.out.int, digits=3)

#PLOT EFFECT SIZES ABUNDANCE AND FUNCTIONAL GROUPS
abun.FG_studies_ES<- abundance_logRR %>%
  group_by(FG_recla)%>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))%>%
  mutate(FG = c("Autotrophs","Decomposers", "Natural enemies", "Others", "Pests", "Pollinators"))

abun.FG.comb<- coef(summary(abun.FG.out.int))%>%
  mutate(FG = c("Autotrophs","Decomposers", "Natural enemies", "Others", "Pests", "Pollinators"),
         FG = as.factor(FG))%>%
  right_join(y=abun.FG_studies_ES, by="FG")
abun.FG.comb

abun.overall_studies_ES <- abundance_logRR%>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))%>%
  mutate(FG = c("Overall"))

abun.overall.comb<- coef(summary(abun.overall))%>%
  mutate(FG_recla = c("Summary"),
         FG = c("Overall"),
         FG= as.factor(FG))%>%
  right_join(y=abun.overall_studies_ES, by="FG")

abun.comb<- rbind(abun.FG.comb,abun.overall.comb)%>%
  rename(ES_LRR = estimate, 
         SE = se)%>%
  mutate(ES_percent = (100*(exp(ES_LRR)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         ES_LRR = round(ES_LRR, digits = 2),
         SE = round(SE, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =0),
         ci.lb_percent = round(ci.lb_percent, digits = 0),
         ci.ub_percent = round(ci.ub_percent, digits = 0))%>%
  select("FG", "ES_LRR", "SE","ci.lb","ci.ub","tval" ,"pval", "n_studies","n_effectsizes","ES_percent",
         "ci.lb_percent", "ci.ub_percent")

abun.comb.graph<- abun.comb%>%
  add_row(ES_LRR = c(NA), SE = c(NA), tval= c(NA), pval= c(NA), ci.lb= c(NA), ci.ub= c(NA),
          FG= c(NA), ES_percent= c(NA), ci.lb_percent = c(NA), ci.ub_percent= c(NA), .before = 1)%>%
  mutate(ID = row_number())
abun.comb.graph

abun.tabletext<-abun.comb%>%
  mutate(ES_LRR = paste(ES_LRR, " ","(", ES_percent, ")", sep=""),
         ci.lb = paste(ci.lb, " ","(", ci.lb_percent, ")", sep=""),
         ci.ub = paste(ci.ub, " ","(", ci.ub_percent, ")", sep=""),
         SE = as.character(SE),
         tval = as.character(tval),
         pval = as.character(pval),
         n_studies = as.character(n_studies),
         n_effectsizes = as.character(n_effectsizes),
         pval= if_else(pval == "0", "<0.001", pval),
         pval = if_else(FG == "Pests"| FG == "Decomposers"|
                          FG =="Pollinators", paste(pval, "**", sep= ""),pval))%>%
  select(FG,ES_LRR, SE ,ci.lb, ci.ub, tval, pval,n_studies, n_effectsizes)%>%
  add_row(FG= c("Functional groups"),ES_LRR = c("ES"), SE = c("SE"), tval= c("t-value"), 
          pval= c("p-value"), ci.lb= c("LL"), ci.ub= c("UL"), n_studies= c("# Studies"), 
          n_effectsizes =c("# ES"), .before = 1)%>%
  mutate(FG = c("Functional groups","Autotrophs","Decomposers", "Natural enemies",
                "Others", "Pests", "Pollinators",  "Overall"))
abun.tabletext

##Forest plot figure PAPER
forestplot(abun.tabletext, mean= abun.comb.graph$ES_percent, lower= abun.comb.graph$ci.lb_percent, 
           upper= abun.comb.graph$ci.ub_percent, graph.pos=2,
           new_page = TRUE, is.summary = c(TRUE,rep(FALSE,6), TRUE),  
           xlab = "% Effect size (ÃÂ±95% CI)", clip = c(-30,200),
           ci.vertices = TRUE, boxsize= 0.3,
           col= fpColors(box="#22211d", line="#22211d", summary="#686D35", zero ="gray50"),
           txt_gp = fpTxtGp(label = gpar(fontfamily = "sans", col= "black", cex = 1.4),
                            ticks= gpar(fontfamily = "sans", cex = 1.1, col= "black"),
                            xlab= gpar(fontfamily = "sans", cex = 1.3, col= "black")),
           hrzl_lines = list("2" = gpar(lwd=2, col = "black"),
                             "8" = gpar(lwd=2, col = "black")),
           vertices=TRUE, align = c("l",rep("c",8)),
           colgap = unit(3.5,"mm"),
           xticks = c(-50, -25, 0, 25, 50,  100, 150, 200),
           lineheight=unit(1.6,'cm'),
           graphwidth = unit(6.2, "cm"))

#---- PLOT ABUNDANCE: RESULTS FROM SENSITIVITY ANALYSIS)
abun.FG.out.int.sensitivity<-rma.mv(y=LRR, V=LRR_var, mods = ~ (FG_recla)-1, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                    tdist=TRUE, data=abundance_logRR_sensitivity.FG, method="REML")
summary(abun.FG.out.int.sensitivity, digits=3)
summary(abun.FG.out.int, digits=3)

#Plot data
abun.FG_studies_ES.sensitivity<- abundance_logRR_sensitivity.FG %>%
  group_by(FG_recla)%>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))%>%
  mutate(FG = c("Autotrophs","Decomposers", "Natural enemies", "Others", "Pests", "Pollinators"))

abun.FG.comb.sensitivity<- coef(summary(abun.FG.out.int.sensitivity))%>%
  mutate(FG = c("Autotrophs","Decomposers", "Natural enemies", "Others", "Pests", "Pollinators"),
         FG = as.factor(FG))%>%
  right_join(y=abun.FG_studies_ES.sensitivity, by="FG")
abun.FG.comb.sensitivity

abun.overall_studies_ES.sensitivity <- abundance_logRR_sensitivity.FG%>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))%>%
  mutate(FG = c("Overall"))

abun.overall.comb.sensitivity<- coef(summary(abun.overall.sensitivity))%>%
  mutate(FG_recla = c("Summary"),
         FG = c("Overall"),
         FG= as.factor(FG))%>%
  right_join(y=abun.overall_studies_ES.sensitivity, by="FG")
abun.overall.comb.sensitivity

abun.comb.sensitivity<- rbind(abun.FG.comb.sensitivity,abun.overall.comb.sensitivity)%>%
  rename(ES_LRR = estimate, 
         SE = se)%>%
  mutate(ES_percent = (100*(exp(ES_LRR)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         ES_LRR = round(ES_LRR, digits = 2),
         SE = round(SE, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =0),
         ci.lb_percent = round(ci.lb_percent, digits = 0),
         ci.ub_percent = round(ci.ub_percent, digits = 0))%>%
  select("FG", "ES_LRR", "SE","ci.lb","ci.ub","tval" ,"pval", "n_studies","n_effectsizes","ES_percent",
         "ci.lb_percent", "ci.ub_percent")

abun.comb.graph.sensitivity<- abun.comb.sensitivity%>%
  add_row(ES_LRR = c(NA), SE = c(NA), tval= c(NA), pval= c(NA), ci.lb= c(NA), ci.ub= c(NA),
          FG= c(NA), ES_percent= c(NA), ci.lb_percent = c(NA), ci.ub_percent= c(NA), .before = 1)%>%
  mutate(ID = row_number())
abun.comb.graph.sensitivity

abun.tabletext.sensitivity<-abun.comb.sensitivity%>%
  mutate(ES_LRR = paste(ES_LRR, " ","(", ES_percent, ")", sep=""),
         ci.lb = paste(ci.lb, " ","(", ci.lb_percent, ")", sep=""),
         ci.ub = paste(ci.ub, " ","(", ci.ub_percent, ")", sep=""),
         SE = as.character(SE),
         tval = as.character(tval),
         pval = as.character(pval),
         n_studies = as.character(n_studies),
         n_effectsizes = as.character(n_effectsizes),
         pval= if_else(pval == "0", "<0.001", pval),
         pval = if_else(FG == "Natural enemies"| FG == "Pests"| FG == "Decomposers"|
                          FG =="Pollinators", paste(pval, "**", sep= ""),pval))%>%
  select(FG,ES_LRR, SE ,ci.lb, ci.ub, tval, pval,n_studies, n_effectsizes)%>%
  add_row(FG= c("Functional groups"),ES_LRR = c("ES"), SE = c("SE"), tval= c("t-value"), 
          pval= c("p-value"), ci.lb= c("LL"), ci.ub= c("UL"), n_studies= c("# Studies"), 
          n_effectsizes =c("# ES"), .before = 1)%>%
  mutate(FG = c("Functional groups","Autotrophs","Decomposers", "Natural enemies",
                "Others", "Pests", "Pollinators",  "Overall"))
abun.tabletext.sensitivity

##Forest plot
forestplot(abun.tabletext.sensitivity, mean= abun.comb.graph.sensitivity$ES_percent, lower= abun.comb.graph.sensitivity$ci.lb_percent, 
           upper= abun.comb.graph.sensitivity$ci.ub_percent, graph.pos=2,
           new_page = TRUE, is.summary = c(TRUE,rep(FALSE,6), TRUE),  
           xlab = "% Effect size (ÃÂ±95% CI)", clip = c(-30,200),
           ci.vertices = TRUE, boxsize= 0.3,
           col= fpColors(box="#22211d", line="#22211d", summary="#686D35", zero ="gray50"),
           txt_gp = fpTxtGp(label = gpar(fontfamily = "sans", col= "black", cex = 1.4),
                            ticks= gpar(fontfamily = "sans", cex = 1.1, col= "black"),
                            xlab= gpar(fontfamily = "sans", cex = 1.3, col= "black")),
           hrzl_lines = list("2" = gpar(lwd=2, col = "black"),
                             "8" = gpar(lwd=2, col = "black")),
           vertices=TRUE, align = c("l",rep("c",8)),
           colgap = unit(3.5,"mm"),
           xticks = c(-50, -25, 0, 25, 50,  100, 150, 200),
           lineheight=unit(1.6,'cm'),
           graphwidth = unit(6.2, "cm"))

#-------------------  META-REGRESSION (MODERATORS = LANDSCAPE METRICS) ----------------------------------------------------#
# See step by step in https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/multiple-meta-regression.html
# Determine the potential moderating effect of the assessed landscape metrics 

#---------------  MODERATOR = % OF NATURAL OR SEMI-NATURAL HABITATS
abun.natural_percentage <- rma.mv(y=LRR, V=LRR_var, mods = ~ natural_percentage_mean, random = 
                                    list(~ 1 | ES_ID, ~ 1 | ID_C),tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.natural_percentage, digits=3)

#Results meta-regression
coef(summary(abun.natural_percentage))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),estimate = round(estimate, digits = 3),
         se = round(se, digits =3),tval= round(tval, digits =3),pval= round(pval, digits =3),
         ci.lb = round(ci.lb, digits =3),ci.ub = round(ci.ub, digits =3),
         ES_percent =round(ES_percent, digits =1),ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

##Sensitivity analysis
#Standardized residuals
rs.abun.natural_percentage<- rstandard(abun.natural_percentage)
rs.abun.natural_percentage<- as.data.frame(rs.abun.natural_percentage)
rs.abun.natural_percentage$ES_ID <- as.numeric(1:nrow(rs.abun.natural_percentage)) #add a new column with the effect size ID number
#Hat values
hat.abun.natural_percentage<- hatvalues.rma.mv(abun.natural_percentage)
hat.abun.natural_percentage<- as.data.frame(hat.abun.natural_percentage)
hat.abun.natural_percentage$ES_ID <- as.numeric(1:nrow(hat.abun.natural_percentage)) #add a new column with the effect size ID number
#Plot hat values agains residual values
plot(x=hat.abun.natural_percentage$hat.abun.natural_percentage, y= rs.abun.natural_percentage$resid, 
     ylab= "Standardized residuals", xlab= "Hat values", main= "A")
abline(v = 0.01, lty=2, lwd=2, col="grey50")
abline(h = 6, lty=2, lwd=2, col="grey50")
abline(h = -6, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the abundance database
abundance_logRR_sensitivity.natural_percentage<- left_join(hat.abun.natural_percentage,rs.abun.natural_percentage, by="ES_ID")%>%
  left_join(abundance_logRR, by ="ES_ID")%>%
  filter(hat.abun.natural_percentage<0.01)%>%
  filter(resid < 6)%>%
  filter(resid >-6)%>%
  select(!resid & !hat.abun.natural_percentage& !resid&!se&!z)

##Meta-analysis model without outlier effect sizes
abun.natural_percentage.sensitivity <- rma.mv(y=LRR, V=LRR_var, mods = ~ natural_percentage_mean, random = 
                                                list(~ 1 | ES_ID, ~ 1 | ID_C),tdist=TRUE, 
                                              data=abundance_logRR_sensitivity.natural_percentage, method="REML")
summary(abun.natural_percentage.sensitivity, digits=3)
summary(abun.natural_percentage, digits=3)

#Results meta-regression (after sensitivity analysis)
coef(summary(abun.natural_percentage.sensitivity))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),estimate = round(estimate, digits = 3),
         se = round(se, digits =3),tval= round(tval, digits =3),pval= round(pval, digits =3),
         ci.lb = round(ci.lb, digits =3),ci.ub = round(ci.ub, digits =3),
         ES_percent =round(ES_percent, digits =1),ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

##Heterogeneity analysis
#Heterogeneity of within-study variance (level 2)###
abun.natural.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), sigma2=c(0,NA), tdist=TRUE, 
                                   data=abundance_logRR, method="REML", mods = ~ natural_percentage_mean)
summary(abun.natural.modelnovar2)

# Perform a likelihood-ratio-test to determine the significance of the within-study variance (level2).
anova(abun.natural_percentage,abun.natural.modelnovar2) #ABUNDANCE

###Heterogeneity of between-study variance (level 3)
abun.natural.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C),
                                   sigma2=c(NA,0), tdist=TRUE, data=abundance_logRR, method="REML", mods = ~ natural_percentage_mean)
summary(abun.natural.modelnovar3)

# Perform a likelihood-ratio-test to determine the significance of the between-study variance.
anova(abun.natural_percentage, abun.natural.modelnovar3) #ABUNDANCE

###The distribution of the variance over the three levels of the meta-analytic model
abun.natural.estimated.sampling.variance<- estimated.sampling.variance.func(abundance_logRR$LRR_var)

#Sampling variance (Amount of variance at level 1)
((abun.natural.estimated.sampling.variance)/(abun.natural_percentage$sigma2[1]+abun.natural_percentage$sigma2[2]+abun.natural.estimated.sampling.variance))*100
#Within-study variance (Amount of variance at level 2)
((abun.natural_percentage$sigma2[1]) / (abun.natural_percentage$sigma2[1] + abun.natural_percentage$sigma2[2] + abun.natural.estimated.sampling.variance))*100
#Between-study variance (Amount of variance at level 3)
((abun.natural_percentage$sigma2[2]) / (abun.natural_percentage$sigma2[1] + abun.natural_percentage$sigma2[2] + abun.natural.estimated.sampling.variance))*100

#---------------  MODERATOR = % OF AGRICULTURAL AREAS
## META-REGRESSION MODEL: using % of agricultural areas:
abun.arable_percentage <- rma.mv(y=LRR, V=LRR_var, mods = ~ arable_percentage_mean, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                 tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.arable_percentage, digits=5)

##Prueba por FG
abun.arable_percentage.FG <- rma.mv(y=LRR, V=LRR_var, mods = ~ (arable_percentage_mean*FG_recla), random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                    tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.arable_percentage.FG, digits=5)

#Results meta-regression
coef(summary(abun.arable_percentage))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),estimate = round(estimate, digits = 3),
         se = round(se, digits =3),tval= round(tval, digits =3),pval= round(pval, digits =3),
         ci.lb = round(ci.lb, digits =3),ci.ub = round(ci.ub, digits =3),
         ES_percent =round(ES_percent, digits =1),ci.lb_percent = round(ci.lb_percent, digits = 0),
         ci.ub_percent = round(ci.ub_percent, digits = 3))

##Sensitivity analysis
#Standardized residuals
rs.abun.arable_percentage<- rstandard(abun.arable_percentage)
rs.abun.arable_percentage<- as.data.frame(rs.abun.arable_percentage)
rs.abun.arable_percentage$ES_ID <- as.numeric(1:nrow(rs.abun.arable_percentage)) #add a new column with the effect size ID number
#Hat values
hat.abun.arable_percentage<- hatvalues.rma.mv(abun.arable_percentage)
hat.abun.arable_percentage<- as.data.frame(hat.abun.arable_percentage)
hat.abun.arable_percentage$ES_ID <- as.numeric(1:nrow(hat.abun.arable_percentage)) #add a new column with the effect size ID number
#Plot hat values against residual values
plot(x=hat.abun.arable_percentage$hat.abun.arable_percentage, y= rs.abun.arable_percentage$resid, 
     ylab= "Standardized residuals", xlab= "Hat values", main= "C")
abline(v = 0.0085, lty=2, lwd=2, col="grey50")
abline(h = 4, lty=2, lwd=2, col="grey50")
abline(h = -4, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the abundance database
abundance_logRR_sensitivity.arable_percentage<- left_join(hat.abun.arable_percentage,rs.abun.arable_percentage, by="ES_ID")%>%
  left_join(abundance_logRR, by ="ES_ID")%>%
  filter(hat.abun.arable_percentage<0.0085)%>%
  filter(resid < 4)%>%
  filter(resid >-4)%>%
  select(!resid & !hat.abun.arable_percentage& !resid&!se&!z)

##Meta-analysis model without outlier effect sizes
abun.arable_percentage.sensitivity <- rma.mv(y=LRR, V=LRR_var, mods = ~ arable_percentage_mean, 
                                             random = list(~ 1 | ES_ID, ~ 1 | ID_C), tdist=TRUE, 
                                             data=abundance_logRR_sensitivity.arable_percentage,method="REML")
summary(abun.arable_percentage.sensitivity, digits=3)
summary(abun.arable_percentage, digits=3)

#Results meta-regression after sensitivity analysis
coef(summary(abun.arable_percentage.sensitivity))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),estimate = round(estimate, digits = 3),
         se = round(se, digits =3),tval= round(tval, digits =3),pval= round(pval, digits =3),
         ci.lb = round(ci.lb, digits =3),ci.ub = round(ci.ub, digits =3),
         ES_percent =round(ES_percent, digits =1),ci.lb_percent = round(ci.lb_percent, digits = 3),
         ci.ub_percent = round(ci.ub_percent, digits = 3))

##Heterogeneity analysis
#Heterogeneity of within-study variance (level 2)###
abun.arable.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), sigma2=c(0,NA), tdist=TRUE, 
                                  data=abundance_logRR, method="REML", mods = ~ arable_percentage_mean)
summary(abun.arable.modelnovar2)

# Perform a likelihood-ratio-test to determine the significance of the within-study variance (level2).
anova(abun.arable_percentage,abun.arable.modelnovar2) #ABUNDANCE

###Heterogeneity of between-study variance (level 3)
abun.arable.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C),
                                  sigma2=c(NA,0), tdist=TRUE, data=abundance_logRR, method="REML", 
                                  mods = ~ arable_percentage_mean)
summary(abun.arable.modelnovar3)

# Perform a likelihood-ratio-test to determine the significance of the between-study variance.
anova(abun.arable_percentage, abun.arable.modelnovar3) #ABUNDANCE

###The distribution of the variance over the three levels of the meta-analytic model
abun.arable.estimated.sampling.variance<- estimated.sampling.variance.func(abundance_logRR$LRR_var)

#Sampling variance (Amount of variance at level 1)
((abun.arable.estimated.sampling.variance)/(abun.arable_percentage$sigma2[1]+abun.arable_percentage$sigma2[2]+abun.arable.estimated.sampling.variance))*100
#Within-study variance (Amount of variance at level 2)
((abun.arable_percentage$sigma2[1]) / (abun.arable_percentage$sigma2[1] + abun.arable_percentage$sigma2[2] + abun.arable.estimated.sampling.variance))*100
#Between-study variance (Amount of variance at level 3)
((abun.arable_percentage$sigma2[2]) / (abun.arable_percentage$sigma2[1] + abun.arable_percentage$sigma2[2] + abun.arable.estimated.sampling.variance))*100

#---------------  MODERATOR = DISTANCE TO NATURAL HABITATS
## META-REGRESSION MODEL: using min_distance_mean in meters as moderator:
abun.distance <- rma.mv(y=LRR, V=LRR_var, mods = ~min_distance_mean, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                        tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.distance, digits=5)

#Calculate the residuals
rs.abun.distance<-rstandard.rma.mv(abun.distance, type="rstandard")

#Calculate predictor values for the model
preds.abun.distance <-predict(abun.distance,newmods=c(0:1000), addx=TRUE)
preds.abun.distance<-as.data.frame(preds.abun.distance)

## META-REGRESSION MODEL: using min_log_distance_mean (ln(distance+1)) as moderator :
abun.log_distance <- rma.mv(y=LRR, V=LRR_var, mods = ~min_log_distance_mean, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                            tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.log_distance, digits=5)

#Calulate the residuals from the metaregression model using distance in log
rs.abun.log_distance<-rstandard.rma.mv(abun.log_distance, type="rstandard")

#Calculate predictor values for the model
preds.abun.log_distance <-predict(abun.log_distance,newmods=c(0:7), addx=TRUE)
preds.abun.log_distance<-as.data.frame(preds.abun.log_distance)

#Plot residuals vs. distance in meters
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))#two plots in the same graph
plot(x=abundance_logRR$min_distance_mean, y=rs.abun.distance$resid,
     ylab="Standardized residuals", xlab= "Distance (m)", main = "A")
lines(x=preds.abun.distance$X.min_distance_mean ,y=preds.abun.distance$pred, col="#686D35", lwd=2)
#Plot residuals vs. distance (ln(distance+1))
plot(abundance_logRR$min_log_distance_mean, rs.abun.log_distance$resid,
     ylab="Standardized residuals", xlab= "ln(Distance + 1)", main = "B")
lines(x=preds.abun.log_distance$X.min_log_distance_mean ,y=preds.abun.log_distance$pred, col="#686D35", lwd=2)
mtext("Abundance", outer = TRUE, cex = 2)

##Sensitivity analysis
#Standardized residuals
rs.abun.log_distance<- as.data.frame(rs.abun.log_distance)
rs.abun.log_distance$ES_ID <- as.numeric(1:nrow(rs.abun.log_distance)) #add a new column with the effect size ID number
#Hat values
hat.abun.log_distance<- hatvalues.rma.mv(abun.log_distance)
hat.abun.log_distance<- as.data.frame(hat.abun.log_distance)
hat.abun.log_distance$ES_ID <- as.numeric(1:nrow(hat.abun.log_distance)) #add a new column with the effect size ID number
#Plot hat values against residual values
par(mfrow=c(1,1))
plot(x=hat.abun.log_distance$hat.abun.log_distance, y= rs.abun.log_distance$resid, 
     ylab= "Standardized residuals", xlab= "Hat values", main= "A")
abline(v = 0.011, lty=2, lwd=2, col="grey50")
abline(h = 5, lty=2, lwd=2, col="grey50")
abline(h = -5, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the abundance database
abundance_logRR_sensitivity.log_distance<- left_join(hat.abun.log_distance,rs.abun.log_distance, by="ES_ID")%>%
  left_join(abundance_logRR, by ="ES_ID")%>%
  filter(hat.abun.log_distance<0.011)%>%
  filter(resid < 5)%>%
  filter(resid >-5)%>%
  select(!resid & !hat.abun.log_distance& !resid&!se&!z)

##Meta-analysis model without outlier effect sizes
abun.log_distance.sensitivity <- rma.mv(y=LRR, V=LRR_var, mods = ~ arable_percentage_mean, 
                                        random = list(~ 1 | ES_ID, ~ 1 | ID_C), tdist=TRUE, 
                                        data=abundance_logRR_sensitivity.log_distance,method="REML")
summary(abun.log_distance.sensitivity, digits=4)
summary(abun.log_distance, digits=3)

##Heterogeneity analysis
#Heterogeneity of within-study variance (level 2)###
abun.log_distance.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), sigma2=c(0,NA), tdist=TRUE, 
                                        data=abundance_logRR, method="REML", mods = ~ min_log_distance_mean)
summary(abun.log_distance.modelnovar2)

# Perform a likelihood-ratio-test to determine the significance of the within-study variance (level2).
anova(abun.log_distance,abun.log_distance.modelnovar2) 

###Heterogeneity of between-study variance (level 3)
abun.log_distance.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C),
                                        sigma2=c(NA,0), tdist=TRUE, data=abundance_logRR, method="REML", 
                                        mods = ~ min_log_distance_mean)
summary(abun.log_distance.modelnovar3)

# Perform a likelihood-ratio-test to determine the significance of the between-study variance.
anova(abun.log_distance, abun.log_distance.modelnovar3)

###The distribution of the variance over the three levels of the meta-analytic model
abun.log_distance.estimated.sampling.variance<- estimated.sampling.variance.func(abundance_logRR$LRR_var)

#Sampling variance (Amount of variance at level 1)
((abun.log_distance.estimated.sampling.variance)/(abun.log_distance$sigma2[1]+abun.log_distance$sigma2[2]+abun.log_distance.estimated.sampling.variance))*100
#Within-study variance (Amount of variance at level 2)
((abun.log_distance$sigma2[1]) / (abun.log_distance$sigma2[1] + abun.log_distance$sigma2[2] + abun.log_distance.estimated.sampling.variance))*100
#Between-study variance (Amount of variance at level 3)
((abun.log_distance$sigma2[2]) / (abun.log_distance$sigma2[1] + abun.log_distance$sigma2[2] + abun.log_distance.estimated.sampling.variance))*100

##----Plot LANDSCAPE ANALYSIS
library(cowplot)
#https://rstudio-pubs-static.s3.amazonaws.com/93611_716b9b330cd54c0196916d6c76052bdc.html
preds.abun.arable <-predict(abun.arable_percentage,newmods=c(0,25,50,75,100,105), addx=TRUE)
preds.abun.arable<-as.data.frame(preds.abun.arable)
preds.abun.arable

abundance_logRR_plot<- abundance_logRR%>%filter(LRR>=-4)%>%filter(LRR<4)
summary(abun.arable_percentage, digits= 5)

abun.arable.summary<- coef(summary(abun.arable_percentage))%>%
  mutate(estimate= round(estimate, digits = 3),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =3),
         ci.ub = round(ci.ub, digits =5),
         name = c("intercept", "slope"),
         x = c(55,70),
         y= c(2.5,5.5),
         label = paste("slope LRR = ", estimate, " (95% CI: ", ci.lb, ", ", ci.ub, ")",sep=""))%>%
  filter(name == "slope")
abun.arable.summary

abun.arable<- ggplot(preds.abun.arable,aes(x=X.arable_percentage_mean,y=pred))+
  geom_point(data=abundance_logRR,aes(x=arable_percentage_mean,y=LRR),colour="grey85",fill="white",
             position="jitter",alpha=0.4,shape= 20,size=2)+
  stat_smooth(data=preds.abun.arable,aes(x=X.arable_percentage_mean,y=pred),method="lm",formula=y~x,
              fullrange=T,se=FALSE,size=1.5,colour="#686D35")+
  stat_smooth(data=preds.abun.arable,aes(x=X.arable_percentage_mean,y=ci.lb),method="lm",formula=y~x,
              fullrange=T,se=FALSE,size=1,linetype=2,colour="#686D35")+
  stat_smooth(data=preds.abun.arable,aes(x=X.arable_percentage_mean,y=ci.ub),method="lm",formula=y~x,
              fullrange=T, se=FALSE,size=1,linetype=2,colour="#686D35")+
  geom_hline(yintercept = 0, colour = "grey50" )+
  #scale_x_discrete(limits=c(0, 25, 50, 75, 100, 105),labels=c("0", "25", "50", "75","100", ""))+
  scale_y_discrete(limits = c(-8,-6,-4,-2,0,2,4,6))+
  labs(x=expression(bold(paste("% of landscape cover by agricultural land"))),
       y =expression(bold(paste("Effect size (LRR)"))))+
  geom_label(data=abun.arable.summary, mapping= aes(x=x, y=y,label= label),
             size=4.5, family = "sans")+
  theme(
    axis.text.x = element_text(color="#22211d",size=12,  family = "sans"),
    axis.text.y = element_text(color="#22211d",size=12, family = "sans"),
    text = element_text(color = "#22211d", size =13, face = "bold", family = "sans"),
    plot.background = element_rect(fill = "White", color = "White"), 
    panel.background = element_rect(fill = "White", color = "White"), 
    axis.line = element_line(colour = "black"))
abun.arable

##################################################################################################################################333
############################## S P E C I E S  R I C H N E S S ######################################################################
#############################################################################################################################3
########---- INTERCEPT ONLY MODEL -------###########
#Estimate the overall mean effect size by fitting an intercept-only model.
#See:  Lopez-Lopez et al. 2018; Assink and Wibbelink (2016)
#tdist=TRUE = the argument specifying that test statistics and confidence intervals must be based on the 
#t-distribution (Assink and Wibbelink 2016)
#For the methodology read Li, Y., Shi, L., & Roth, D. (1994) to see the problem of not use tdis .
#For the methodology read Knapp, G. & Hartung, J. (2003), they proposed to use tdist.
#LEVEL 2: ES_ID (the variable containing the unique identifiers of all ef- fect sizes in the data set)
#LEVEL 3: ID_C (study ID)
#RESULTS: k= number of ES comprised; REML(REstricted Maximum Likelihood estimation method): method is in some 
#ways superior to other methods (see, Hox, 2010; Viechtbauer, 2005), but has restrictions (see Cheung, 2014; Van den Noortgate et al., 2013).
#RESULTS: sigma^2.1 (estim): variance between ES within studies (level 2) if it is small indicates that the ES are similar within studies (Cheung 2014); 
#sigma^2.2(estim): variance between studies (level 3): if it is large, indicates the population effect sizes vary across Level 3, so study characteristics can be included to explain the heterogeneity at level 3 (Cheung 2014)
#RESULTS: Test for heterogeneity: p-val<0.001 significant variation between all effect sizes in the data set (However, these results are not very informative, as we are interested in within-study variance (level 2) as well as between- study variance (level 3) and not in variance between all effect sizes in the data set.)
#RESULTS: estimate = the overall effect size; se = standard error; tval = t value; pval = p value; ci.lb = lower bound of the confidence in- terval; and ci.ub = upper bound of the confidence in- terval.
richness.overall <- rma.mv(y= LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                           tdist= TRUE, data=richness_logRR, method="REML")
summary(richness.overall, digits=3)

##Sensitivity analysis
#Standardized residuals
rs.richness.overall<- rstandard(richness.overall)
rs.richness.overall<- as.data.frame(rs.richness.overall)
rs.richness.overall$ES_ID <- as.numeric(1:nrow(rs.richness.overall)) #add a new column with the effect size ID number

#Hat values
hat.richness.overall<- hatvalues.rma.mv(richness.overall)
hat.richness.overall<- as.data.frame(hat.richness.overall)
hat.richness.overall$ES_ID <- as.numeric(1:nrow(hat.richness.overall)) #add a new column with the effect size ID number

#Plot hat values agains residual values
plot(x=hat.richness.overall$hat.richness.overall, y= rs.richness.overall$resid, 
     ylab= "Standardized residuals", xlab= "Hat values")
abline(v = 0.007, lty=2, lwd=2, col="grey50")
abline(h = 2.5, lty=2, lwd=2, col="grey50")
abline(h = -2.5, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the abundance database
richness_logRR_sensitivity.overall<- left_join(hat.richness.overall,rs.richness.overall, by="ES_ID")%>%
  left_join(richness_logRR, by ="ES_ID")%>%
  filter(hat.richness.overall<0.007)%>%
  filter(resid < 2.5)%>%
  filter(resid >-2.5)%>%
  select(!resid & !hat.richness.overall& !resid&!se&!z)

# Model without outlier effect sizes
richness.overall.sensitivity <- rma.mv(y= LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                       tdist= TRUE, data=richness_logRR_sensitivity.overall, method="REML")
summary(richness.overall.sensitivity, digits=3)
summary(richness.overall, digits=3)

#####Heterogeneity of within-study variance (level 2)###
#Build a two-level model without within-study variance.
#If the test results provide support for rejecting the null hypothesis, we can conclude that the fit of the 
#original three-level model is statistically better than the fit of the two-level model, and consequently, that 
#there is significant variability between effect sizes within studies.
#sigma2=c(0,NA) = the argument is taken by the rma.mv function when the user wants to fix a specific 
#variance component to a user-defined value. The first parameter (0) states that the within-study variance is 
#fixed to zero (i.e., no within-study variance will be modeled), and the second parameter (NA) states that the 
#between-study variance is estimated.
#The variance at the first level (sampling variance) was not included in the model, because it is assumed to be known.
richness.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), sigma2=c(0,NA), tdist=TRUE, 
                               data=richness_logRR, method="REML")
summary(richness.modelnovar2)

# Perform a likelihood-ratio-test to determine the significance of the within-study variance (level2).
#RESULTS: Full= represents the three-level model stored in the object abun.overall; 
#Reduced= represents the two- level model stored in the object abun.modelnovar2
#df = degrees of freedom= The reduced model has one degree less than the full model, since within-study vari- ance is not present in the reduced model;
#LRT = likelihood-ratio test. In this column, the value of the test statistic is presented;
#pval = the two-sided p value of the test statistic
#QE= resembles the test for heterogeneity in all effect sizes in the data set, and the value of the test statistic is given in this column
#CONCLUSION: LRT<pval = I found significant variability between effect sizes within studies
anova(richness.overall,richness.modelnovar2) #SPECIES RICHNESS

###Heterogeneity of between-study variance (level 3)
# Build a two-level model without between-study variance;
#In this last model, between-study variance is not modeled. 
#If the null hypothesis should be rejected based on the test results, we can conclude that the fit of the 
#original three-level model is statistically better than the fit of the two-level model, and consequently, 
#that there is significant variability between studies.
#sigma2=c(NA,0): Since we want to fix the between-study variance to zero and freely estimate the within-study variance
richness.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C),
                               sigma2=c(NA,0), tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.modelnovar3)

# Perform a likelihood-ratio-test to determine the significance of the between-study variance.
#CONCLUSION: LRT<pval = I found significant variability between studies
anova(richness.overall,richness.modelnovar3) #SPECIES RICHNESS

#CONCLUSION: In our database, there is significant within-study variance (at level 2) as well as significant 
#between-study variance (at level 3). This implies that there is more variability in effect sizes (within and 
#between studies) than may be expected based on sampling variance alone. Therefore, moderator analyses can be 
#performed in order to examine variables that may explain within- and/or between-study variance. However, before 
#turning to moderator analyses, we will first examine how the total variance is distributed over the three levels 
#of the meta-analytic model.

###The distribution of the variance over the three levels of the meta-analytic model
#Recall that three different sources of variance are modeled in our meta-analytic model: sampling variance at the
#first level; within-study variance at the second level; and between-study variance at the third level.
#To determine how much variance can be attributed to differences between effect sizes within studies (level 2)
#and to differences between studies (level 3), formulas given by Cheung (2014 - formula 14 on page 2015) can be used
# Determining how the total variance is distributed over the three levels of the meta-analytic model;
# Print the results in percentages on screen.
richness.estimated.sampling.variance<- estimated.sampling.variance.func(richness_logRR$LRR_var)

###Each of the three variance components (I2_1, I2_2, I2_3) is divided by the total amount of variance, 
###so that a proportional estimate of each variance component is stored in an object.
###overall$sigma2[1]: refers to the amount of within-study variance in the object abun.overall 
###overall$sigma2[2]: refers to the amount of between-study variance in the object abun.overall
###The proportional estimates of the three variance com- ponents are multiplied by 100 (%), so that a percentage 
#estimate of each variance component is stored in an object

#Sampling variance (Amount of variance at level 1)
((richness.estimated.sampling.variance)/(richness.overall$sigma2[1]+richness.overall$sigma2[2]+richness.estimated.sampling.variance))*100
#Within-study variance (Amount of variance at level 2)
((richness.overall$sigma2[1]) / (richness.overall$sigma2[1] + richness.overall$sigma2[2] + richness.estimated.sampling.variance))*100
#Between-study variance (Amount of variance at level 3)
((richness.overall$sigma2[2]) / (richness.overall$sigma2[1] + richness.overall$sigma2[2] + richness.estimated.sampling.variance))*100

#CONCLUSION: If less than 75% of the total amount of variance is attributed to sampling variance (at level 1), 
#it would be fruitful to examine the potential moderating effect of study and/or effect size characteristics on 
#the over- all effect. In our database,less than 1% of the total amount of variance could be attributed to 
#sampling variance (see abun.I2_1), and based on the rule of Hunter and Schmidt (1990), we can once again 
#conclude that there is substantial variation between effect sizes within studies and/or between studies, 
#making it relevant to perform moderator analyses.

#-------------------------##########  META-REGRESSION (MODERATOR: FUNCTIONAL GROUPS) ################----------------------------------------------------#
###Because the variance within and between studies was substantial, we have to fit a moderator analysis
### Determine the potential moderating effect of functional group;
# Autotrophs is chosen as the reference category;
#Meta-regression model
richness.FG <- rma.mv(y=LRR, V=LRR_var, mods = ~ FG_recla, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                      tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.FG, digits=3)

##Sensitivity analysis
#Standardized residuals
rs.richness.FG<- rstandard(richness.FG)
rs.richness.FG<- as.data.frame(rs.richness.FG)
rs.richness.FG$ES_ID <- as.numeric(1:nrow(rs.richness.FG)) #add a new column with the effect size ID number

#Hat values
hat.richness.FG<- hatvalues.rma.mv(richness.FG)
hat.richness.FG<- as.data.frame(hat.richness.FG)
hat.richness.FG$ES_ID <- as.numeric(1:nrow(hat.richness.FG)) #add a new column with the effect size ID number

#Plot hat values agains residual values
plot(x=hat.richness.FG$hat.richness.FG, y= rs.richness.FG$resid, 
     ylab= "Standardized residuals", xlab= "Hat values")
abline(v = 0.3, lty=2, lwd=2, col="grey50")
abline(h = 2.5, lty=2, lwd=2, col="grey50")
abline(h = -2.5, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the species richness database
richness_logRR_sensitivity.FG<- left_join(hat.richness.FG,rs.richness.FG, by="ES_ID")%>%
  left_join(richness_logRR, by ="ES_ID")%>%
  filter(hat.richness.FG<0.3)%>%
  filter(resid < 2.5)%>%
  filter(resid >-2.5)

# Model without outlier effect sizes
richness.FG.sensitivity <- rma.mv(y=LRR, V=LRR_var, mods = ~ FG_recla, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                  tdist=TRUE, data=richness_logRR_sensitivity.FG, method="REML")

summary(richness.FG.sensitivity, digits=3)
summary(richness.FG, digits=3)

## Heterogeneity analysis
#Heterogeneity: within-study variance (level 2)###
richness.FG.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, mods = ~ FG_recla, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                  sigma2=c(0,NA), tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.FG.modelnovar2)

#Perform a likelihood-ratio-test to determine the significance of the within-study variance.
anova(richness.FG,richness.FG.modelnovar2)

#Heterogeneity: between-study variance (level 3)
richness.FG.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, mods = ~ FG_recla, random = list(~ 1 | ES_ID, ~ 1 | ID_C), sigma2=c(NA,0), 
                                  tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.FG.modelnovar3)

#Perform a likelihood-ratio-test to determine the significance of the between-study variance.
anova(richness.FG, richness.FG.modelnovar3)#SPECIES RICHNESS

#The distribution of the variance over the three levels of the meta-analytic model
richness.FG.estimated.sampling.variance<- estimated.sampling.variance.func(richness_logRR$LRR_var)

#Sampling variance (Amount of variance at level 1)
((richness.FG.estimated.sampling.variance)/(richness.FG$sigma2[1]+richness.FG$sigma2[2]+richness.FG.estimated.sampling.variance))*100
#Within-study variance (Amount of variance at level 2) (Functional groups)
((richness.FG$sigma2[1])/(richness.FG$sigma2[1] + richness.FG$sigma2[2] + richness.FG.estimated.sampling.variance))*100
#Between-study variance (Amount of variance at level 3) (Studies ID)
((richness.FG$sigma2[2]) / (richness.FG$sigma2[1] + richness.FG$sigma2[2] + richness.FG.estimated.sampling.variance))*100

#------- PLOT RESULTS INTERCEPT ONLY MODEL AND META-REGRESSION (MODERATORS = Functional groups)
#---- RESULTS (BEFORE SENSITIVITY ANALYSIS)
richness.FG.out.int<-rma.mv(y=LRR, V=LRR_var, mods = ~ (FG_recla)-1, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                            tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.FG.out.int, digits=3)

#Plot effect sizes Species richness and Functional groups
rich.FG_studies_ES<- richness_logRR %>%
  group_by(FG_recla)%>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))%>%
  mutate(FG = c("Autotrophs","Decomposers", "Natural enemies", "Others", "Pests", "Pollinators"))

rich.FG.comb<- coef(summary(richness.FG.out.int))%>%
  mutate(FG = c("Autotrophs","Decomposers", "Natural enemies", "Others", "Pests", "Pollinators"),
         FG = as.factor(FG))%>%
  right_join(y=rich.FG_studies_ES, by="FG")

rich.overall_studies_ES <- richness_logRR%>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))%>%
  mutate(FG = c("Overall"))

rich.overall.comb<- coef(summary(richness.overall))%>%
  mutate(FG_recla = c("Summary"),
         FG = c("Overall"),
         FG= as.factor(FG))%>%
  right_join(y=rich.overall_studies_ES, by="FG")

rich.comb<- rbind(rich.FG.comb,rich.overall.comb)%>%
  rename(ES_LRR = estimate, 
         SE = se)%>%
  mutate(ES_percent = (100*(exp(ES_LRR)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         ES_LRR = round(ES_LRR, digits = 2),
         SE = round(SE, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =0),
         ci.lb_percent = round(ci.lb_percent, digits = 0),
         ci.ub_percent = round(ci.ub_percent, digits = 0))%>%
  select("FG", "ES_LRR", "SE","ci.lb","ci.ub","tval" ,"pval", "n_studies","n_effectsizes","ES_percent",
         "ci.lb_percent", "ci.ub_percent")

rich.comb.graph<- rich.comb%>%
  add_row(ES_LRR = c(NA), SE = c(NA), tval= c(NA), pval= c(NA), ci.lb= c(NA), ci.ub= c(NA),
          FG= c(NA), ES_percent= c(NA), ci.lb_percent = c(NA), ci.ub_percent= c(NA), .before = 1)%>%
  mutate(ID = row_number())
rich.comb.graph

rich.tabletext<-rich.comb%>%
  mutate(ES_LRR = paste(ES_LRR, " ","(", ES_percent, ")", sep=""),
         ci.lb = paste(ci.lb, " ","(", ci.lb_percent, ")", sep=""),
         ci.ub = paste(ci.ub, " ","(", ci.ub_percent, ")", sep=""),
         SE = as.character(SE),
         tval = as.character(tval),
         pval = as.character(pval),
         n_studies = as.character(n_studies),
         n_effectsizes = as.character(n_effectsizes),
         pval= if_else(pval == "0", "<0.001", pval),
         pval = if_else(FG == "Autotrophs"| FG == "Natural enemies"| FG == "Pests"| 
                          FG =="Pollinators"| FG== "Overall", paste(pval, "**", sep= ""),pval))%>%
  select(FG,ES_LRR, SE ,ci.lb, ci.ub, tval, pval,n_studies, n_effectsizes)%>%
  add_row(FG= c("Functional groups"),ES_LRR = c("ES"), SE = c("SE"), tval= c("t-value"), 
          pval= c("p-value"), ci.lb= c("LL"), ci.ub= c("UL"), n_studies= c("# Studies"), 
          n_effectsizes =c("# ES"), .before = 1)%>%
  mutate(FG = c("Functional groups","Autotrophs","Decomposers", "Natural enemies",
                "Others", "Pests", "Pollinators",  "Overall"))
rich.tabletext

##Figure: Forest plot 
forestplot(rich.tabletext, mean= rich.comb.graph$ES_percent, lower= rich.comb.graph$ci.lb_percent, 
           upper= rich.comb.graph$ci.ub_percent, graph.pos=2,
           new_page = TRUE, is.summary = c(TRUE,rep(FALSE,6), TRUE),  
           xlab = "% Effect size (±95% CI)", clip = c(-10,100),
           ci.vertices = TRUE, boxsize= 0.3,
           col= fpColors(box="#22211d", line="#22211d", summary="#A63117", zero ="gray50"),
           txt_gp = fpTxtGp(label = gpar(fontfamily = "sans", col= "black", cex = 1.4),
                            ticks= gpar(fontfamily = "sans", cex = 1.1, col= "black"),
                            xlab= gpar(fontfamily = "sans", cex = 1.3, col= "black")),
           hrzl_lines = list("2" = gpar(lwd=2, col = "black"),
                             "8" = gpar(lwd=2, col = "black")),
           vertices=TRUE, align = c("l",rep("c",8)),
           colgap = unit(3.5,"mm"),
           xticks = c(-25, 0, 25, 50,  75),
           lineheight=unit(1.6,'cm'),
           graphwidth = unit(6.2, "cm"))

#---- RESULTS (AFTER SENSITIVITY ANALYSIS)
richness.FG.out.int_sensitivity<-rma.mv(y=LRR, V=LRR_var, mods = ~ (FG_recla)-1, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                        tdist=TRUE, data=richness_logRR_sensitivity.FG, method="REML")
summary(richness.FG.out.int_sensitivity, digits=3)
summary(richness.FG.out.int)

#Plot effect sizes Species richness and Functional groups
rich.FG_studies_ES_sensitivity<- richness_logRR_sensitivity.FG %>%
  group_by(FG_recla)%>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))%>%
  mutate(FG = c("Autotrophs","Decomposers", "Natural enemies", "Pests", "Pollinators"))

rich.FG.comb_sensitivity<- coef(summary(richness.FG.out.int_sensitivity))%>%
  mutate(FG = c("Autotrophs","Decomposers", "Natural enemies", "Pests", "Pollinators"),
         FG = as.factor(FG))%>%
  right_join(y=rich.FG_studies_ES_sensitivity, by="FG")

rich.overall_studies_ES_sensitivity <- richness_logRR_sensitivity.FG%>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))%>%
  mutate(FG = c("Overall"))

rich.overall.comb_sensitivity<- coef(summary(richness.overall.sensitivity))%>%
  mutate(FG_recla = c("Summary"),
         FG = c("Overall"),
         FG= as.factor(FG))%>%
  right_join(y=rich.overall_studies_ES_sensitivity, by="FG")

rich.comb_sensitivity<- rbind(rich.FG.comb_sensitivity,rich.overall.comb_sensitivity)%>%
  rename(ES_LRR = estimate, 
         SE = se)%>%
  mutate(ES_percent = (100*(exp(ES_LRR)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         ES_LRR = round(ES_LRR, digits = 2),
         SE = round(SE, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =0),
         ci.lb_percent = round(ci.lb_percent, digits = 0),
         ci.ub_percent = round(ci.ub_percent, digits = 0))%>%
  select("FG", "ES_LRR", "SE","ci.lb","ci.ub","tval" ,"pval", "n_studies","n_effectsizes","ES_percent",
         "ci.lb_percent", "ci.ub_percent")

rich.comb.graph_sensitivity<- rich.comb_sensitivity%>%
  add_row(ES_LRR = c(NA), SE = c(NA), tval= c(NA), pval= c(NA), ci.lb= c(NA), ci.ub= c(NA),
          FG= c(NA), ES_percent= c(NA), ci.lb_percent = c(NA), ci.ub_percent= c(NA), .before = 1)%>%
  mutate(ID = row_number())
rich.comb.graph_sensitivity

rich.tabletext_sensitivity<-rich.comb_sensitivity%>%
  mutate(ES_LRR = paste(ES_LRR, " ","(", ES_percent, ")", sep=""),
         ci.lb = paste(ci.lb, " ","(", ci.lb_percent, ")", sep=""),
         ci.ub = paste(ci.ub, " ","(", ci.ub_percent, ")", sep=""),
         SE = as.character(SE),
         tval = as.character(tval),
         pval = as.character(pval),
         n_studies = as.character(n_studies),
         n_effectsizes = as.character(n_effectsizes),
         pval= if_else(pval == "0", "<0.001", pval),
         pval = if_else(FG == "Autotrophs"| FG == "Natural enemies"| FG == "Pests"| 
                          FG =="Pollinators"| FG== "Overall", paste(pval, "**", sep= ""),pval))%>%
  select(FG,ES_LRR, SE ,ci.lb, ci.ub, tval, pval,n_studies, n_effectsizes)%>%
  add_row(FG= c("Functional groups"),ES_LRR = c("ES"), SE = c("SE"), tval= c("t-value"), 
          pval= c("p-value"), ci.lb= c("LL"), ci.ub= c("UL"), n_studies= c("# Studies"), 
          n_effectsizes =c("# ES"), .before = 1)%>%
  mutate(FG = c("Functional groups","Autotrophs","Decomposers", "Natural enemies",
                "Pests", "Pollinators",  "Overall"))
rich.tabletext_sensitivity
summary(richness.FG.out.int_sensitivity)

##Figure: Forest plot 
forestplot(rich.tabletext_sensitivity, mean= rich.comb.graph_sensitivity$ES_percent, lower= rich.comb.graph_sensitivity$ci.lb_percent, 
           upper= rich.comb.graph_sensitivity$ci.ub_percent, graph.pos=2,
           new_page = TRUE, is.summary = c(TRUE,rep(FALSE,5), TRUE),  
           xlab = "% Effect size (±95% CI)", clip = c(-10,100),
           ci.vertices = TRUE, boxsize= 0.3,
           col= fpColors(box="#22211d", line="#22211d", summary="#A63117", zero ="gray50"),
           txt_gp = fpTxtGp(label = gpar(fontfamily = "sans", col= "black", cex = 1.4),
                            ticks= gpar(fontfamily = "sans", cex = 1.1, col= "black"),
                            xlab= gpar(fontfamily = "sans", cex = 1.3, col= "black")),
           hrzl_lines = list("2" = gpar(lwd=2, col = "black"),
                             "7" = gpar(lwd=2, col = "black")),
           vertices=TRUE, align = c("l",rep("c",8)),
           colgap = unit(3.5,"mm"),
           xticks = c(-25, 0, 25, 50,  75),
           lineheight=unit(1.6,'cm'),
           graphwidth = unit(6.2, "cm"))

#-------------------  META-REGRESSION (MODERATORS = LANDSCAPE METRICS) ----------------------------------------------------#
# See step by step in https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/multiple-meta-regression.html
# Determine the potential moderating effect of the assessed landscape metrics 

#---------------  MODERATOR = % OF NATURAL HABITATS
richness.natural_percentage <- rma.mv(y=LRR, V=LRR_var, mods = ~ natural_percentage_mean,random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                      tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.natural_percentage, digits=3)

#Results meta-regression
coef(summary(richness.natural_percentage))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),estimate = round(estimate, digits = 3),
         se = round(se, digits =3),tval= round(tval, digits =3),pval= round(pval, digits =3),
         ci.lb = round(ci.lb, digits =3),ci.ub = round(ci.ub, digits =3),
         ES_percent =round(ES_percent, digits =1),ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

##Sensitivity analysis
#Standardized residuals
rs.richness.natural_percentage<- rstandard(richness.natural_percentage)
rs.richness.natural_percentage<- as.data.frame(rs.richness.natural_percentage)
rs.richness.natural_percentage$ES_ID <- as.numeric(1:nrow(rs.richness.natural_percentage)) #add a new column with the effect size ID number

#Hat values
hat.richness.natural_percentage<- hatvalues.rma.mv(richness.natural_percentage)
hat.richness.natural_percentage<- as.data.frame(hat.richness.natural_percentage)
hat.richness.natural_percentage$ES_ID <- as.numeric(1:nrow(hat.richness.natural_percentage)) #add a new column with the effect size ID number

#Plot hat values agains residual values
plot(x=hat.richness.natural_percentage$hat.richness.natural_percentage, y= rs.richness.natural_percentage$resid, 
     ylab= "Standardized residuals", xlab= "Hat values")
abline(v = 0.029, lty=2, lwd=2, col="grey50")
abline(h = 2, lty=2, lwd=2, col="grey50")
abline(h = -2, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the species richness database
richness_logRR_sensitivity.natural_percentage<- left_join(hat.richness.natural_percentage,rs.richness.natural_percentage, by="ES_ID")%>%
  left_join(richness_logRR, by ="ES_ID")%>%
  filter(hat.richness.FG<0.029)%>%
  filter(resid < 2)%>%
  filter(resid >-2)

# Model without outlier effect sizes
richness.natural_percentage.sensitivity <- rma.mv(y=LRR, V=LRR_var, mods = ~ natural_percentage_mean, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                                  tdist=TRUE, data=richness_logRR_sensitivity.natural_percentage, method="REML")
summary(richness.natural_percentage.sensitivity, digits=3)
summary(richness.natural_percentage, digits=3)

#Results meta-regression
coef(summary(richness.natural_percentage.sensitivity))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),estimate = round(estimate, digits = 3),
         se = round(se, digits =3),tval= round(tval, digits =3),pval= round(pval, digits =3),
         ci.lb = round(ci.lb, digits =3),ci.ub = round(ci.ub, digits =3),
         ES_percent =round(ES_percent, digits =1),ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

##Heterogeneity analysis
#Heterogeneity of within-study variance (level 2)
richness.natural.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), sigma2=c(0,NA), tdist=TRUE, 
                                       data=richness_logRR, method="REML", mods = ~ natural_percentage_mean)
summary(richness.natural.modelnovar2)

# Perform a likelihood-ratio-test to determine the significance of the within-study variance (level2).
anova(richness.natural_percentage,richness.natural.modelnovar2) #SPECIES RICHNESS

###Heterogeneity of between-study variance (level 3)
richness.natural.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C),
                                       sigma2=c(NA,0), tdist=TRUE, data=richness_logRR, method="REML",
                                       mods = ~ natural_percentage_mean)
summary(richness.natural.modelnovar3)

# Perform a likelihood-ratio-test to determine the significance of the between-study variance.
anova(richness.natural_percentage,richness.natural.modelnovar3) #SPECIES RICHNESS

###The distribution of the variance over the three levels of the meta-analytic model
richness.natural.estimated.sampling.variance<- estimated.sampling.variance.func(richness_logRR$LRR_var)

#Sampling variance (Amount of variance at level 1)
((richness.natural.estimated.sampling.variance)/(richness.natural_percentage$sigma2[1]+richness.natural_percentage$sigma2[2]+richness.natural.estimated.sampling.variance))*100
#Within-study variance (Amount of variance at level 2)
((richness.natural_percentage$sigma2[1]) / (richness.natural_percentage$sigma2[1] + richness.natural_percentage$sigma2[2] + richness.natural.estimated.sampling.variance))*100
#Between-study variance (Amount of variance at level 3)
((richness.natural_percentage$sigma2[2]) / (richness.natural_percentage$sigma2[1] + richness.natural_percentage$sigma2[2] + richness.natural.estimated.sampling.variance))*100

########---- % OF ARABLE HABITATS ---######
#SPECIES RICHNESS
richness.arable_percentage <- rma.mv(y=LRR, V=LRR_var, mods = ~ arable_percentage_mean, random = list(~ 1 | ES_ID, ~ 1 | ID_C),
                                     tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.arable_percentage, digits=3)

#Results meta-regression
coef(summary(richness.arable_percentage))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),estimate = round(estimate, digits = 3),
         se = round(se, digits =3),tval= round(tval, digits =3),pval= round(pval, digits =3),
         ci.lb = round(ci.lb, digits =3),ci.ub = round(ci.ub, digits =3),
         ES_percent =round(ES_percent, digits =1),ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

##Sensitivity analysis
#Standardized residuals
rs.richness.arable_percentage<- rstandard(richness.arable_percentage)
rs.richness.arable_percentage<- as.data.frame(rs.richness.arable_percentage)
rs.richness.arable_percentage$ES_ID <- as.numeric(1:nrow(rs.richness.arable_percentage)) #add a new column with the effect size ID number

#Hat values
hat.richness.arable_percentage<- hatvalues.rma.mv(richness.arable_percentage)
hat.richness.arable_percentage<- as.data.frame(hat.richness.arable_percentage)
hat.richness.arable_percentage$ES_ID <- as.numeric(1:nrow(hat.richness.arable_percentage)) #add a new column with the effect size ID number

#Plot hat values agains residual values
plot(x=hat.richness.arable_percentage$hat.richness.arable_percentage, 
     y= rs.richness.arable_percentage$resid, ylab= "Standardized residuals", xlab= "Hat values")
abline(v = 0.02, lty=2, lwd=2, col="grey50")
abline(h = 2, lty=2, lwd=2, col="grey50")
abline(h = -2, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the species richness database
richness_logRR_sensitivity.arable_percentage<- left_join(hat.richness.arable_percentage,
                                                         rs.richness.arable_percentage, by="ES_ID")%>%
  left_join(richness_logRR, by ="ES_ID")%>%
  filter(hat.richness.FG<0.02)%>%
  filter(resid < 2)%>%
  filter(resid >-2)

# Model without outlier effect sizes
richness.arable_percentage.sensitivity <- rma.mv(y=LRR, V=LRR_var, mods = ~ arable_percentage_mean, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                                 tdist=TRUE, data=richness_logRR_sensitivity.arable_percentage, method="REML")
summary(richness.arable_percentage.sensitivity, digits=3)
summary(richness.arable_percentage, digits=3)

#Results meta-regression (after sensitivity analysis)
coef(summary(richness.arable_percentage.sensitivity))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),estimate = round(estimate, digits = 3),
         se = round(se, digits =3),tval= round(tval, digits =3),pval= round(pval, digits =3),
         ci.lb = round(ci.lb, digits =3),ci.ub = round(ci.ub, digits =3),
         ES_percent =round(ES_percent, digits =1),ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

##Heterogeneity analysis
#Heterogeneity of within-study variance (level 2)###
##ABUNDANCE
richness.arable.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), sigma2=c(0,NA), tdist=TRUE, 
                                      data=richness_logRR, method="REML", mods = ~ arable_percentage_mean)
summary(richness.arable.modelnovar2)

# Perform a likelihood-ratio-test to determine the significance of the within-study variance (level2).
anova(richness.arable_percentage,richness.arable.modelnovar2) #SPECIES RICHNESS

###Heterogeneity of between-study variance (level 3)
richness.arable.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C),
                                      sigma2=c(NA,0), tdist=TRUE, data=richness_logRR, method="REML",
                                      mods = ~ arable_percentage_mean)
summary(richness.arable.modelnovar3)

# Perform a likelihood-ratio-test to determine the significance of the between-study variance.
anova(richness.arable_percentage,richness.arable.modelnovar3) #SPECIES RICHNESS

###The distribution of the variance over the three levels of the meta-analytic model
richness.arable.estimated.sampling.variance<- estimated.sampling.variance.func(richness_logRR$LRR_var)

#Sampling variance (Amount of variance at level 1)
((richness.arable.estimated.sampling.variance)/(richness.arable_percentage$sigma2[1]+richness.arable_percentage$sigma2[2]+richness.arable.estimated.sampling.variance))*100
#Within-study variance (Amount of variance at level 2)
((richness.arable_percentage$sigma2[1]) / (richness.arable_percentage$sigma2[1] + richness.arable_percentage$sigma2[2] + richness.arable.estimated.sampling.variance))*100
#Between-study variance (Amount of variance at level 3)
((richness.arable_percentage$sigma2[2]) / (richness.arable_percentage$sigma2[1] + richness.arable_percentage$sigma2[2] + richness.arable.estimated.sampling.variance))*100

#########----------- META-REGRESSION (MODERATOR: DISTANCE TO NATURAL HABITATS) ------------------######
## META-REGRESSION MODEL: using min_distance_mean (meters) as moderator
richness.distance <- rma.mv(y=LRR, V=LRR_var, mods = ~min_distance_mean, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                            tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.distance, digits=4)

#Calulate the residuals from the metaregression model using distance in meters
rs.richness.distance<-rstandard.rma.mv(richness.distance, type="rstandard")

#Calculate predictor values for the model
preds.richness.distance <-predict(richness.distance,newmods=c(0:1000), addx=TRUE)
preds.richness.distance<-as.data.frame(preds.richness.distance)

## META-REGRESSION MODEL: using min_log_distance_mean (ln(distance + 1)) as moderator
richness.log_distance <- rma.mv(y=LRR, V=LRR_var, mods = ~min_log_distance_mean, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.log_distance, digits=4)

#Calulate the residuals from the metaregression model using distance in log10
rs.richness.log_distance<-rstandard.rma.mv(richness.log_distance, type="rstandard")

#Calculate predictor values for the model
preds.richness.log_distance <-predict(richness.log_distance,newmods=c(0:1000), addx=TRUE)
preds.richness.log_distance<-as.data.frame(preds.richness.log_distance)

par(mfrow=c(1,2),oma = c(0, 0, 2, 0)) #two plots in the same graph
#Plot residuals vs. distance in meters
plot(richness_logRR$min_distance_mean, rs.richness.distance$resid,
     ylab="Standardized residuals", xlab= "Distance (m)", main="A")
lines(x=preds.richness.distance$X.min_distance_mean ,y=preds.richness.distance$pred, col="#A63117", lwd=2)
#Plot residuals vs. distance in (ln(Distance + 1)
plot(richness_logRR$min_log_distance_mean, rs.richness.log_distance$resid,
     ylab="Standardized residuals", xlab= "ln(Distance + 1)", main="B")
lines(x=preds.richness.log_distance$X.min_log_distance_mean ,y=preds.richness.log_distance$pred, col="#A63117", lwd=2)
mtext("Species richness", outer = TRUE, cex = 1.5)

##Sensitivity analysis
#Standardized residuals
rs.richness.log_distance<- as.data.frame(rs.richness.log_distance)
rs.richness.log_distance$ES_ID <- as.numeric(1:nrow(rs.richness.log_distance)) #add a new column with the effect size ID number

#Hat values
hat.richness.log_distance<- hatvalues.rma.mv(richness.log_distance)
hat.richness.log_distance<- as.data.frame(hat.richness.log_distance)
hat.richness.log_distance$ES_ID <- as.numeric(1:nrow(hat.richness.log_distance)) #add a new column with the effect size ID number

#Plot hat values agains residual values
par(mfrow=c(1,1))
plot(x=hat.richness.log_distance$hat.richness.log_distance, 
     y= rs.richness.log_distance$resid, ylab= "Standardized residuals", xlab= "Hat values")
abline(v = 0.015, lty=2, lwd=2, col="grey50")
abline(h = 2, lty=2, lwd=2, col="grey50")
abline(h = -2, lty=2, lwd=2, col="grey50")

#Identify possible effect size outliers and exclude outliers from the species richness database
richness_logRR_sensitivity.log_distance<- left_join(hat.richness.log_distance,
                                                    rs.richness.log_distance, by="ES_ID")%>%
  left_join(richness_logRR, by ="ES_ID")%>%
  filter(hat.richness.log_distance<0.015)%>%
  filter(resid < 2)%>%
  filter(resid >-2)

# Model without outlier effect sizes
richness.log_distance.sensitivity <- rma.mv(y=LRR, V=LRR_var, mods = ~ min_log_distance_mean, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                                            tdist=TRUE, data=richness_logRR_sensitivity.log_distance, method="REML")
summary(richness.log_distance.sensitivity, digits=3)
summary(richness.log_distance, digits=3)

#Results meta-regression (after sensitivity analysis)
coef(summary(richness.log_distance.sensitivity))%>%
  mutate(estimate = round(estimate, digits = 3),
         se = round(se, digits =3),tval= round(tval, digits =3),pval= round(pval, digits =3),
         ci.lb = round(ci.lb, digits =3),ci.ub = round(ci.ub, digits =3))

##Heterogeneity analysis
##Heterogeneity of within-study variance (level 2)###
richness.log_distance.modelnovar2 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C), sigma2=c(0,NA), tdist=TRUE, 
                                            data=richness_logRR, method="REML", mods = ~ min_log_distance_mean)
summary(richness.log_distance.modelnovar2)

# Perform a likelihood-ratio-test to determine the significance of the within-study variance (level2).
anova(richness.log_distance, richness.log_distance.modelnovar2)

###Heterogeneity of between-study variance (level 3)
# Build a two-level model without between-study variance;
richness.log_distance.modelnovar3 <- rma.mv(y=LRR, V=LRR_var, random = list(~ 1 | ES_ID, ~ 1 | ID_C),
                                            sigma2=c(NA,0), tdist=TRUE, data=richness_logRR, method="REML",
                                            mods = ~ min_log_distance_mean)
summary(richness.log_distance.modelnovar3)

# Perform a likelihood-ratio-test to determine the significance of the between-study variance.
anova(richness.log_distance,richness.log_distance.modelnovar3) #SPECIES RICHNESS

###The distribution of the variance over the three levels of the meta-analytic model
richness.log_distance.estimated.sampling.variance<- estimated.sampling.variance.func(richness_logRR$LRR_var)

#Sampling variance (Amount of variance at level 1)
((richness.log_distance.estimated.sampling.variance)/(richness.log_distance$sigma2[1]+richness.log_distance$sigma2[2]+richness.log_distance.estimated.sampling.variance))*100
#Within-study variance (Amount of variance at level 2)
((richness.log_distance$sigma2[1]) / (richness.log_distance$sigma2[1] + richness.log_distance$sigma2[2] + richness.log_distance.estimated.sampling.variance))*100
#Between-study variance (Amount of variance at level 3)
((richness.log_distance$sigma2[2]) / (richness.log_distance$sigma2[1] + richness.log_distance$sigma2[2] + richness.log_distance.estimated.sampling.variance))*100

##----Plot LANDSCAPE ANALYSIS
preds.rich.log.distance <-predict(richness.natural_log.distance,newmods=c(0:8), addx=TRUE)
preds.rich.log.distance<-as.data.frame(preds.rich.log.distance)%>%
  rename("X.log.min_distance_mean" ="X.log.min_distance_mean.")
preds.rich.log.distance

summary(richness.log_distance)
rich.log.distance.summary<- coef(summary(richness.log_distance))%>%
  mutate(estimate= round(estimate, digits = 2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =3),
         ci.ub = round(ci.ub, digits =3),
         name = c("intercept", "slope"),
         x = c(1,4),
         y= c(2.5,3.7),
         label = paste("slope (%) = ", estimate, " (95% CI: ", ci.lb, ", ", ci.ub, ")",sep=""))%>%
  filter(name == "slope")
rich.log.distance.summary

rich.log.distance<- ggplot(preds.rich.log.distance,aes(x=X.log.min_distance_mean,y=pred))+
  geom_point(data=richness_logRR_log_distance,aes(x=min_log_distance_mean,y=LRR),colour="grey80",fill="white",
             position="jitter",alpha=0.4,shape= 20,size=2)+
  stat_smooth(method="glm",formula=y~x,fullrange=T,se=FALSE,size=1.5, colour="#A63117")+
  stat_smooth(data=preds.rich.log.distance,aes(x=X.log.min_distance_mean,y=ci.lb),method="lm",formula=y~x,
              fullrange=T,se=FALSE,size=1,linetype=2,colour="#A63117")+
  stat_smooth(data=preds.rich.log.distance,aes(x=X.log.min_distance_mean,y=ci.ub),method="lm",formula=y~x,
              fullrange=T, se=FALSE,size=1,linetype=2,colour="#A63117")+
  geom_hline(yintercept = 0, colour = "grey50" )+
  #scale_x_discrete(limits=c(0, 2, 4, 6, 7, 8),labels=c("0", "2", "4", "6","7", ""))+
  labs(x=expression(bold(paste("Distance to natural or semi-natural habitats [ln(distance + 1)]"))),
       y= expression(bold(paste("Effect size (LRR)"))))+
  geom_label(data=rich.log.distance.summary, mapping= aes(x=x, y=y,label= label),
             size=5, family = "sans")+
  theme(
    axis.text.x = element_text(color="#22211d",size=12,  family = "sans"),
    axis.text.y = element_text(color="#22211d",size=12, family = "sans"),
    text = element_text(color = "#22211d", size =13, face = "bold", family = "sans"),
    plot.background = element_rect(fill = "White", color = "White"), 
    panel.background = element_rect(fill = "White", color = "White"), 
    axis.line = element_line(colour = "black"))
rich.log.distance
summary(richness.log_distance)
##############################################################################################################################333
################################  PUBLICATION BIAS ANALYSIS ###################################################################3----------------------------------------------------#
#####################################################################################################################################33
##FOR BIAS SEE Nakagawa AND Santos 2011
#Contour-enhanced  Funnel plots
#https://rstudio-pubs-static.s3.amazonaws.com/28456_ea0b1faf0f4645cc8af81d81aaf0c1af.html
#http://www.metafor-project.org/doku.php/plots:contour_enhanced_funnel_plot

##Egger regression test
##Replace the moderator by SE (see Habeck & Schultz 2015)
##ABUNDANCE
abun.bias.egger <-rma.mv(y=LRR, V=LRR_var, mods = ~ LRR_se, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                         tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.bias.egger, digits=3)
coef(summary(abun.bias.egger, digits=3))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         estimate = round(estimate, digits = 2),
         se= round(se, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =0),
         ci.lb_percent = round(ci.lb_percent, digits = 0),
         ci.ub_percent = round(ci.ub_percent, digits = 0))

resid.1<-rstandard.rma.mv(abun.bias.egger, type="rstandard")

##SPECIES RICHNESS
richness.bias.egger <-rma.mv(y=LRR, V=LRR_var, mods = ~ LRR_se, random = list(~ 1 | ES_ID, ~ 1 | ID_C), 
                             tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.bias.egger, digits=3)
coef(summary(richness.bias.egger, digits=3))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         estimate = round(estimate, digits = 2),
         se= round(se, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =0),
         ci.lb_percent = round(ci.lb_percent, digits = 0),
         ci.ub_percent = round(ci.ub_percent, digits = 0))

##--Meta-regression
#### MODERATOR: YEAR OF PUBLICATION
#ABUNDANCE
abun.bias.year <- rma.mv(y=LRR, V=LRR_var, mods = ~ Year, method="REML",
                         random = list(~ 1 | ES_ID, ~1 | ID_C), tdist=TRUE, data=abundance_logRR)
summary(abun.bias.year, digits=3)
coef(summary(abun.bias.year, digits=3))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         estimate = round(estimate, digits = 2),
         se= round(se, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =1),
         ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

#SPECIES RICHNESS
richness.year <- rma.mv(y=LRR, V=LRR_var, mods = ~ Year, method="REML",
                        random = list(~ 1 | ES_ID, ~1 | ID_C), tdist=TRUE, data= richness_logRR)
summary(richness.year, digits=3)
coef(summary(richness.year, digits=3))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         estimate = round(estimate, digits = 2),
         se= round(se, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =1),
         ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

#### MODERATOR: LATITUDE
#ABUNDANCE
abun.latitude <- rma.mv(y=LRR, V=LRR_var, mods = ~ Lat_2_C, 
                        random = list(~ 1 | ES_ID, ~1 | ID_C), tdist=TRUE, data=abundance_logRR, method="REML")
summary(abun.latitude, digits=3)
coef(summary(abun.latitude, digits=3))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         estimate = round(estimate, digits = 2),
         se= round(se, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =1),
         ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

#SPECIES RICHNESS
richness.latitude <- rma.mv(y=LRR, V=LRR_var, mods = ~ Lat_2_C, 
                            random = list(~ 1 | ES_ID, ~1 | ID_C), tdist=TRUE, data=richness_logRR, method="REML")
summary(richness.latitude, digits=3)
coef(summary(richness.latitude, digits=3))%>%
  mutate(ES_percent = (100*(exp(estimate)-1)),
         ci.lb_percent = (100*(exp(ci.lb)-1)),
         ci.ub_percent = (100*(exp(ci.ub)-1)),
         estimate = round(estimate, digits = 2),
         se= round(se, digits =2),
         tval= round(tval, digits =2),
         pval= round(pval, digits =2),
         ci.lb = round(ci.lb, digits =2),
         ci.ub = round(ci.ub, digits =2),
         ES_percent =round(ES_percent, digits =1),
         ci.lb_percent = round(ci.lb_percent, digits = 1),
         ci.ub_percent = round(ci.ub_percent, digits = 1))

######------- SENSITIVITY ANALYSIS ----------########
rs.abun.overall<- rstandard(abun.overall)
rs.abun.overall
hat.abun.overall<- hatvalues(abun.overall)/mean(hatvalues(abun.overall))
plot(hat.abun.overall, rs.abun.overall$resid, ylim = c(-4,4))
text(hat.abun.overall, rs.abun.overall$resid, labels = abundance_logRR$ID, cex= 1, pos = 2)
abline(h = -3)
abline(h = 3)
abline(v = 2)


############################## DATA EXTRACTION PROCESS ##########################################################################
###Entry data: Calculate how many observations I recorded
addmargins(table(data$Data_entry))

##Excluded studies by exclusion criteria
addmargins(table(excluded$exclusion_ID, excluded$Exclusion_reason_recla)) #reason of exclusion
addmargins(table(excluded$Article_source_recla)) #source of studies
addmargins(table(excluded$Article_source_recla)) #recording person

length(unique(excluded$ID))
length(unique(meta$ID))
length(unique(data_MA$ID_T))
length(which(excluded$Article_source_recla == "nose"))
sort(which(excluded$Article_source_recla == "nose"))
y<- anti_join(effectsize_logRR, excluded, by = c("ID_C" = "ID"))
sort(unique(excluded$Article_source))


#----------------------------------- RESULTS DATA DESCRIPTION -----------------------------------------------------------------------

##Temporal distribution of the studies
studies_year<- effectsize_logRR%>%
  group_by(ID_C, B_measure, Year)%>%
  tally()%>%
  group_by(B_measure, Year)%>%
  mutate(studies_perYear = sum(n))%>%
  mutate(studies_perYear = 1)%>%
  group_by(B_measure, Year)%>%
  mutate(studies_perYear = sum(studies_perYear))%>%
  ungroup()%>%distinct(B_measure, Year, .keep_all = TRUE)%>%
  group_by(B_measure)%>%
  mutate(total= sum(studies_perYear))%>%
  ungroup()%>%
  mutate(percentage= ((studies_perYear/total)*100))%>%
  mutate(Year= as.factor(Year))

studies_year%>%group_by(B_measure, Year)%>%
  mutate(between_2011_2019= if_else(Year == 2011| Year == 2012| Year == 2013| Year == 2014| Year == 2015| Year == 2016| Year == 2017| Year == 2018| Year == 2019, studies_perYear, 0),
         between_2001_2010 = if_else(Year == 2001|Year == 2002|Year == 2003|Year == 2004|Year == 2005|Year == 2006|Year == 2007|Year == 2008|Year == 2009|Year == 2010, studies_perYear, 0),
         between_1986_2000 = if_else(Year==1986 |Year==1987|Year== 1988 |Year==1992 |Year==1994|Year== 1998 |Year==1999 |Year==2000, studies_perYear, 0))%>%
  group_by(B_measure)%>%mutate(total= mean(total),
                               between_2011_2019= sum(between_2011_2019),
                               between_2001_2010 = sum(between_2001_2010),
                               between_1986_2000 = sum(between_1986_2000))%>%
  ungroup()%>%distinct(B_measure, .keep_all = TRUE)%>%mutate(percentage_between_2011_2019= ((between_2011_2019/total)*100),
                                                             percentage_between_2001_2010 = ((between_2001_2010/total)*100),
                                                             percentage_between_1986_2000 = ((between_1986_2000/total)*100))

sort(unique(studies_year$Year))

#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization#create-barplots

figure_1 <- ggplot(studies_year, aes(x=Year, y= studies_perYear, fill=B_measure))+
  geom_bar(stat="identity")+
  scale_fill_manual(name = "Biodiversity measures", values=c("#686D35","#A63117"))+
  scale_x_discrete(name ="Publication Year", breaks = c(1986, 1994,1995,2000,2005,2010,2015,2019))+
  #scale_y_discrete(name ="Frequency (%)")
  theme(
    legend.position = c(0.25, 0.7),
    legend.justification = c("center", "bottom"),
    legend.direction = "vertical",
    axis.text.x = element_text(color="#22211d",size=10,  family = "sans"),
    axis.text.y = element_text(color="#22211d",size=10, family = "sans"),
    text = element_text(color = "#22211d", size =11, face = "bold", family = "sans"),
    legend.box.background = element_rect(color="#22211d", size=0.5),
    legend.box.margin = margin(3, 3, 3, 3),
    plot.background = element_rect(fill = "White", color = "White"), 
    panel.background = element_rect(fill = "White", color = "White"), 
    legend.background = element_rect(fill = "White", color = "White"),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(size= 11, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))+
  labs(y = "Number of primary studies")
figure_1

#Global distribution of the data
##https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
require(maps)
require(viridis)
#library(magrittr)
theme_set(
  theme_void()
)

###Location of the studies (transform an cvs to shapefile)
##https://datacarpentry.org/r-raster-vector-geospatial/10-vector-csv-to-shapefile-in-r/
studies_location<- data_2 %>%
  group_by(ID, B_measure, Country, Continent, Landscape_ID, Lat, Long) %>% tally()%>%
  mutate(Lat= as.numeric(Lat),
         Long= as.numeric(Long))
length(sort(unique(studies_location$Country))) #total number of countries
length(sort(unique(abundance_logRR$Country))) #number of countries for abundance
length(sort(unique(richness_logRR$Country))) #number of countries for species richness
length(sort(unique(studies_location$Continent))) #total number of continents

### World map
#https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
world_map <- map_data("world")%>%filter(region != "Antarctica")

ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", color = "black")

## Global distribution of the study points
#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
#Label: https://www.datanovia.com/en/blog/how-to-change-ggplot-labels/
#Colors: https://color.adobe.com/search?q=tree
figure_2<- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="lightgray", color = "grey")+
  geom_point(data = studies_location, mapping = aes(x=Long, y=Lat, color = B_measure), cex = 1.4, show.legend = TRUE)+
  scale_color_manual(values = c("#686D35","#A63117"))+
  labs(color = "Biodiversity measures") +
  theme(
    legend.position = c(0.62, 0.05),
    legend.direction = "horizontal", 
    legend.justification = c("center", "bottom"),
    text = element_text(color = "#22211d", size =11, face = "bold", family = "sans"),
    legend.box.background = element_rect(color="#22211d", size=0.5),
    legend.box.margin = margin(6, 6, 6, 6),
    plot.background = element_rect(fill = "White", color = "White"), 
    panel.background = element_rect(fill = "White", color = "White"), 
    legend.background = element_rect(fill = "White", color = "White"),
    plot.title = element_text(size= 11, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
figure_2

##Number or studies and effect sizes by continent
studies_per_continent<- effectsize_logRR%>%
  mutate(Continent = if_else(Continent == "caribbean" | Continent =="central america"| 
                               Continent =="south america", "Central and South America",
                             if_else(Continent == "oceania", "Oceania",
                                     if_else(Continent == "north america", "North America",
                                             if_else(Continent== "europe", "Europe",
                                                     if_else(Continent == "asia", "Asia",
                                                             if_else(Continent == "africa", "Africa", Continent)))))))%>%
  group_by(ID_T,B_measure, Continent)%>%
  tally()%>%ungroup()%>%distinct(ID_T,B_measure, Continent, .keep_all = TRUE)%>%
  group_by(B_measure, Continent)%>%
  mutate(n_effectsizes = sum(n),
         n_studies = length(ID_T), 
         n_studies = mean(n_studies))%>%
  ungroup()%>%distinct(B_measure, Continent, .keep_all = TRUE)%>%
  mutate(n_effectsizes_parentheses = paste("(", n_effectsizes, ")", sep=""))

figure_3<- ggplot(studies_per_continent, aes(x=Continent, y=n_studies, fill=B_measure))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(name = "Biodiversity measures", values=c("#686D35","#A63117"))+
  coord_flip()+
  scale_y_continuous(name = "Number of studies", limits = c(0, 60)) +
  geom_text(aes(label=n_effectsizes_parentheses), vjust= 0.5, position = position_dodge(0.9), 
            color="Black", size=3.5, family="sans", hjust = -0.2)+
  theme(
    legend.position = c(0.79, 0.2),
    legend.justification = c("center", "bottom"),
    legend.direction = "vertical",
    axis.text.x = element_text(color="#22211d",size=10,  family = "sans"),
    axis.text.y = element_text(color="#22211d",size=10, family = "sans"),
    text = element_text(color = "#22211d", size =11, face = "bold", family = "sans"),
    legend.box.background = element_rect(color="#22211d", size=0.5),
    legend.box.margin = margin(3, 3, 3, 3),
    plot.background = element_rect(fill = "White", color = "White"), 
    panel.background = element_rect(fill = "White", color = "White"), 
    legend.background = element_rect(fill = "White", color = "White"),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(size= 11, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))+
  labs(x= " ",y = "Number of studies", color = "#22211d", size =11, face = "bold", 
       family = "sans")
figure_3

##Number of effect sizes grouped by biodiversity measures and functional groups
studies_taxa<- effectsize_logRR %>%
  group_by(B_measure, FG_recla, Taxa_group) %>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))
write.csv(studies_taxa, "studies_taxa_06.25.csv")

#Number of studies and efect sizes by Taxa group
effectsize_logRR %>%
  group_by(B_measure, Taxa_group) %>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))
#Number of studies and effect sizes by functional groups
effectsize_logRR %>%
  group_by(B_measure, FG_recla) %>%
  summarise(n_studies = n_distinct(ID_C),
            n_effectsizes = n_distinct(ES_ID))

###LIST OF INCLUDED STUDIES
included<- effectsize_logRR%>%
  group_by(ID_T)%>%
  summarise(included = n_distinct(ID_C))%>%
  mutate(included = "included")%>%
  rename("ID" = "ID_T")


references<- rbind(references_beillouin, references_others)%>%
  mutate(Title = str_to_lower(Title, locale = "en"))%>%
  right_join(y=list, by="Title")%>%
  mutate(ID= as.character(ID))%>%
  left_join(y=included, by="ID")%>%
  select(1:36,76) 

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#https://stackoverflow.com/questions/9511281/handling-special-characters-e-g-accents-in-r
list$Authors <- iconv(list$Authors, from="UTF-8", to="LATIN1")
list[list==""] <- NA


references<- list%>%mutate(ID= as.character(ID))%>%
  rename("Journal" = "Source.title..Please..make.sure.to.enter.this.correctly..we.needed.to.check.if.it.belongs.to.predatory.journals.")%>%
  anti_join(y=excluded, by="ID")%>%
  mutate(included = if_else(included == "included", "included","excluded"))%>%
  mutate(Title = firstup(Title),
         Authors = gsub(";", ",",Authors),
         Authors = gsub(" ", ", ", Authors),
         Authors = gsub(",+", ",", Authors),
         #Authors = gsub("([A-Z].)([A-Z].)", "\\1 \\2", Authors),
         Year = as.character(Year),
         Year = paste("(",Year,")"))%>%
  # subset(included == "included")%>%
  mutate(reference_cite = if_else(!is.na(Volume)&is.na(Art_No)&!is.na(DOI), paste(Authors," ", Year, ". ", Title, ". ", Journal,". ",Volume,"(", Issue, ")", ", ", Page_start, "-", Page_end, ". ", "available from: ", DOI, ".", sep= ""),
                                  if_else(!is.na(Volume)&is.na(Art_No) & is.na(DOI), paste(Authors," ", Year, ". ", Title, ". ", Journal,". ",Volume,"(", Issue, ")", ", ", Page_start, "-", Page_end, ".", sep= ""), 
                                          if_else(!is.na(Volume)&!is.na(Art_No) & !is.na(DOI),paste(Authors," ", Year, ". ", Title, ". ", Journal,". ",Volume,", ", Art_No, ". ", "available from: ", DOI, ".", sep= ""),
                                                  if_else(is.na(Volume) & !is.na(Page_count), paste(Authors," ", Year, ". ", Title, ". ", Journal,". p. ", Page_count, ".", sep= ""),
                                                          "No listo")))))%>%
  mutate(reference_cite = gsub("[(NA)]", "", reference_cite),
         reference_cite = gsub("available", "Available", reference_cite))%>%
  select(ID, reference_cite)%>%
  filter(ID == 27)

