#Preprocessing ----
library("readr")
library("dplyr")
library("tidyr")
library("janitor")
library("DescTools")
library("corrplot")
set.seed(12)

##Calculating midpoint of coordinates of sites for GIS- Work----
coordinates<-read_delim("Abiotic_MI_Field_corr.txt",delim="\t", col_names=FALSE, col_types = list(col_character(), col_number(),col_number(),col_number()))

names(coordinates)<-c("site_prez","X","Y","Z")#give columns names

coordinates$site <-sub("\\-.*", "", coordinates$site_prez)#extract site

coordinates <- coordinates |> 
  group_by(site) |> 
  summarise(X=mean(X), Y=mean(Y), Z=mean(Z))#summarise by site

#write.csv2(coordinates, file = "coordinates.csv")# to write csv
rm(coordinates)#remove, as not needed anymore

##Overall Data cleanup for environmental variables----
enviro<- read_delim("Environmental_Variables_FINAL_5.csv", delim=";")

###Fix column titles ----
enviro<-clean_names(enviro)

###Replacing "-" with NA ----
enviro<-enviro |> 
  mutate(across(where(is.character), ~na_if(., "-")))

###Characters to numeric ----
char_num_cols <-c(58,71,73)#columns we need to change

enviro <- enviro |> 
  mutate_at(char_num_cols, as.numeric)#conversion to numeric

###Drop information variables -----
dropstrings2 <-c("reliabil","flow_model","river", "elevation_hp_release_m_a_s_l", "catchment_size_upstream_of_hp_release_km2")#
mymatches2<-paste(dropstrings2, collapse = "|")
enviro<-enviro|> ungroup () |> select(-matches(mymatches2))
enviro<-subset(enviro, select=-c(tributaries))

###pre-Split data by flow regime for different models----
natural1 <-enviro[enviro$flow_regime == "Natural/near-natural",]
hydropeaking1 <-enviro[enviro$flow_regime == "Hydropeaking",]

###Drop all but shortest distance to HP- release per site hydropeaking----
hydropeaking1 <- hydropeaking1[hydropeaking1$distance_study_site_to_hp_release_km >= 0, ]
hydropeaking1<-hydropeaking1 |> 
  group_by(study_site) |> 
  filter(distance_study_site_to_hp_release_km == Closest(distance_study_site_to_hp_release_km, 0, na.rm = T)) 

###fill distance to study site for natural ----
natural1<- natural1 |> 
  mutate(distance_study_site_to_hp_release_km  = replace(distance_study_site_to_hp_release_km, is.na(distance_study_site_to_hp_release_km), 100000))

###merge sets back together
enviro <-bind_rows(natural1, hydropeaking1)

###Create fake variables for the whole dataset----
set.seed(279)
enviro$fake_A <- rnorm(47,mean=15, sd=8)#fake A: Min=2.924897, Max=31.85385
set.seed(948)
enviro$fake_B <- runif(47,min= 0, max = 100)#fake B: Min=1.001196, Max=98.32158
set.seed(687)
enviro$fake_C <- sample(c("A","B","C"),size=47,replace=TRUE, prob=c(1,1,1))#A 17, B 15, C 15

##Checking variables for the distribution of the values of their variables ----
hist(enviro$ratiodc_median)#example for plotting distribution

##Checking variables for correlation ----
cor_all <- base::Filter(is.numeric, enviro)
cor_all <-cor_all[,-c(23,26), drop=FALSE]
cor_mat_all<-round(cor(cor_all), 3)

cor_mat_df<-as.data.frame(cor_mat_all)
write_csv2(cor_mat_df, file = "other_outputs/cor_all.csv")# write csv
#cor_mat_all[abs(cor_mat_all)< 0.4]<-NA#remove weak correlations
#cor_mat_all[(cor_mat_all)== 1]<-NA#remove perfect correlations
#cor_mat_all <- cor_mat_all[rowSums(is.na(cor_mat_all)) != ncol(cor_mat_all), colSums(is.na(cor_mat_all))<nrow(cor_mat_all)]#remove all rows and columns only consisting of NAs

testRes = cor.mtest(cor_mat_all, conf.level = 0.95)

corrplot(cor_mat_all, p.mat = testRes$p, sig.level = 0.05, method = 'circle', type = 'upper', insig='blank',number.cex = 0.25,  diag=FALSE, tl.cex = 0.4, tl.col="black")

##addCoef.col ='black',

#new
corrplot(cor_mat_all, p.mat = testRes$p, method = 'color', type = 'upper',number.cex = 0.25,  diag=FALSE, tl.cex = 0.4, tl.col="black", sig.level = c(0.001, 0.01, 0.05),insig = 'label_sig',pch.cex = 0.3)

#new

corrplot(cor_mat_all, method="circle",type="upper",tl.cex=0.4, na.label = "square", na.label.col = "white",  tl.col="black")

corsums <- function(df, val1, val2, val3) {
  list(
    big_negative =sum(df > -val1 & df <= -val2, na.rm=TRUE)/2, 
    mid_negative =sum(df > -val2 & df <= -val3, na.rm=TRUE)/2,
    small_negative =sum(df > -val3 & df <0, na.rm=TRUE)/2,
    small_positive =sum(df < val3 & df >0, na.rm=TRUE)/2,
    mid_positive=sum(df < val2 & df >= val3, na.rm=TRUE)/2,
    big_positive =sum(df < val1 & df >= val2, na.rm=TRUE)/2
  )}
corsums(df=cor_mat_all, val1= 1, val2= 0.9, val3= 0.7)

##Data cleanup----
dropstring_clean <- c("cpom_cover_1_3_median","moos_cover_1_3_median","hp_support")
mymatches3<-paste(dropstring_clean, collapse = "|")
enviro<-enviro |> select(-matches(mymatches3))#remove columns with NAs, and colums, where the values are equal for all sites


##Make subsets for the models based on macroinvertebrate- data from the laboratory ----
labsites <-c("M1","M5","M7","TI1","TI2","L1","L2","P1","PR","S1","S2","S5","SA1","SA2","SA6","GL1","GL2", "GL5","VR1","VR3","SE2", "SE3","TH3", "TH4","VE1", "VE3")

labenviro <-enviro[enviro$study_site %in% labsites,]

##Export datasets
write_csv2(enviro, "enviro_fix.csv")
write_csv2(labenviro, "labenviro_fix.csv")

## Merge with macroinvertebrate indices for each index----
metrics<- read_delim("20240718_Export_Metrics_fie_lab.csv", ",")
metrics2<-read_delim("20241030_Export_Metrics_fie_lab.csv", ";")
metrics <- metrics |> rename(study_site= site)
metrics2 <- metrics2 |> rename(study_site= site)

metrics_lab <-metrics[metrics$method == "lab",]
metrics_field <-metrics[metrics$method == "fie",]

metrics2_lab <-metrics2[metrics2$method == "lab",]
metrics2_field <-metrics2[metrics2$method == "fie",]

##Create data frames for metrics and different sites and export them----



###DI ----
field_DI <- enviro|> left_join(metrics_field |> select(study_site,DI))
write_csv2(field_DI, file = "model_data/field_DI.csv")

lab_DI <- labenviro|> left_join(metrics_lab|> select(study_site,DI))
write_csv2(lab_DI, file = "model_data/lab_DI.csv")


###k-index ----
field_k_index <- enviro|> left_join(metrics_field |> select(study_site,k_index))
write_csv2(field_k_index, file = "model_data/field_k_index.csv")

lab_k_index <- labenviro|> left_join(metrics_lab|> select(study_site,k_index))
write_csv2(lab_k_index, file = "model_data/lab_k_index.csv")


###IBCH----
field_IBCH <- enviro|> left_join(metrics_field |> select(study_site,IBCH))
write_csv2(field_IBCH, file = "model_data/field_IBCH.csv")

lab_IBCH <- labenviro|> left_join(metrics_lab|> select(study_site,IBCH))
write_csv2(lab_IBCH, file = "model_data/lab_IBCH.csv")


###DK_IBCH----
field_DK_IBCH <- enviro|> left_join(metrics_field |> select(study_site,DK_IBCH))
write_csv2(field_DK_IBCH, file = "model_data/field_DK_IBCH.csv")

lab_DK_IBCH <- labenviro|> left_join(metrics_lab|> select(study_site,DK_IBCH))
write_csv2(lab_DK_IBCH, file = "model_data/lab_DK_IBCH.csv")


###sha_div ----
field_sha_div <- enviro|> left_join(metrics_field |> select(study_site,sha_div))
write_csv2(field_sha_div, file = "model_data/field_sha_div.csv")

lab_sha_div <- labenviro|> left_join(metrics_lab|> select(study_site,sha_div))
write_csv2(lab_sha_div, file = "model_data/lab_sha_div.csv")


###mar_div----
field_mar_div <- enviro|> left_join(metrics_field |> select(study_site,mar_div))
write_csv2(field_mar_div, file = "model_data/field_mar_div.csv")

lab_mar_div <- labenviro|> left_join(metrics_lab|> select(study_site,mar_div))
write_csv2(lab_mar_div, file = "model_data/lab_mar_div.csv")


###nr_taxa ----
field_nr_taxa <- enviro|> left_join(metrics_field |> select(study_site,nr_taxa))
write_csv2(field_nr_taxa, file = "model_data/field_nr_taxa.csv")

lab_nr_taxa <- labenviro|> left_join(metrics_lab|> select(study_site,nr_taxa))
write_csv2(lab_nr_taxa, file = "model_data/lab_nr_taxa.csv")


###nr_ept_tax ----
field_nr_ept_tax <- enviro|> left_join(metrics_field |> select(study_site,nr_ept_tax))
write_csv2(field_nr_ept_tax, file = "model_data/field_nr_ept_tax.csv")

lab_nr_ept_tax <- labenviro|> left_join(metrics_lab|> select(study_site,nr_ept_tax))
write_csv2(lab_nr_ept_tax, file = "model_data/lab_nr_ept_tax.csv")


###nr_ple-taxa ----
field_nr_ple_taxa <- enviro|> left_join(metrics_field |> select(study_site,nr_ple_taxa))
write_csv2(field_nr_ple_taxa, file = "model_data/field_nr_ple_taxa.csv")

lab_nr_ple_taxa <- labenviro|> left_join(metrics_lab|> select(study_site,nr_ple_taxa))
write_csv2(lab_nr_ple_taxa, file = "model_data/lab_nr_ple_taxa.csv")


###dom_lentic----
field_dom_lentic <- enviro|> left_join(metrics_field |> select(study_site,dom_lentic))
write_csv2(field_dom_lentic, file = "model_data/field_dom_lentic.csv")

lab_dom_lentic <- labenviro|> left_join(metrics_lab|> select(study_site,dom_lentic))
write_csv2(lab_dom_lentic, file = "model_data/lab_dom_lentic.csv")


###dom_lotic ----
field_dom_lotic <- enviro|> left_join(metrics_field |> select(study_site,dom_lotic))
write_csv2(field_dom_lotic, file = "model_data/field_dom_lotic.csv")

lab_dom_lotic <- labenviro|> left_join(metrics_lab|> select(study_site,dom_lotic))
write_csv2(lab_dom_lotic, file = "model_data/lab_dom_lotic.csv")


###Egat ----
field_Egat <- enviro|> left_join(metrics_field |> select(study_site,Egat))
write_csv2(field_Egat, file = "model_data/field_Egat.csv")

lab_Egat <- labenviro|> left_join(metrics_lab|> select(study_site,Egat))
write_csv2(lab_Egat, file = "model_data/lab_Egat.csv")


###Ewei ----
field_Ewei <- enviro|> left_join(metrics_field |> select(study_site,Ewei))
write_csv2(field_Ewei, file = "model_data/field_Ewei.csv")

lab_Ewei <- labenviro|> left_join(metrics_lab|> select(study_site,Ewei))
write_csv2(lab_Ewei, file = "model_data/lab_Ewei.csv")


###CEFI ----
field_cefi <- enviro|> left_join(metrics_field |> select(study_site,cefi))
write_csv2(field_cefi, file = "model_data/field_cefi.csv")

lab_cefi <- labenviro|> left_join(metrics_lab|> select(study_site,cefi))
write_csv2(lab_cefi, file = "model_data/lab_cefi.csv")


###LZI ----
field_lzi <- enviro|> left_join(metrics_field |> select(study_site,lzi))
write_csv2(field_lzi, file = "model_data/field_lzi.csv")

lab_lzi <- labenviro|> left_join(metrics_lab|> select(study_site,lzi))
write_csv2(lab_lzi, file = "model_data/lab_lzi.csv")


###MMI_HP ----
field_MMI_HP <- enviro|> left_join(metrics_field |> select(study_site,MMI_HP))
write_csv2(field_MMI_HP, file = "model_data/field_MMI_HP.csv")

lab_MMI_HP <- labenviro|> left_join(metrics_lab|> select(study_site,MMI_HP))
write_csv2(lab_MMI_HP, file = "model_data/lab_MMI_HP.csv")


## Merge with macroinvertebrate indices for each multimetric index----
MMI<- read_delim("20240820_Export_Metrics_fie_lab.csv", ",")
MMI <- MMI |> rename(study_site= site)

MMI_lab <-MMI[MMI$method == "lab",]
MMI_field <-MMI[MMI$method == "fie",]

##Create data frames for multimetric indices (+ some Metrics ) and different sites and export them----

###CEFI_Arm ----
field_CEFI_Arm <- enviro|> left_join(MMI_field |> select(study_site,CEFI_Arm))
write_csv2(field_CEFI_Arm, file = "model_data_MMI/field_CEFI_Arm.csv")

lab_CEFI_Arm <- labenviro|> left_join(MMI_lab|> select(study_site,CEFI_Arm))
write_csv2(lab_CEFI_Arm, file = "model_data_MMI/lab_CEFI_Arm.csv")

###taxa_surf_lenit ----
field_taxa_surf_lenit <- enviro|> left_join(MMI_field |> select(study_site,taxa_surf_lenit))
write_csv2(field_taxa_surf_lenit, file = "model_data_MMI/field_taxa_surf_lenit.csv")

lab_taxa_surf_lenit <- labenviro|> left_join(MMI_lab|> select(study_site,taxa_surf_lenit))
write_csv2(lab_taxa_surf_lenit, file = "model_data_MMI/lab_taxa_surf_lenit.csv")


###HP Sen----
field_hp_sen <- enviro|> left_join(metrics2_field |> select(study_site,hp_sen))
write_csv2(field_hp_sen, file = "model_data2/field_hp_sen.csv")

lab_hp_sen <- labenviro|> left_join(metrics2_lab|> select(study_site,hp_sen))
write_csv2(lab_hp_sen, file = "model_data2/lab_hp_sen.csv")

###dom_inter----
field_dom_inter <- enviro|> left_join(metrics2_field |> select(study_site,dom_inter))
write_csv2(field_dom_inter, file = "model_data2/field_dom_inter.csv")

lab_dom_inter <- labenviro|> left_join(metrics2_lab|> select(study_site,dom_inter))
write_csv2(lab_dom_inter, file = "model_data2/lab_dom_inter.csv")


###dom_surf----
field_dom_surf <- enviro|> left_join(metrics2_field |> select(study_site,dom_surf))
write_csv2(field_dom_surf, file = "model_data2/field_dom_surf.csv")

lab_dom_surf <- labenviro|> left_join(metrics2_lab|> select(study_site,dom_surf))
write_csv2(lab_dom_surf, file = "model_data2/lab_dom_surf.csv")

###dom_surf_lentic----
field_dom_surf_lentic <- enviro|> left_join(metrics2_field |> select(study_site,dom_surf_lentic))
write_csv2(field_dom_surf_lentic, file = "model_data2/field_dom_surf_lentic.csv")

lab_dom_surf_lentic <- labenviro|> left_join(metrics2_lab|> select(study_site,dom_surf_lentic))
write_csv2(lab_dom_surf_lentic, file = "model_data2/lab_dom_surf_lentic.csv")


