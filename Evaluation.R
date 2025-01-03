#Evaluation----
#This file contains different parts of the Evaluation. Parts which are not included in this file, where created using Excel.

library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(stringr)
library(cowplot)
library(Hmisc)
library(corrplot)
library(purrr)
library(tibble)

enviro<- read_csv2("enviro_fix.csv")|> rename(site= study_site)

enviro$river <- c(rep("Glenner",6),rep("Sense",2), rep("Thur",2), rep("Verzasca",3), rep("Landquart", 4), rep("Moesa",7),rep("Plessur",3), rep("Sitter",6), rep("Saane",6),rep("Ticino",3), rep("Vorderrhein",5))

metrics_neu <-read.csv("20241030_Export_Metrics_fie_lab_2.csv",sep=";",dec=".") 

metrics_alt<-read_csv("20240718_Export_Metrics_fie_lab.csv") 

metrics_alt<-metrics_alt[1:85,3:23]

metrics_alt <-subset(metrics_alt, select=-c(dom_inter,dom_surf,hp_score))

metric_change <-subset(metrics_neu, select=c(site,method,dom_inter,dom_surf,dom_surf_lentic,hp_sen,MMI_Labor_5,MMI_Labor_3,MMI_Field_5,MMI_Field_3,MMI_Combi_5,MMI_Combi_3, CEFI_Arm))

metrics<- left_join(metric_change, metrics_alt,join_by(site,method))

cols.num <- c(colnames(metrics[,3:28]))


remove_sites<-c("MR","PR","SAR","SR","LR","SA5","VR6")#remove sites

metrics <-metrics[!metrics$site %in% remove_sites, ]
#colnames(metrics)
metrics<-metrics[,-c(7:12)]#remove mmi


# mmi <-read_csv("20240820_Export_Metrics_fie_lab.csv")|>  rename(cefi_Arm = CEFI_Arm )

metrics$hp_sen <-as.numeric(metrics$hp_sen)

# mmi_lab <-filter(mmi[,c(3,4,20,24,31:36)], method == "lab")
# mmi_fie <-filter(mmi[,c(3,4,20,24,31:36)], method == "fie")

metrics_lab1 <-filter(metrics, method == "lab")
metrics_fie1 <-filter(metrics, method == "fie")

metrics_lab <- metrics_lab1|> inner_join(enviro)
metrics_fie <- metrics_fie1|>  inner_join(enviro)


## Correlation between numeric variables----
###metric ----
env_cor_num <- enviro[,c(2,9,10,43,58,59,63,66)]
env_cor_fact <- enviro[,c(2,44,65)]
colnames(metrics_fie1)

met_fie_cor<-metrics_fie1[,c(1,3:23)]
met_lab_cor<-metrics_lab1[,c(1,3:23)]

met_fie_num<-inner_join(env_cor_num, met_fie_cor)[,-1]
met_fie_fac<-inner_join(env_cor_fact, met_fie_cor)[,-1]
met_lab_num<-inner_join(env_cor_num, met_lab_cor)[,-1]
met_lab_fac<-inner_join(env_cor_fact, met_lab_cor)[,-1]

res_met_fie_num<-rcorr(as.matrix(met_fie_num),type="pearson")
res_met_fie_num_r <-as.data.frame(round((res_met_fie_num$r),3))[8:28,1:7]
res_met_fie_num_p<-as.data.frame(round((res_met_fie_num$P),3))[8:28,1:7]
colnames(res_met_fie_num_p) <- paste( colnames(res_met_fie_num_p), "p",sep = '_')
colnames(res_met_fie_num_r) <- paste( colnames(res_met_fie_num_r), "r",sep = '_')
res_met_fie_num_pr <-cbind(res_met_fie_num_p, res_met_fie_num_r)
res_met_fie_num_pr <-res_met_fie_num_pr |>  select(order(colnames(res_met_fie_num_pr)))
write.csv2(res_met_fie_num_pr,file = "other_outputs/res_met_fie_num_pr_update.csv")

res_met_lab_num<-rcorr(as.matrix(met_lab_num),type="pearson")
res_met_lab_num_r <-as.data.frame(round((res_met_lab_num$r),3))[8:28,1:7]
res_met_lab_num_p<-as.data.frame(round((res_met_lab_num$P),3))[8:28,1:7]
colnames(res_met_lab_num_p) <- paste( colnames(res_met_lab_num_p),"p", sep = '_')
colnames(res_met_lab_num_r) <- paste( colnames(res_met_lab_num_r),"r", sep = '_')
res_met_lab_num_pr <-cbind(res_met_lab_num_p, res_met_lab_num_r)
res_met_lab_num_pr <-res_met_lab_num_pr |>  select(order(colnames(res_met_lab_num_pr)))

write.csv2(res_met_lab_num_pr,file = "other_outputs/res_met_lab_num_pr_update.csv")

##Differences between factors (Anova and Tukey- Tests)----
#bioregion
met_fie_fac$natural_hydrological_regime_type <-as.factor(met_fie_fac$natural_hydrological_regime_type)

met_fie_fac_bio <-(met_fie_fac[,-c(1)])

anova_results_fie_bio <- purrr::map(met_fie_fac_bio[,2:22], ~summary(aov(.x ~ met_fie_fac_bio$biogeographical_region)))

aov_fie_bio<- as.data.frame(do.call(cbind, anova_results_fie_bio))

as <- function(i) {
  TukeyHSD(aov(i~biogeographical_region, data=met_fie_fac_bio), conf.level=.95)
}

as(i= met_fie_fac_bio$hp_sen)
as(i= met_fie_fac_bio$DI)
as(i= met_fie_fac_bio$dom_inter)
as(i= met_fie_fac_bio$k_index)
as(i= met_fie_fac_bio$mar_div)
as(i= met_fie_fac_bio$nr_taxa)
as(i= met_fie_fac_bio$nr_ept_tax)
as(i= met_fie_fac_bio$nr_ple_taxa)
as(i= met_fie_fac_bio$Egat)
as(i= met_fie_fac_bio$Ewei)
as(i= met_fie_fac_bio$cefi)
as(i= met_fie_fac_bio$MMI_HP)
as(i= met_fie_fac_bio$CEFI_Arm)

met_lab_fac$natural_hydrological_regime_type <-as.factor(met_lab_fac$natural_hydrological_regime_type)

met_lab_fac_bio <-(met_lab_fac[,-c(1)])

anova_results_lab_bio <- purrr::map(met_lab_fac_bio[,2:22], ~summary(aov(.x ~ met_lab_fac_bio$biogeographical_region)))

aov_lab_bio<- as.data.frame(do.call(cbind, anova_results_lab_bio))


as2 <- function(i) {
  TukeyHSD(aov(i~biogeographical_region, data=met_lab_fac_bio), conf.level=.95)
}

as2(i= met_lab_fac_bio$hp_sen)
as2(i= met_lab_fac_bio$DI)
as2(i= met_lab_fac_bio$dom_inter)
as2(i= met_lab_fac_bio$dom_lentic)
as2(i= met_lab_fac_bio$dom_surf_lentic)
as2(i= met_lab_fac_bio$k_index)
as2(i= met_lab_fac_bio$mar_div)
as2(i= met_lab_fac_bio$nr_taxa)
as2(i= met_lab_fac_bio$nr_ept_tax)
as2(i= met_lab_fac_bio$nr_ple_taxa)
as2(i= met_lab_fac_bio$lzi)
as2(i= met_lab_fac_bio$MMI_HP)
as2(i= met_lab_fac_bio$dom_surf)
#abflussregime

ttfie <- function(i) {
  t.test(i ~ natural_hydrological_regime_type, data =met_fie_fac)
}

ttlab <- function(i) {
  t.test(i ~ natural_hydrological_regime_type, data =met_lab_fac)
}

ttfie(i= met_fie_fac$hp_sen)
ttfie(i= met_fie_fac$DI)
ttfie(i= met_fie_fac$k_index)
ttfie(i= met_fie_fac$IBCH)
ttfie(i= met_fie_fac$DK_IBCH)
ttfie(i= met_fie_fac$sha_div)
ttfie(i= met_fie_fac$mar_div)
ttfie(i= met_fie_fac$nr_taxa)
ttfie(i= met_fie_fac$nr_ept_tax)
ttfie(i= met_fie_fac$nr_ple_taxa)
ttfie(i= met_fie_fac$dom_surf)
ttfie(i= met_fie_fac$dom_inter)
ttfie(i= met_fie_fac$dom_lentic)
ttfie(i= met_fie_fac$dom_lotic)
ttfie(i= met_fie_fac$Egat)
ttfie(i= met_fie_fac$Ewei)
ttfie(i= met_fie_fac$cefi)
ttfie(i= met_fie_fac$lzi)
ttfie(i= met_fie_fac$MMI_HP)
ttfie(i= met_fie_fac$CEFI_Arm)
ttfie(i= met_fie_fac$dom_surf_lentic)



ttlab(i= met_lab_fac$hp_sen)
ttlab(i= met_lab_fac$DI)
ttlab(i= met_lab_fac$k_index)
ttlab(i= met_lab_fac$IBCH)
ttlab(i= met_lab_fac$DK_IBCH)
ttlab(i= met_lab_fac$sha_div)
ttlab(i= met_lab_fac$mar_div)
ttlab(i= met_lab_fac$nr_taxa)
ttlab(i= met_lab_fac$nr_ept_tax)
ttlab(i= met_lab_fac$nr_ple_taxa)
ttlab(i= met_lab_fac$dom_surf)
ttlab(i= met_lab_fac$dom_inter)
ttlab(i= met_lab_fac$dom_lentic)
ttlab(i= met_lab_fac$dom_lotic)
ttlab(i= met_lab_fac$Egat)
ttlab(i= met_lab_fac$Ewei)
ttlab(i= met_lab_fac$cefi)
ttlab(i= met_lab_fac$lzi)
ttlab(i= met_lab_fac$MMI_HP)
ttlab(i= met_lab_fac$CEFI_Arm)
ttlab(i= met_lab_fac$dom_surf_lentic)



metrics_fie<-metrics_fie |> 
  rename(CEFI = cefi,
         dom_surf_lenit = dom_surf_lentic,
         nr_ept_taxa = nr_ept_tax)

metrics_lab<-metrics_lab |> 
  rename(CEFI = cefi,
         dom_surf_lenit = dom_surf_lentic,
         nr_ept_taxa = nr_ept_tax)


my_levels=c("CEFI","CEFI_Arm","DI","DK_IBCH", "dom_inter" ,    
"dom_lentic","dom_lotic","dom_surf","dom_surf_lenit","Egat","Ewei","hp_sen" , "IBCH" ,"k_index","lzi","mar_div","sha_div" ,"nr_ept_taxa","nr_ple_taxa","nr_taxa","MMI_HP")

my_palette <- brewer.pal(name="Paired",n=11)[c(1:10,12)]



##Biogeographical_region- plots----
###Field data----
biogeographical_met_fie <-melt(setDT(metrics_fie [,c(1,3:23, 87, 97)] ), id.vars = c("site","biogeographical_region","river"), variable.name = "metrics")|>
 mutate(across(metrics, ~factor(., levels=my_levels))) 

biogeographical_met_fie$biogeographical_region[biogeographical_met_fie$biogeographical_region =="Southern Alps"] <- "SA"
biogeographical_met_fie$biogeographical_region[biogeographical_met_fie$biogeographical_region =="Eastern Central Alps"] <- "ECA" 
biogeographical_met_fie$biogeographical_region[biogeographical_met_fie$biogeographical_region =="Central Plateau"] <- "CP"
  
ggplot(biogeographical_met_fie, aes(x= value, y= biogeographical_region, fill=river, color=river))+
  geom_dotplot(stackdir = "center")+
  coord_flip()+
  facet_wrap(metrics ~ ., scales="free")+
  labs(x = "",
       y = "")+
  guides(colour=guide_legend(title="Fliessgewässer"), fill=guide_legend(title="Fliessgewässer"))+
  theme_bw()+ 
  scale_fill_manual(values = my_palette)+
  scale_color_manual(values = my_palette)+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("images/evaluation/biogeo_met_fie.png", width=25, height=20, unit="cm")

###Lab data ----
biogeographical_met_lab <-melt(setDT(metrics_lab [,c(1, 3:23, 87, 97)] ), id.vars = c("site","biogeographical_region","river"), variable.name = "metrics")|>
  mutate(across(metrics, ~factor(., levels=my_levels))) 

biogeographical_met_lab$biogeographical_region[biogeographical_met_lab$biogeographical_region =="Southern Alps"] <- "SA"
biogeographical_met_lab$biogeographical_region[biogeographical_met_lab$biogeographical_region =="Eastern Central Alps"] <- "ECA" 
biogeographical_met_lab$biogeographical_region[biogeographical_met_lab$biogeographical_region =="Central Plateau"] <- "CP"

ggplot(biogeographical_met_lab, aes(x= value, y= biogeographical_region, fill=river, color= river))+
  geom_dotplot(stackdir = "center")+
  coord_flip()+
  facet_wrap(metrics ~ ., scales="free")+
  labs(x = "",
       y = "")+
  guides(colour=guide_legend(title="Fliessgewässer"), fill=guide_legend(title="Fliessgewässer"))+
  theme_bw()+ 
  scale_fill_manual(values = my_palette)+
  scale_color_manual(values = my_palette)+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
ggsave("images/evaluation/biogeo_met_lab.png", width=25, height=20, unit="cm")

#Natural_hydrological_regime_type-plots ----
###Field data----
hydrological_reg_met_fie <-melt(setDT(metrics_fie [,c(1,3:23, 66, 97)] ), id.vars = c("site","natural_hydrological_regime_type","river"), variable.name = "metrics")|>
  mutate(across(metrics, ~factor(., levels=my_levels))) 

ggplot(hydrological_reg_met_fie, aes(x= value, y= natural_hydrological_regime_type, fill=river, color=river))+
  geom_dotplot(stackdir = "center")+
  coord_flip()+
  facet_wrap(metrics ~ ., scales="free")+
  labs(x = "",
       y = "")+
  guides(colour=guide_legend(title="Fliessgewässer"), fill=guide_legend(title="Fliessgewässer"))+
  theme_bw()+ 
  scale_fill_manual(values = my_palette)+
  scale_color_manual(values = my_palette)+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("images/evaluation/hydrological_reg_met_fie.png", width=25, height=20, unit="cm")

###Lab data----
hydrological_reg_met_lab <-melt(setDT(metrics_lab [,c(1,3:23, 66, 97)] ), id.vars = c("site","natural_hydrological_regime_type","river"), variable.name = "metrics")|>
  mutate(across(metrics, ~factor(., levels=my_levels))) 

ggplot(hydrological_reg_met_lab, aes(x= value, y= natural_hydrological_regime_type, fill=river, color= river))+
  geom_dotplot(stackdir = "center")+
  coord_flip()+
  facet_wrap(metrics ~ ., scales="free")+
  labs(x = "",
       y = "")+
  guides(colour=guide_legend(title="Fliessgewässer"), fill=guide_legend(title="Fliessgewässer"))+
  theme_bw()+ 
  scale_fill_manual(values = my_palette)+
  scale_color_manual(values = my_palette)+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("images/evaluation/hydrological_reg_met_lab.png", width=25, height=20, unit="cm")


#Number of hydrological predictors
pa1<-ggplot(bericht, aes(x= reorder(metric, -number_of_hydrological_variables), y= number_of_hydrological_variables, fill=number_of_hydrological_variables))+
  scale_fill_gradientn(values = c(1,0), colours = c("black", "white"))+
  geom_bar(stat = "identity", color="black", linewidth= 0.3 ,show.legend = FALSE)+
  labs(x = "",y = "Anzahl hydrologische Prädiktoren")+
  coord_flip()+  
  theme_bw() +
  scale_x_discrete(limits=rev)+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank())+
  scale_y_continuous(limits = c(0,15), expand = expansion(mult = c(0, 0.0)))

#Number of hydraulic predictors
pa4<-ggplot(bericht, aes(x= reorder(metric, -number_of_hydraulic_variables), y= number_of_hydraulic_variables, fill=number_of_hydraulic_variables))+
  scale_fill_gradientn(values = c(1,0), colours = c("black", "white"))+
  geom_bar(stat = "identity", color="black", linewidth= 0.3 ,show.legend = FALSE)+
  labs(x = "",y = "Anzahl hydraulische Prädiktoren")+
  coord_flip()+  
  theme_bw()+
  scale_x_discrete(limits=rev)+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank())+
  scale_y_continuous(limits = c(0,15), expand = expansion(mult = c(0, 0.0)))


#Percentage of hydrological predictors
pa2<-ggplot(bericht, aes(x= reorder(metric, -percentage_of_hydrological_variables), y= percentage_of_hydrological_variables, fill=percentage_of_hydrological_variables))+
  scale_fill_gradientn(values = c(1,0), colours = c("black", "white"))+  
  geom_bar(stat = "identity", color="black", linewidth= 0.3,show.legend = FALSE)+
  labs(x = "",y = "Anteil hydrologischer Prädiktoren")+
  coord_flip()+  
  theme_bw()+
  scale_x_discrete(limits=rev)+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank())+
  scale_y_continuous(limits = c(0,100), expand = expansion(mult = c(0, 0.0)))

#Percentage of hydraulic predictors
pa5<-ggplot(bericht, aes(x= reorder(metric, -percentage_of_hydraulic_variables), y= percentage_of_hydraulic_variables, fill=percentage_of_hydraulic_variables))+
  scale_fill_gradientn(values = c(1,0.7,0), colours = c("black", "white"))+
  geom_bar(stat = "identity", color="black", linewidth= 0.3,show.legend = FALSE)+
  labs(x = "",y = "Anteil hydraulischer Prädiktoren")+
  coord_flip()+  
  theme_bw()+
  scale_x_discrete(limits=rev)+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank())+
  scale_y_continuous(limits = c(0,100), expand = expansion(mult = c(0, 0.0)))

plot_grid(pa1, pa2, pa4, pa5, nrow = 2, scale= 0.95)

ggsave("images/Anzahl_Anteil.jpg", width=25, height=30, unit="cm")
