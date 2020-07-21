library(tidyverse)
library(wesanderson)
library(ggpubr)
library(grid)
library(scales)
library(openxlsx)
library(plotly)

# This script is a function that plots species based on their growth depth and coverage.
# The input file(rds) is based on VELMU-survey data
# Further questions contact rasmus.boman@metsa.fi

# Location of the data set used. 
MP <- read_rds("C:/Users/Rasmusbo/Documents/AAMasterVelmu/data/sijanti_lajit_Master.rds") 

# Removing all non-algae / non-vascular species and combining some that have been named differently in different times
MPmut <- MP %>%
  mutate(Laji = case_when(str_detect(Laji, "Fucus") ~ "Fucus vesiculosus",
                          str_detect(Laji, "pectinat") ~ "Potamogeton pectinatus",
                          str_detect(Laji, "aegagro|Aegagro") ~ "Aegagropila linnaei",
                          str_detect(Laji, "Ulva") ~ "Ulva sp.",
                          str_detect(Laji, "Sparganium") ~ "Sparganium sp.",
                          str_detect(Laji, "loveni") ~ "Laomedea loveni",
                          str_detect(Laji, "Alisma plantago") ~ "Alisma plantago-aquatica",
                          str_detect(Laji, "Hildenbrandia") ~ "Hildenbrandia rubra",
                          str_detect(Laji, "purpureum") ~ "Rhodochorton purpureum",
                          TRUE ~ as.character(Laji))) %>%
  filter(!is.na(Laji))

# Summarising depth information on separate table
master_summarised <- MPmut %>%
  group_by(Laji) %>%
  summarise(
    min_1pros = quantile(Syvyys, .01, na.rm = T),
    max_1pros = quantile(Syvyys, .99, na.rm = T),
    KA_kasvusyvyys = mean(Syvyys,na.rm = T),
    Mediaani_kasvusyvyys = median(Syvyys, na.rm = T),
    Keskihajonta = sd(Syvyys, na.rm = T),
    min_5_pros = quantile(Syvyys, .05, na.rm = T),
    max_5_pros = quantile(Syvyys, .95, na.rm = T),
    min_syvyys = min(Syvyys, na.rm = T),
    max_syvyys = max(Syvyys, na.rm = T),
    havaintojen_lkm = n()) %>%
  arrange(desc(havaintojen_lkm)) %>%
  mutate(Lajin_90_pros_esiintyvyysrajat = paste(round(min_5_pros, digits = 2), "-", round(max_5_pros, digits = 2)),
         lajihavainto = Laji) # Stripping 10% of both ends

#Writing the summerised table into separate xlsx-table if needed
write.csv(master_summarised, "C:/Users/Rasmusbo/Documents/R_shiny/2020VELMU_readxl_tarkistus/Read_surveydata_in/external_data/VesikasviSummary.csv")

comb_data <- MP %>%
  left_join(master_summarised, by = "Laji")

outliers <- comb_data %>%
  mutate(Outlier = case_when(Syvyys < min_1pros ~ "Matala",
                             Syvyys > max_1pros ~ "Syva",
                             TRUE ~ "NA")) %>%
  select(Y_wgs84, 
         X_wgs84,
         Kohteen.nimi,
         Kartoitus.pvm,
         Laji,
         Syvyys,
         Peittavyys,
         Outlier,
         Lajin_90_pros_esiintyvyysrajat,
         min_1pros,
         max_1pros) %>%
  filter(Outlier != "NA")

print(outliers, n = 30)























### PLOT FUNCTION ###

Draw_sp_plots <- function(tiet_nimi){
  
  # Filtering the species out from larger dataframe:
  
  FiltLaji <- MPmut %>% #MPmut hardcoded into the function, change this into parameter if necessary
    filter(Laji == tiet_nimi) %>%
    filter(Syvyys > quantile(Syvyys, 0.005, na.rm = T), #Removing outliers both in coverage as well as depth.
           Syvyys < quantile(Syvyys, 0.99, na.rm = T),
           Peittavyys > quantile(Peittavyys, 0.001, na.rm = T),
           Peittavyys < quantile(Peittavyys, 0.99, na.rm = T))
  
  FiltSumm <- master_summarised %>% # For extra information bring in another dataframe with summarised info
    filter(Laji == tiet_nimi) # Extract the row with current species
  
  # Extracting describing values for plot annotations:
  
  mediaani <- round(FiltSumm[1,5], digits = 1)
  keskihaj <- round(FiltSumm[1,3], digits = 1)
  matala <- round(FiltSumm[1,6], digits = 1)
  syva <- round(FiltSumm[1,7], digits = 1)
  vaihtelu <- FiltSumm[1,4]
  
  # Preparing annotation box for plot
  
  grob_descr <- grobTree(textGrob(paste("Matalimmalla havaittu yksil?: ", matala, "m", sep = " "), x = 0.7, y = 0.97, just = "left", gp = gpar(fontsize = 10)),
                         textGrob(paste("Syvimm?ll? havaittu yksil?:", syva, "m", sep = " "), x = 0.7, y = 0.94, just = "left", gp = gpar(fontsize = 10)),
                         textGrob(paste("Kasvusyvyyksien mediaani:", mediaani, "m", sep = " "), x = 0.7,  y = 0.91, just = "left", gp=gpar(fontsize=10)),
                         textGrob(paste("Kasvusyvyyksien keskihajonta: ", keskihaj, "m", sep = " "), x = 0.7, y = 0.88, just = "left", gp = gpar(fontsize = 10)),
                         textGrob(paste("Lajihavainnoista 80% syvyysv?lill?:", vaihtelu, "m", sep = " "), x = 0.7, y = 0.85, just = "left", gp = gpar(fontsize = 10)))
  
  
  Lajinimi <- FiltLaji$Laji[1] # Removing the species for title of plot
  Havaintomaara <- FiltSumm$havaintojen_lkm[1] # Counting the nr of observations for subtitle of plot
  row_nr <- which(master_summarised$Laji == tiet_nimi) # Row number for saving plots in order
  pal <- wes_palette("Zissou1", 100, type = "continuous") #Defining the palette from wes anderson palette
  
  # Changing the size and transparency of plot points depending on the nr of observations
  
  Point_size <- case_when(Havaintomaara > 10000 ~ 0.5,
                          Havaintomaara <= 10000 && Havaintomaara > 5000 ~ 0.6,
                          Havaintomaara <= 5000 && Havaintomaara > 1000 ~ 0.8,
                          Havaintomaara <= 1000 && Havaintomaara > 500 ~ 1,
                          Havaintomaara <= 500 && Havaintomaara > 200 ~ 1.5,
                          Havaintomaara <= 200 ~ 2)
  
  point_alpha <- case_when(Havaintomaara > 10000 ~ 0.15,
                           Havaintomaara <= 10000 && Havaintomaara > 5000 ~ 0.25,
                           Havaintomaara <= 5000 && Havaintomaara > 1000 ~ 0.35,
                           Havaintomaara <= 1000 && Havaintomaara > 500 ~ 0.45,
                           Havaintomaara <= 500 && Havaintomaara > 200 ~ 0.55,
                           Havaintomaara <= 200 ~ 0.6)
  
  bins_in_d <- case_when(Havaintomaara > 10000 ~ 10,
                         Havaintomaara <= 10000 && Havaintomaara > 5000 ~ 9,
                         Havaintomaara <= 5000 && Havaintomaara > 1000 ~ 9,
                         Havaintomaara <= 1000 && Havaintomaara > 500 ~ 8,
                         Havaintomaara <= 500 && Havaintomaara > 150  ~ 6,
                         Havaintomaara <= 150 ~ 5)
  
  ggg <- grobTree(rectGrob(gp=gpar(fill="grey", alpha=0.15), x = 0.69, y = 0.91, width = 0.3, height = 0.15, just = "left" ))                         
  
  Lajiplot <- FiltLaji %>%
    ggplot(aes(x = Syvyys, y = Peittavyys))+
    stat_density_2d(aes(fill = stat(level), alpha = 0.8), geom = "polygon", bins = bins_in_d, show.legend = F) +
    geom_jitter(height = 2, width = 0.1, alpha = point_alpha, size = Point_size, fill = "black")  +
    scale_fill_gradientn(colours = pal) +
    scale_y_continuous(limits = c(-0.1 ,NA), breaks = seq(0, 110, 20), oob = squish) + #squish forces all the points scattered below 0 back to 0 axis
    scale_x_continuous(limits = c(0 ,NA), oob = squish) +
    theme_minimal() +
    labs(title= Lajinimi,
         subtitle = paste("Havaintojen lukum??r?:", Havaintomaara),
         y = "Peitt?vyys (%)", 
         x = "Kasvusyvyys (m)") +
    theme(plot.title = element_text(face = "italic", size = 14, hjust = 0),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(color = "grey20", size = 13, hjust = 0.5, face = "plain"),
          axis.title.y = element_text(color = "grey20", size = 13, hjust = 0.5, face = "plain")) +
    annotation_custom(grob = grob_descr) +
    annotation_custom(grob = ggg)
  
  #Save the plot to desired folder:
  ggsave(paste("C:/Users/Rasmusbo/Documents/Lajikuvaajat2/", row_nr, ". ", Lajinimi, ".jpeg", sep = ""),
         #device = "jpeg",
         units = "cm",
         width = 30,
         height = 22)
  
}

# Call the function:
# "Possibly" works as a tryCatch statement, which doesn't stop the function in case of error
# purrr:map loops over the species defined in lajit_testi in the function

lajit_testi <- master_summarised$Laji[1:180]
map(lajit_testi, possibly(Draw_sp_plots, otherwise = NA)) 




################################
# Below manual plot drawing if you want change the looks of the plot
################################

# The list of species:
# master_summarised$Laji

LajiMP <- MPmut %>%
  filter(Laji == "Zannichellia sp.") #Species name here

zan <- LajiMP %>% 
  filter(Syvyys > quantile(Syvyys, 0.01, na.rm = T),
         Syvyys < quantile(Syvyys, 0.99, na.rm = T),
         Peittavyys > quantile(Peittavyys, 0.01, na.rm = T),
         Peittavyys < quantile(Peittavyys, 0.99, na.rm = T)) %>%
  ggplot(aes(x = Syvyys, y = Peittavyys))+
  stat_density_2d(aes(fill = stat(level)), geom = "polygon", bins = 10) +
  geom_jitter(color = "black", height = 2, width = 0.1, alpha = 0.2, size = 0.5)  +
  scale_fill_gradient(low = "green", high = "red") +
  scale_y_continuous(limits = c(0,NA), breaks = seq(0, 110, 20), oob = squish) +
  scale_x_continuous(limits = c(0,NA), oob = squish) +
  theme_light() +
  ggtitle(LajiMP$Laji[1]) +
  theme(axis.title.y = element_text(color = "grey20", size = 13, hjust = 0.5, vjust = 0, face = "plain")) 

ggplotly(zan)
