# Analyse field data from SUSALPS-RS field campaigns 2019-2020

# LAST MODIFIED:  21/10/2021 by AS 
# Background:     Study for multi-temporal estimation of vegetation parameters in 
#                 alpine & prealpine grasslands
# PURPOSE:        Bring field data from the experiment (2019-2021 sampling campaigns) in format for modelling
# INPUT:          dataframe with data from all sampling dates plus spectral data of UAS images (SEQ data)
# OUTPUT:         Table with field and spectral data without unnecessary columns and in suitable format
##############################################################################################################

library(tidyverse)
#library(ggplot2)
library(viridis) # color scales for those with colorblindness and that print well in grey scale
#library(reshape2)
#library(skimr)

path_base = "G:/BioAtmo/schucknecht-a/NEXTCLOUD/MultiTempGrass/"
path_data = "./InputData/"
path_out = "./DataExploration/UAS_effect_of_window_size/"

# set working directory
setwd(path_base)

# get date of system time for use in figure names
st <- format(Sys.time(), "%Y%m%d")

# get the csv-file with field and spectral data
df <- read_csv(paste(path_data, "SUSALPS-RS_UAS+Sat_samplingData_2019-2021_all_20211015_UASdata.csv", sep = ""), na = "NA")


length(which(!is.na(df$R_550_SEQ_W_1)))
colSums(!is.na(df))

### Compare spectral data with respect to window size -----

png(paste(path_out, "R550_w1_vs_w3", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_550_SEQ_W_1, y = R_550_SEQ_W_3, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R550_w1_vs_w5", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_550_SEQ_W_1, y = R_550_SEQ_W_5, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R550_w3_vs_w5", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_550_SEQ_W_3, y = R_550_SEQ_W_5, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R660_w1_vs_w3", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_660_SEQ_W_1, y = R_660_SEQ_W_3, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R660_w1_vs_w5", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_660_SEQ_W_1, y = R_660_SEQ_W_5, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R660_w3_vs_w5", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_660_SEQ_W_3, y = R_660_SEQ_W_5, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R735_w1_vs_w3", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_735_SEQ_W_1, y = R_735_SEQ_W_3, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R735_w1_vs_w5", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_735_SEQ_W_1, y = R_735_SEQ_W_5, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R735_w3_vs_w5", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_735_SEQ_W_3, y = R_735_SEQ_W_5, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R790_w1_vs_w3", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_790_SEQ_W_1, y = R_790_SEQ_W_3, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R790_w1_vs_w5", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_790_SEQ_W_1, y = R_790_SEQ_W_5, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

png(paste(path_out, "R790_w3_vs_w5", st, ".png", sep = ""), width=7, height=6, units="in", res=300)
ggplot(df, aes(x = R_790_SEQ_W_3, y = R_790_SEQ_W_5, colour = plot_ID))+
  geom_point()+
  geom_abline()+
  theme_bw() +
  theme(text=element_text(size = 16)) +
  scale_colour_viridis_d()
dev.off()

# remove all unnecessary columns of field data, transform depth information in new columns, rename columns, bring in new order
df <- df %>%
  select(!c(Portion_NG, Portion_L, Portion_F, Portion_G, FM, DM_NG, DM_L, DM_F, DM_G, DM_M, DM_total, 
            DM_kgha, FM_kgha, DM_sorted, SamplingDateSimplified)) %>%
  mutate(Subplot_ID = str_replace(Subplot_ID, "_2-7", "")) %>%
  pivot_wider(names_from = Depth, values_from = c(FM_gm2, DM_gm2, WC)) %>%
  rename(c(subplot_ID = Subplot_ID,
         quadrant = Quadrant,
         CH1 = Canopy_height, 
         soil_pt = Portion_soil, 
         comment = Comment, 
         sampling_date = SamplingDate,
         DM_gt7 = `DM_gm2_>7`,
         DM_sm7 = `DM_gm2_2-7`,
         FM_gt7 = `FM_gm2_>7`,
         FM_sm7 = `FM_gm2_2-7`,
         WC_gt7 = `WC_>7`,
         WC_sm7 = `WC_2-7`)) %>%
  rename_at(vars(contains("SEQ")), ~ str_remove(.,"_SEQ")) %>%
  relocate(comment, .before = scene_ids) %>%
  relocate(x_coord, .after = quadrant) %>%
  relocate(y_coord, .after = x_coord) %>%
  relocate(z_coord, .after = y_coord) %>%
  relocate(sampling_date, .after = z_coord) %>%
  relocate(soil_pt, .after = CH6) %>%
  relocate(FM_gt7, .after = soil_pt) %>%
  relocate(FM_sm7, .after = FM_gt7) %>%
  relocate(DM_gt7, .after = FM_sm7) %>%
  relocate(DM_sm7, .after = DM_gt7) %>%
  relocate(WC_gt7, .after = DM_sm7) %>%
  relocate(WC_sm7, .after = WC_gt7) 
