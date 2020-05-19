# Titulo:   Analisis descriptivo datos del Censo de Bienestar
# Datos:    Datos Censo de Bienestar
# Fecha:    18-05-2020
# Autores:  Gatitos Contra la Desigualdad


# --- Directory and packages
rm(list = ls())
setwd("~/Documents/Encuestas/Censo de Bienestar")
library(pacman)

p_load(survival, data.table, tidyverse, reshape2, gghighlight, RColorBrewer, wesanderson, 
       ggpubr, gridExtra, magrittr,ggrepel)

# ---  Importar los datos
  #Datos originales: https://www.gob.mx/programasintegrales/
  #Datos en formato amigable y no con un manchado pdf que además trae números como imagen:
    #https://drive.google.com/open?id=18roO4GexZf3kjm1BkGnTFhkaPkLQa6dm 
  #Diccionario de datos: https://drive.google.com/open?id=1RL3LbFZIP3dNtuu6ogwsC5kE5EDq0i5A 
datitos <- read.csv("censo_resultados.csv", header = T)


#Tidy Data
datitos %<>% mutate_if(is.character,as.numeric)

datitos <- datitos %>%
  mutate(elec_gob_2020_21=  factor(elec_gob_2020_21,
                            levels = c(0,1),
                            labels = c("No", "Sí")))
         


##Figure 1 
#####################
#Principal AES
ggplot(data =datitos,
       mapping  = aes(x = pobreza , 
                      y = porc_viv,
                      color=elec_gob_2020_21,
                      size=hli,
                      label = ent_corto)
                              ) +
#Geom_point
  geom_point()+
#Line with LR
  geom_smooth(method=lm,
              se=F,
              linetype = "dashed" , 
              color="#fcd949")+
#Text - points
  geom_text_repel(          
                      hjust=0.01, 
                      nudge_x = 0.012,
                      size = 2.5,
                      color="#6f6f6f"
                      #check_overlap = T,
             ) +  
#Colors  
  #scale_color_manual(values=wes_palette(n=5, name="Cavalcanti1")) +
  scale_color_manual(values=c("#3a3a39","#dc5356")) +
#Theme    
  theme_minimal() +
  theme (text = element_text(family = "Verdana",color="#3a3a39"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "#e8e8e6", linetype = "blank"),
         plot.background = element_rect(fill = "#e8e8e6", linetype = "blank"),
         plot.title = element_text(face = "bold", color="#dc5356"),
         plot.subtitle = element_text(face = "bold", color="#3a3a39"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9, color="#6f6f6f", size = 8),
         legend.title = element_text( size = 10),
         #legend.text = element_text( size = 10)
         )+
#Scale Y  
  scale_y_continuous( labels =scales::percent
  #breaks = seq(0,70,10)
     ) +
#Scale X  
  scale_x_continuous(labels =scales::percent
    #breaks = seq(0,90,10)
    ) +
#Labels and titles
  labs ( x = "Porcentaje de población en situación de pobreza",
         y = "Porcentaje de viviendas censadas",
         title = "Viviendas incluidas en Censo de Bienestar",
         subtitle = "Según variables por las que se focalizó",
         caption = "Fuente: Elaborado por @gatitosvsdesig, con datos de Coord. Gral. Prog. Desarr., CONEVAL e INEGI. bit.ly/2LDgV9J",
         color = "¿Elecciones guber. 
en 2020-21?",
         size= "% habla lengua 
indígena")   +
#Lines of 100%
    geom_abline(intercept = 1, slope = 0, linetype = "solid" , color="#3a3a39")

#Save Figure 1
ggsave("Censo y variables explicativas.png", width = 7)









