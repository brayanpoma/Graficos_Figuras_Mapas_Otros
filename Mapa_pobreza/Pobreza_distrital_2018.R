#--------------------------------------------------------------------------------
#              MAPA DE POBREZA DISTRITAL MACRORREGIÓN CENTRO - 2018
#--------------------------------------------------------------------------------
# Objetivo: Visualizar mediante un mapala pobreza a nivel distrital de la macrorregión 
#           centro del Perú del 2018.

#configuraciones iniciales

rm(list=ls())
dev.off()
setwd("D:/TESIS/DATOS_2025")
list.files()

#librerias
library("tidyverse")
library("readxl")
library("gridExtra")
library("grid")
library("sf")

# Cargando la base de datos
df<-read_xlsx(path = "Mapa_Pobreza_2018.xlsx",sheet = "Anexo1")

#### Limpieza y tratamiento de datos ####
glimpse(df)
head(df,10)
col_nombres<-df[3,]
col_nombres[6]<-"Inferior"
col_nombres[7]<-"Superior"
col_nombres<-unlist(col_nombres)
names(df)<-col_nombres
df<-df[7:nrow(df),-c(5,8,9)]
dim(df)
df<-df |> 
  filter(!is.na(Ubigeo))

colSums(is.na(df))
df |> 
  filter(is.na(Departamento))

which(is.na(df$Departamento))
df<-df[-1874,]

# Corregimos el tipo de variable
glimpse(df)
df$Inferior<-as.numeric(df$Inferior)
df$Superior<-as.numeric(df$Superior)

# Creamos la variable pobreza

df<-df |> 
  mutate(Pobreza=(Inferior+Superior)/2)

# Filtramos las regiones de interés: Macrorregión centro
# Huancavelica, Huanuco, Junin, Pasco

df_mrc<-df |> 
  filter(Departamento %in%c("HUANCAVELICA","HUÁNUCO","JUNÍN","PASCO"))
table(df_mrc$Departamento)

# Importamos la libreria para graficar distritos

peru_distritos <- st_read("C:/Users/HP/Downloads/Distrital INEI 2023 geogpsperu SuyoPomalia/Distrital INEI 2023 geogpsperu SuyoPomalia.shp")
peru_dptos<-st_read("C:/Users/HP/Downloads/Departamental INEI 2023 geogpsperu SuyoPomalia/Departamental INEI 2023 geogpsperu SuyoPomalia.shp")
peru_dptos<-peru_dptos |> 
  filter(DEPARTAMEN %in% c("HUANCAVELICA","JUNIN","HUANUCO","PASCO"))

table(peru_distritos$DEPARTAMEN)
peru_distritos<-peru_distritos |> 
  filter(DEPARTAMEN %in% c("HUANCAVELICA","JUNIN","HUANUCO","PASCO"))
dim(peru_distritos)
dim(df_mrc)

# Existen dos nuevos distritos que fueron creados despues del 2018
peru_distritos |>
  filter(!UBIGEO %in% df_mrc$Ubigeo)

# Unimos nuestras tablas

df_final<-left_join(peru_distritos,df_mrc,by=c("UBIGEO" = "Ubigeo")) 

peru_deptos |> 
  filter(NAME_1  |>  departamentos_centro)
#### Visualización ####

mapa<-df_final |>
  ggplot()+
  geom_sf(aes(fill=Pobreza),color="black")+
  geom_sf(data = peru_dptos, color="white",fill=NA,linewidth =1)+
  scale_fill_gradient(high = "#17334E",low="#78bbf0")+
  theme_classic()+
  geom_sf_text(data=peru_dptos,
               aes(label =DEPARTAMEN),
               size=2.8,
               fontface = "bold",
               color="white")+
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
  

mapa_1 <- grid.arrange(
  top = arrangeGrob( #inseguridad
    textGrob("MAPA DE POBREZA - MACROREGIÓN CENTRO 2018", gp = gpar(fontsize = 15,fontface = "bold"),
             vjust =1 ),
    textGrob("(Población en situación de Pobreza - Porcentaje)",gp = gpar(fontsize = 10, fontface = "italic"),
             vjust = 2.5),
    ncol = 1 ),
  bottom = textGrob("Fuente: INEI - Mapa de Pobreza 2018", 
                    gp = gpar(fontsize = 10)),
  mapa
)

ggsave(filename = "pobreza_dist_2018.png",plot = mapa_1,width = 3000, height = 2500, 
       units = "px", dpi = 320)


