
#install.packages("Rspotify")
library(Rspotify)
library(httpuv)
library(stringi)
library(stringr)
library(tidyverse)


keys <- spotifyOAuth(app_id="***",client_id="***", client_secret="***")

bizarro_playlists <- getPlaylists("bizarrorecords",token=keys) 

cantoras_uru <- getPlaylistSongs("bizarrorecords","08ArmULKCwKKaNUbPPyk09",token=keys)

cantoras_uru_datos <- data.frame()

for (i in cantoras_uru$id) {
  song<-getFeatures(i,token=keys)
  cantoras_uru_datos <- rbind(cantoras_uru_datos, song)
}

cantoras_uru <- cantoras_uru %>% left_join(cantoras_uru_datos, by="id")

library(ggridges)
library(extrafont)
library(RColorBrewer)
library(ggrepel)
vtable::vtable(cantoras_uru)

#Movimiento

movimiento <- cantoras_uru %>% select(tracks, id, artist, danceability, energy, valence) %>%
  pivot_longer(cols=c(danceability, energy, valence), names_to = "variable", values_to = "value")

movimiento_es <- movimiento %>% mutate(variable_es=ifelse(variable=="danceability", 
                                                          "Bailabilidad",
                                                          ifelse(variable=="energy",
                                                                 "Energía", "Positivismo")))

gmov <- ggplot(movimiento_es, aes(x=value, y=variable_es, fill=variable_es)) +
  geom_density_ridges(alpha = 0.4) + 
  labs(title = "Cantoras uruguayas: ¿canciones movidas?",
       x = "Puntaje",
       y = "",
       caption = "Visualización: @danidlsa | Playlist: Bizarro Records | Datos: Spotify",
       subtitle="Positivismo, Energía y Bailabilidad de canciones seleccionadas") +
  ggdark::dark_theme_grey() +
  scale_fill_brewer(palette = "RdPu") +
  theme(text=element_text(family="Segoe UI", color="white"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=12, hjust=.5),
        plot.caption=element_text(size=10),
        axis.text=element_text(size=12),
        legend.position="") 
gmov

ggsave("rspotify_movimiento.png", height=12, width=16, units="cm")


gmov_en <- ggplot(movimiento, aes(x=value, y=variable, fill=variable)) +
  geom_density_ridges(alpha = 0.4) + 
  labs(title = "Uruguayan Women Singers: Upbeat Songs?",
       x = "Score",
       y = "",
       caption = "Visualization: @danidlsa | Playlist: Bizarro Records | Data: Spotify",
       subtitle="Valence, Energy and Danceability in Selected Songs") +
  ggdark::dark_theme_grey() +
  scale_fill_brewer(palette = "RdPu") +
  theme(text=element_text(family="Segoe UI", color="white"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=12, hjust=.5),
        plot.caption=element_text(size=10),
        axis.text=element_text(size=12),
        legend.position="") 
gmov_en

ggsave("rspotify_movimiento_en.png", height=12, width=16, units="cm")

#Sonido: volumen, tempo, acustico

acustico_volumen <- cantoras_uru %>% select(tracks, id, artist, acousticness, loudness) %>%
  mutate(index=acousticness*loudness) %>%
  mutate(tag=paste(tracks, artist, sep=", ")) %>%
  mutate(color=ifelse(min(loudness)==loudness | max(loudness)==loudness |
                        min(acousticness)==acousticness | max(acousticness)==acousticness, 
                      "c1", "c2"))

ggplot(acustico_volumen, aes(x=loudness, y=acousticness, color=color)) + 
  geom_point(alpha=.7) +
  scale_color_manual(values=c("c1"="#FDE0DD", "c2"="#F768A1")) +
  ggdark::dark_theme_grey() +
  theme(text=element_text(family="Segoe UI", color="white"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=12, hjust=.5),
        plot.caption=element_text(size=10),
        axis.text=element_text(size=12),
        legend.position="") +
  labs(x="Volumen (dB)",
       y="Probabilidad de canción acústica",
       title="El sonido de las Cantoras Uruguayas",
       subtitle="Volumen y probabilidad de sonido acústico en canciones seleccionadas",
       caption="Visualización: @danidlsa | Playlist: Bizarro Records | Datos: Spotify"
       ) +
  geom_text_repel(data = subset(acustico_volumen, color=="c1"), 
                   aes(label = tag),
                   size=3)

ggsave("rspotify 2020_sonido.png", height=14, width = 16, units="cm")


ggplot(acustico_volumen, aes(x=loudness, y=acousticness, color=color)) + 
  geom_point(alpha=.7) +
  scale_color_manual(values=c("c1"="#FDE0DD", "c2"="#F768A1")) +
  ggdark::dark_theme_grey() +
  theme(text=element_text(family="Segoe UI", color="white"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=12, hjust=.5),
        plot.caption=element_text(size=10),
        axis.text=element_text(size=12),
        legend.position="") +
  labs(x="Loudness (dB)",
       y="Acousticness",
       title="The Sound of Uruguayan Women Singers",
       subtitle="Loudness and Acousticness of Selected Songs",
       caption="Visualization: @danidlsa | Playlist: Bizarro Records | Data: Spotify"
  ) +
  geom_text_repel(data = subset(acustico_volumen, color=="c1"), 
                  aes(label = tag),
                  size=3)

ggsave("rspotify 2020_sonido_en.png", height=14, width = 16, units="cm")

##Clave y modo

clavmod <- cantoras_uru %>% select(tracks, id, artist, key, mode) %>%
  mutate(n=1)%>%
  mutate(modo=ifelse(mode==0, "Menor", "Mayor")) %>%
  mutate(clave=
           case_when(mode==1 & key==1 ~ "C",
                     mode==1 & key==2 ~ "G",
                     mode==1 & key==3 ~ "D",
                     mode==1 & key==4 ~ "A",
                     mode==1 & key==5 ~ "E",
                     mode==1 & key==6 ~ "B",
                     mode==1 & key==7 ~ "F#",
                     mode==1 & key==8 ~ "D♭",
                     mode==1 & key==9 ~ "A♭",
                     mode==1 & key==10 ~ "E♭",
                     mode==1 & key==11 ~ "B♭",
                     mode==1 & key==12 ~ "F",
                     mode==0 & key==1 ~ "a",
                     mode==0 & key==2 ~ "e",
                     mode==0 & key==3 ~ "b",
                     mode==0 & key==4 ~ "f#",
                     mode==0 & key==5 ~ "c#",
                     mode==0 & key==6 ~ "g#",
                     mode==0 & key==7 ~ "d#",
                     mode==0 & key==8 ~ "b♭",
                     mode==0 & key==9 ~ "f",
                     mode==0 & key==10 ~ "c",
                     mode==0 & key==11 ~ "g",
                     mode==0 & key==12 ~ "d"
                     ))

resum_clavmod <- aggregate(n~clave+modo, clavmod, sum)

ggplot(resum_clavmod, aes(x=reorder(clave, n), y=n, fill=modo)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_grid(modo~., scales="free", space="free", switch="y") +
  scale_fill_manual(values=c("#FDE0DD", "#F768A1")) +
  ggdark::dark_theme_grey() +
  theme(text=element_text(family="Segoe UI", color="white"),
        plot.title=element_text(size=16, face="bold"),
        plot.subtitle=element_text(size=12, hjust=.5),
        plot.caption=element_text(size=10),
        axis.text=element_text(size=12),
        legend.position="",
        strip.placement="outside",
        strip.text=element_text(size=12, face="bold")) +
  labs(y="Cantidad de canciones",
       x="Clave y Modo",
       title="Cantoras Uruguayas: Canciones según Clave y Modo",
       caption="Visualización: @danidlsa | Playlist: Bizarro Records | Datos: Spotify") +
  scale_y_continuous(breaks=c(0,2,4,6,8,10))

ggsave("rspotify 2020_modo y clave.png", height=16, width=17, units="cm")  

resum_clavmod %>% mutate(mode_2=ifelse(modo=="Mayor", "Major", "Minor")) %>%
ggplot(aes(x=reorder(clave, n), y=n, fill=mode_2)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_grid(mode_2~., scales="free", space="free", switch="y") +
  scale_fill_manual(values=c("#FDE0DD", "#F768A1")) +
  ggdark::dark_theme_grey() +
  theme(text=element_text(family="Segoe UI", color="white"),
        plot.title=element_text(size=16, face="bold"),
        plot.subtitle=element_text(size=12, hjust=.5),
        plot.caption=element_text(size=10),
        axis.text=element_text(size=12),
        legend.position="",
        strip.placement="outside",
        strip.text=element_text(size=12, face="bold")) +
  labs(y="Number of songs",
       x="Key and Mode",
       title="Uruguayan Women Singers: Songs by Key and Mode",
       caption="Visualization: @danidlsa | Playlist: Bizarro Records | Data: Spotify") +
  scale_y_continuous(breaks=c(0,2,4,6,8,10))

ggsave("rspotify 2020_modo y clave_en.png", height=16, width=17, units="cm")  

