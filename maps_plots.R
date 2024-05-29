
ggplot()+
  geom_sf(data=plots%>%filter(Name=="HC4"|Name=="HC2a"|Name=="HC1"|Name=="HC5"),
          color="darkgrey",fill="grey")+
  geom_sf(data=plants_2017_parents_merged%>%filter(plot=="HC4"|plot=="HC2a"|plot=="HC1"|plot=="HC5"),
          aes(color=temperature),size=4)+
  geom_text_repel(data=plants_2017_parents_merged%>%filter(plot=="HC4"|plot=="HC2a"|plot=="HC1"|plot=="HC5"),
                            aes(label=label,geometry=geometry),
                            stat="sf_coordinates",min.segment.length=0,
                            max.overlaps=Inf,box.padding=2)+
  geom_text_repel(data=plots%>%filter(Name=="HC4"|Name=="HC2a"|Name=="HC1"|Name=="HC5"),
            aes(label=Name,geometry=geometry),
            stat="sf_coordinates",color="#6cb221",max.overlaps=Inf,
            min.segment.length=10,box.padding=3,size=5)+
  scale_x_continuous(expand=expansion(mult=0.05))+
  scale_y_continuous(expand=expansion(mult=0.05))+
  scale_color_viridis(option="plasma",
                      limits=range(plants_2017_parents_merged$temperature))+
  labs(color="Soil temperature (ºC)")+theme_bw()+
  theme(legend.position="none")+
  annotation_scale()+xlab(NULL)+ylab(NULL)
ggsave(filename="output/figures/maps/maps_plots_1.jpeg",dpi=300)

ggplot()+
  geom_sf(data=plots%>%filter(Name=="H01"|Name=="H08"|Name=="H09"),
          color="darkgrey",fill="grey")+
  geom_sf(data=plants_2017_parents_merged%>%filter(plot=="H01"|plot=="H08"|plot=="H09"),
          aes(color=temperature),size=4)+
  geom_text_repel(data=plants_2017_parents_merged%>%
                    filter(plot=="H01"|plot=="H08"|plot=="H09"),
                  aes(label=label,geometry=geometry),
                  stat="sf_coordinates",min.segment.length=0,
                  max.overlaps=Inf,box.padding=1.5)+
  geom_text_repel(data=plots%>%
                    filter(Name=="H01"|Name=="H08"|Name=="H09"),
                  aes(label=Name,geometry=geometry),
                  stat="sf_coordinates",color="#6cb221",max.overlaps=Inf,
                  min.segment.length=10,box.padding=9,size=5)+
  scale_x_continuous(expand=expansion(mult=0.2))+
  scale_y_continuous(expand=expansion(mult=0.2))+
  scale_color_viridis(option="plasma",
                      limits=range(plants_2017_parents_merged$temperature))+
  labs(color="Soil temperature (ºC)")+theme_bw()+
  theme(legend.position="none")+
  annotation_scale()+xlab(NULL)+ylab(NULL)
ggsave(filename="output/figures/maps/maps_plots_2.jpeg",dpi=300)

ggplot()+
  geom_sf(data=plots%>%filter(Name=="H02"|Name=="H10"|Name=="H03"),
          color="darkgrey",fill="grey")+
  geom_sf(data=plants_2017_parents_merged%>%filter(plot=="H02"|plot=="H10"|plot=="H03"),
          aes(color=temperature),size=4)+
  geom_text_repel(data=plants_2017_parents_merged%>%
                    filter(plot=="H02"|plot=="H10"|plot=="H03"),
                  aes(label=label,geometry=geometry),
                  stat="sf_coordinates",min.segment.length=0,
                  max.overlaps=Inf,box.padding=3)+
  geom_text_repel(data=plots%>%
                    filter(Name=="H02"|Name=="H10"|Name=="H03"),
                  aes(label=Name,geometry=geometry),
                  stat="sf_coordinates",color="#6cb221",max.overlaps=Inf,
                  min.segment.length=10,box.padding=4,size=5)+
  scale_x_continuous(expand=expansion(mult=0.1))+
  scale_y_continuous(expand=expansion(mult=0.1))+
  scale_color_viridis(option="plasma",
                      limits=range(plants_2017_parents_merged$temperature))+
  labs(color="Soil temperature (ºC)")+theme_bw()+
  theme(legend.position="none")+
  annotation_scale()+xlab(NULL)+ylab(NULL)
ggsave(filename="output/figures/maps/maps_plots_3.jpeg",dpi=300)

ggplot()+
  geom_sf(data=plots%>%filter(Name=="H13"),
          color="darkgrey",fill="grey")+
  geom_sf(data=plants_2017_parents_merged%>%filter(plot=="H13"),
          aes(color=temperature),size=4)+
  geom_text_repel(data=plants_2017_parents_merged%>%
                    filter(plot=="H13"),
                  aes(label=label,geometry=geometry),
                  stat="sf_coordinates",min.segment.length=0,
                  max.overlaps=Inf,box.padding=1)+
  geom_text_repel(data=plots%>%
                    filter(Name=="H13"),
                  aes(label=Name,geometry=geometry),
                  stat="sf_coordinates",color="#6cb221",max.overlaps=Inf,
                  min.segment.length=10,box.padding=15,size=5)+
  scale_x_continuous(expand=expansion(mult=0.1))+
  scale_y_continuous(expand=expansion(mult=0.1))+
  scale_color_viridis(option="plasma",
                      limits=range(plants_2017_parents_merged$temperature))+
  labs(color="Soil temperature (ºC)")+theme_bw()+
  theme(legend.position="none")+
  annotation_scale()+xlab(NULL)+ylab(NULL)
ggsave(filename="output/figures/maps/maps_plots_4.jpeg",dpi=300)

ggplot()+
  geom_sf(data=plots%>%filter(Name=="H04"|Name=="H05"),
          color="darkgrey",fill="grey")+
  geom_sf(data=plants_2017_parents_merged%>%filter(plot=="H04"|plot=="H05"),
          aes(color=temperature),size=4)+
  geom_text_repel(data=plants_2017_parents_merged%>%
                    filter(plot=="H04"|plot=="H05"),
                  aes(label=label,geometry=geometry),
                  stat="sf_coordinates",min.segment.length=0,
                  max.overlaps=Inf,box.padding=1.15)+
  geom_text_repel(data=plots%>%
                    filter(Name=="H04"|Name=="H05"),
                  aes(label=Name,geometry=geometry),
                  stat="sf_coordinates",color="#6cb221",max.overlaps=Inf,
                  min.segment.length=10,box.padding=15,size=5)+
  scale_x_continuous(expand=expansion(mult=0.1))+
  scale_y_continuous(expand=expansion(mult=0.1))+
  scale_color_viridis(option="plasma",
                      limits=range(plants_2017_parents_merged$temperature))+
  labs(color="Soil temperature (ºC)")+theme_bw()+
  theme(legend.position="none")+
  annotation_scale()+xlab(NULL)+ylab(NULL)
ggsave(filename="output/figures/maps/maps_plots_5.jpeg",dpi=300)

maps_legend<-get_legend(
  ggplot()+
    geom_sf(data=plots%>%filter(Name=="H04"|Name=="H05"),
            color="darkgrey",fill="grey")+
    geom_sf(data=plants_2017_parents_merged%>%filter(plot=="H04"|plot=="H05"),
            aes(color=temperature),size=4)+
    geom_text_repel(data=plants_2017_parents_merged%>%
                      filter(plot=="H04"|plot=="H05"),
                    aes(label=label,geometry=geometry),
                    stat="sf_coordinates",min.segment.length=0,
                    max.overlaps=Inf,box.padding=1.25)+
    geom_text_repel(data=plots%>%
                      filter(Name=="H04"|Name=="H05"),
                    aes(label=Name,geometry=geometry),
                    stat="sf_coordinates",color="#6cb221",max.overlaps=Inf,
                    min.segment.length=10,box.padding=15,size=5)+
    scale_x_continuous(expand=expansion(mult=0.2))+
    scale_y_continuous(expand=expansion(mult=0.2))+
    scale_color_viridis(option="plasma",
                        limits=range(plants_2017_parents_merged$temperature))+
    labs(color="Soil temperature (ºC)")+theme_bw()+
    theme(legend.position="none")+
    annotation_scale()+xlab(NULL)+ylab(NULL)


  st_write(plants_2017_parents_merged, "data/shapefiles/plants_2017_parents_merged.shp")
