readdata=read.csv("C:/Users/Yogesh/Desktop/terrorism.csv")
readdata=select(readdata,-summary,-alternative,-approxdate,-resolution,-targsubtype1,-targsubtype1_txt,-corp1,-target1,-gsubname,-property,-propcomment,-divert)
readdata=select(readdata,-attacktype2,-attacktype2_txt)
readdata=select(readdata,-c(targtype2,targtype2_txt,corp2,target2,natlty2,natlty2_txt,targtype3,targtype3_txt,targsubtype3,targsubtype3_txt))
readdata=select(readdata,-c(targsubtype2,targsubtype2_txt,corp3,target3,gname2,gsubname2,gname3,gsubname3))
readdata=select(readdata,-c(guncertain1,guncertain2,guncertain3,nperps,nperpcap,claimmode2,claimmode2_txt,claim3,claimmode3))
readdata=select(readdata,-c(weaptype2,weaptype2_txt,weapsubtype2))
readdata=select(readdata,-c(weapsubtype2_txt,weaptype3,weaptype3_txt,weapsubtype3,weapsubtype3_txt,weaptype4,weapsubtype4,weapsubtype4_txt))
readdata=select(readdata,-c(nkillus,nwoundus,propextent,propextent_txt,propvalue,nhostkidus,ransomamtus,ransompaidus,ransomnote,addnotes))
readdata=select(readdata,-c(scite1,scite2,scite3,dbsource,related,claimmode3_txt))

readdata[readdata==""] <- NA

View(readdata)
write.csv(readdata,"C:/Users/Yogesh/Desktop/terror_updated.csv")
library('treemap')
treemap(readdata, 
        index=c("iyear"), 
        vSize = "nkill",  
        palette = "Reds",  
        title="Killings in Global Terrorism", 
        fontsize.title = 14 
)

treemap(readdata, 
        index=c("country_txt"), 
        vSize = "nkill",  
        palette = "Reds",  
        title="Killings in Global Terrorism", 
        fontsize.title = 14 
)    
library(tidyverse)
readdata %>% group_by(iyear,attacktype1_txt) %>% summarise(n = length(iyear)) %>% ungroup() -> dfya
colnames(dfya)<-c("Year","Type of attack","Number of events")
ggplot(data = dfya, aes(x = Year, y = `Number of events`, colour = `Type of attack`)) + 
  geom_line() + geom_point() + theme_bw()


readdata %>% group_by(iyear,targtype1_txt) %>% summarise(n=length(iyear)) %>% ungroup() -> dfya2
colnames(dfya2)<-c("Year","Target type","Number of events")
ggplot(data = dfya2, aes(x = Year, y = `Number of events`, colour = `Target type`)) + 
  geom_line() +geom_point() +theme_bw()


dfa <-sort(table(readdata$gname),decreasing = TRUE)[1:10]

dfa<-as.data.frame(dfa)
dfa<-dfa[-1,]

ggplot(data = dfa, aes(x = reorder(Var1,Freq), y = Freq)) +  
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="", x ="Terrorist group name", y = "Number of events")


readdata %>% filter(suicide ==1) -> data_s
library('leaflet')
leaflet(data = data_s) %>%
  addTiles() %>%
  addMarkers(lat=data_s$latitude, lng=data_s$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", data_s$iday,"/",data_s$imonth,"/", data_s$iyear,
                          "<br><br><strong>Place: </strong>", data_s$city,"-",data_s$country_txt,
                          "<br><strong>Killed: </strong>", data_s$nkill,
                          "<br><strong>Wounded: </strong>", data_s$nwound,
                          "<br><strong>Killed US citizens: </strong>", data_s$nkillus,
                          "<br><strong>Wounded US citizens: </strong>", data_s$nwoundus
             ))  
  
  
  