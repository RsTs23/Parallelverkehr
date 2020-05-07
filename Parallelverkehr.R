# Parallelverkehrlistenexport
# Packages installieren
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
library("dplyr") 
library("ggplot2")
library("tidyr")

# Einlesen der Rohdaten
#Garbage collection und workspace clearen
rm(list = ls())
gc()

#Anpassen (Pfad der ersten Woche)!!!!!
setwd("C:/Users/b0012/Desktop/Parallelverkehr/0")	


stop_times = read.delim("stop_times.txt", sep = ",")
stops = read.delim("stops.txt", sep = ",")
trips = read.delim("trips.txt", sep = ",")
routes = read.delim("routes.txt", sep = ",", stringsAsFactors = FALSE)
agency = read.delim("agency.txt", sep = ",")

# Droppen der unnötigen Variablen
stops = stops[1:2]
stop_times = stop_times[c(1,4)]
trips = trips[c(1,3)]
routes = routes[1:3]

routes = filter(routes, !grepl("-Y", route_short_name))
routes = filter(routes, !grepl("^M", route_short_name))
routes$route_short_name[is.na(routes$route_short_name)] = "Na"
routestemp = filter(routes, grepl("^N",  route_short_name))
routes = filter(routes, !grepl("^N",  route_short_name))
routestemp = filter(routestemp, !grepl("[0-9]$",  route_short_name))
routes = rbind(routes, routestemp)
rm(routestemp)

agency = agency[1]

# Verbinden der Agency und Route Daten
AgRou  =merge(x=routes,y=agency, by="agency_id", all.x=TRUE)
rm(agency)
rm(routes)

# Verbinden der trips und AgRou
TrAgRou= merge(x=trips,y=AgRou, by="ï..route_id")
rm(trips)
rm(AgRou)

# Verbinden der stop_times und TrAgRou
StoTrAgRou = merge(x=stop_times,y=TrAgRou, by.x ="ï..trip_id", by.y = "trip_id")
rm(stop_times)
rm(TrAgRou)

# Verbinden der stops mit Rest
Master = merge(x=StoTrAgRou,y=stops, by.x ="stop_id", by.y = "ï..stop_id")
rm(stops)
rm(StoTrAgRou)

# Aggregieren der Daten
Master = filter(Master, !is.na(Master$route_short_name))
Master2 = Master[c(4,6)]
Master2 = distinct(Master2)
x = Master2$agency_id
Master2$agency_id = Master2$stop_name
Master2$stop_name = x
names(Master2) = c("stop_name", "agency_id")
rm(x)
Master2 = Master2[order(Master2$stop_name),]
Master2 = Master2 %>% group_by(stop_name) %>% filter(n()>1)

Master2$stop_name = as.character(Master2$stop_name)
Master$stop_name = as.character(Master$stop_name)
Master2$agency_id = as.character(Master2$agency_id)
Master$agency_id = as.character(Master$agency_id)
Master$route_short_name = as.character(Master$route_short_name)

MiniMaster = filter(Master, Master$stop_name  %in% Master2$stop_name)
MiniMaster = filter(MiniMaster, MiniMaster$agency_id  %in% Master2$agency_id)

MiniMaster = MiniMaster[4:6]
gc()

i = 1
while(i<length(Master2$stop_name)+1){
temp = filter(MiniMaster, MiniMaster$stop_name == Master2$stop_name[i] & MiniMaster$agency_id == Master2$agency_id[i] )
Master2$Linien[i] = paste(unique(temp$route_short_name), collapse = "  ")
print(i)
i = i + 1
}
rm(i)
rm(temp)

Parallelverkehr = Master2

rm(Master)
rm(Master2)
rm(MiniMaster)

j = 1

#Anpassen (Anzahl Wochen plus 2)!!!!
while(j < 2){	
  #Anpassen (Pfad der Wochenordner)
  path = paste("C:/Users/b0012/Desktop/Parallelverkehr/",j, sep = "")								
  setwd(path)
  rm(path)
  stop_times = read.delim("stop_times.txt", sep = ",")
  stops = read.delim("stops.txt", sep = ",")
  trips = read.delim("trips.txt", sep = ",")
  routes = read.delim("routes.txt", sep = ",", stringsAsFactors = FALSE)
  agency = read.delim("agency.txt", sep = ",")
  
  # Droppen der unnötigen Variablen
  stops = stops[1:2]
  
  stops = filter(stops, !(stops$stop_name  %in% Parallelverkehr$stop_name))
  
  stop_times = stop_times[c(1,4)]
  trips = trips[c(1,3)]
  routes = routes[1:3]
  
  routes = filter(routes, !grepl("-Y", route_short_name))
  routes = filter(routes, !grepl("^M", route_short_name))
  routes$route_short_name[is.na(routes$route_short_name)] = "Na"
  routestemp = filter(routes, grepl("^N",  route_short_name))
  routes = filter(routes, !grepl("^N",  route_short_name))
  routestemp = filter(routestemp, !grepl("[0-9]$",  route_short_name))
  routes = rbind(routes, routestemp)
  rm(routestemp)
  
  agency = agency[1]
  
  # Verbinden der Agency und Route Daten
  AgRou  =merge(x=routes,y=agency, by="agency_id", all.x=TRUE)
  rm(agency)
  rm(routes)
  # Verbinden der trips und AgRou
  TrAgRou= merge(x=trips,y=AgRou, by="ï..route_id", all.x=TRUE)
  rm(trips)
  rm(AgRou)
  # Verbinden der stop_times und TrAgRou
  StoTrAgRou = merge(x=stop_times,y=TrAgRou, by.x ="ï..trip_id", by.y = "trip_id", all.x=TRUE)
  rm(stop_times)
  rm(TrAgRou)
  # Verbinden der stops mit Rest
  Master = merge(x=StoTrAgRou,y=stops, by.x ="stop_id", by.y = "ï..stop_id", all.x=TRUE)
  rm(stops)
  rm(StoTrAgRou)
  
  # Aggregieren der Daten
  Master = filter(Master, !(is.na(route_short_name) ))
  Master2 = Master[c(4,6)]
  Master2 = distinct(Master2)
  x = Master2$agency_id
  Master2$agency_id = Master2$stop_name
  Master2$stop_name = x
  names(Master2) = c("stop_name", "agency_id")
  rm(x)
  Master2 = Master2[order(Master2$stop_name),]
  Master2 = Master2 %>% group_by(stop_name) %>% filter(n()>1)
  
  Master2$stop_name = as.character(Master2$stop_name)
  Master$stop_name = as.character(Master$stop_name)
  Master2$agency_id = as.character(Master2$agency_id)
  Master$agency_id = as.character(Master$agency_id)
  Master$route_short_name = as.character(Master$route_short_name)
  
  MiniMaster = filter(Master, Master$stop_name  %in% Master2$stop_name)
  MiniMaster = filter(MiniMaster, MiniMaster$agency_id  %in% Master2$agency_id)
  
  MiniMaster = MiniMaster[4:6]
  gc()
  
  i = 1
  while(i<length(Master2$stop_name)+1){
    temp = filter(MiniMaster, MiniMaster$stop_name == Master2$stop_name[i] & MiniMaster$agency_id == Master2$agency_id[i] )
    Master2$Linien[i] = paste(unique(temp$route_short_name), collapse = "  ")
    print(i)
    i = i + 1
  }
  rm(i)
  rm(temp)
  
  Parallelverkehr = rbind(Parallelverkehr, Master2)
  rm(MiniMaster)
  rm(Master2)
  rm(Master)
  gc()
  print(j)
  j = j+1
}

# Parallelhaltestellen exportieren
Para = Parallelverkehr
Parallelverkehr = Parallelverkehr %>% group_by(stop_name) %>% summarize(agency_id=paste(agency_id, collapse="//"), Linien=paste(Linien, collapse="//"))
Parallelverkehr = separate(Parallelverkehr, agency_id, sep = "//", into= c("TU1", "TU2", "TU3", "TU4", "TU5", "TU6", "TU7"), remove = TRUE)
Parallelverkehr = separate(Parallelverkehr, Linien, sep = "//", into= c("TU1Linien", "TU2Linien", "TU3Linien", "TU4Linien", "TU5Linien", "TU6Linien", "TU7Linien"), remove = TRUE)

Parallelverkehr$stop_name = iconv(Parallelverkehr$stop_name,from="UTF-8" ,to="iso_8859_15")
Parallelverkehr$TU1 = iconv(Parallelverkehr$TU1,from="UTF-8" ,to="iso_8859_15")
Parallelverkehr$TU2 = iconv(Parallelverkehr$TU2,from="UTF-8" ,to="iso_8859_15")
Parallelverkehr$TU3 = iconv(Parallelverkehr$TU3,from="UTF-8" ,to="iso_8859_15")
Parallelverkehr$TU4 = iconv(Parallelverkehr$TU4,from="UTF-8" ,to="iso_8859_15")
Parallelverkehr$TU5 = iconv(Parallelverkehr$TU5,from="UTF-8" ,to="iso_8859_15")
Parallelverkehr$TU6 = iconv(Parallelverkehr$TU6,from="UTF-8" ,to="iso_8859_15")
Parallelverkehr$TU7 = iconv(Parallelverkehr$TU7,from="UTF-8" ,to="iso_8859_15")

write.table(Parallelverkehr, 
            file = "Parallelhaltestellen.csv",										
            sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)
