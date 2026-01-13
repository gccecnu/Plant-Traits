library(dplyr)

data = read.csv("../Data/PlantData/newdat.csv")
data$Elevation = data$Elevation - min(data$Elevation)
data$Species = factor(data$Species, levels = c("CD","CS","IC","SM","ST"),
                                            labels = c("Juncellus serotinus","Carex scabrifolia",
                                                       "Imperate cylindrica","Scirpus mariqueter",
                                                       "Scirpus triqueter"))
BioSumElevation = data %>%
  group_by(Species) %>%
  summarise(Totalbiomass = sum(Biomass, na.rm = T), 
            minElecation = min(Elevation, na.rm = T),
            maxElevation = max(Elevation, na.rm = T)) 
write.csv(BioSumElevation, file = "Table1.csv", row.names = T)

BioSumCreek = data %>%
  group_by(xDistance, Species) %>%
  summarise(sumBiomass = sum(Biomass, na.rm = T), .groups = "drop")
matrix_form <- xtabs(sumBiomass ~ xDistance + Species, data = BioSumCreek)
print(matrix_form)
write.csv(matrix_form, file = "Table2.csv", row.names = T)
