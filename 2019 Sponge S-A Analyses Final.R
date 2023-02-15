#### load in packages ####

require("ggpubr")
require("vegan") 
require("plyr") 
require("dplyr") 
require("ggplot2") 
require("lme4") 
require("lmerTest") 
require("car") 
require("ggOceanMaps") 
require("pals") 

#### load data into data.frames ####

sponge <- read.csv("Sponge_2019.csv", header = T, fileEncoding= "latin1") #add fileEncoding function to get R-studio to read CSV properly. 
spongedf <- data.frame(read.csv("Sponge_2019.csv", header = TRUE, fileEncoding = "latin1"))
spongebytaxa <- data.frame(read.csv("Sponge_by_Taxa_Vegan_2019.csv", header = TRUE)) #when I first try to read in this file (and preivous file) it throws an error saying "no lines available in input". Just open the file in excel and it will then work. 
spongebytaxa[is.na(spongebytaxa)] <- 0
sponge$Richness <- specnumber(spongebytaxa[,2:51]) # use specnumber function to calculate species richness. 
sponge$Richness
sponge$PWL = sponge$Consumed.Weight.g/sponge$Initial.Weight.g * 100 # calculate and add a column to DF for % weight loss (PWL)
spongeM = sponge %>% filter(Locality == "Marine") #make data frame with marine sponges only
spongeT = sponge %>% filter(Locality == "Terrestrial") #make data frame with terrestrial sponges only
spongebytaxaT = spongebytaxa %>% filter(!grepl("M", X)) #make matrix for Terrestrial sponges only.
spongebytaxaM = spongebytaxa %>% filter(!grepl("T", X)) #make matrix for marine sponges only. 
spongebytaxaM.noLB = spongebytaxaM %>% filter(!grepl("LB", X)) # make new data frame excluding Longboard sponges (to test for SIE)
spongebytaxaT.noLB = spongebytaxaT %>% filter(!grepl("LB", X)) # make new data frame excluding Longboard sponges
spongeM.noLB = spongeM %>% filter(!grepl("LB", ID))  # make new data frame excluding Longboard sponges
spongeT.noLB = spongeT %>% filter(!grepl("LB", ID))  # make new data frame excluding Longboard sponges

#### log10 transformations ####

spongelog10 = sponge %>% filter(Abundance > 0) # filter out one sample (T-17-C) with 0 abundance
spongelog10$log10Abundance <- log10(spongelog10$Abundance)
spongelog10$log10Richness <- log10(spongelog10$Richness)
spongeM$log10Abundance <- log10(spongeM$Abundance)
spongeT$log10Abundance <- log10(spongeT$Abundance)
spongeM$log10Richness <- log10(spongeM$Richness)
spongeT$log10Richness <- log10(spongeT$Richness)
spongeTlog10 = spongeT %>% filter(Abundance >0)
spongeTlog10.noLB = spongeTlog10 %>% filter(!grepl("LB", ID))

#### Set Factors ####

sponge$Shape <- as.factor(sponge$Shape)
sponge$Shape <- factor(sponge$Shape, levels = c("Cube", "Rectangle", "Board", "Longboard")) #order factor levels. But won't recognize the shape "Rectangle" for some reason. Converts it to NA. 
sponge[is.na(sponge)] <- "Rectangle" #correct error created by line above. 
sponge$Locality <- as.factor(sponge$Locality)
spongeM$Locality <- as.factor(spongeM$Locality)
spongeT$Locality <- as.factor(spongeT$Locality)
spongeM$Shape <- as.factor(spongeM$Shape)
spongeT$Shape <- as.factor(spongeT$Shape)
spongeTlog10$Shape <- as.factor(spongeTlog10$Shape)

########## Deployment Station Map ##########

require("ggOceanMapsData") 
require("ggspatial") 

Sites <- data.frame(lon = c(-90.541518, -90.665152), lat = c(29.246417, 29.256232), Locality = c("Terrebonne Bay", "Coastal Salt Marsh")) # create dataframe with coordinates for station sites. 

basemap(limits = c(-91, -90, 28.75, 29.75), bathymetry = T, land.col = "antiquewhite", grid.col = NA, legends = FALSE)+ #create base map using coordinates. 
  geom_spatial_point(data = Sites, aes(x = lon, y = lat, color = Locality))+ #add station points
  annotation_scale(location = "br") + #add scale bar at bottom right
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering) + # add north arrow at bottom left
  ggtitle("Deployment Stations Map") + #add map title
  xlab("Longitude") + 
  ylab("Latitude") +
  annotate(geom = "text", x = -90.5, y = 28.83, label = "Gulf of Mexico", fontface = "italic", color = "grey22", size = 4) + # add GoM label 
  annotate(geom = "text", x = -90.48, y = 29.17, label = "Terrebonne Bay", fontface = "italic", color = "grey22", size = 2) +  # add TB label
  scale_y_continuous(name = "Latitude", breaks = c(28, 28.5, 29, 29.5, 30)) + # make y-axis ticks more fine
  scale_x_continuous(name = "Longitude", breaks = c(-91.5, -91, -90.5, -90))  + # make x-axis ticks more fine
  scale_color_manual("Locality", values = c("orange4", "cornflowerblue"))
  
########## Marine Plots ##########

#### log10 Abundance predicted by Surface Area ####

spongeM$Shape <- factor(spongeM$Shape, levels = c("Cube", "Rectangle", "Board", "Longboard")) #order factor levels. But won't recognize the shape "Rectangle" for some reason. Converts it to NA. 
spongeM[is.na(spongeM)] <- "Rectangle" #Convert NA from previous step back to Rectangle. 

AbundanceM <- ggplot(data=spongeM, aes(y=log10Abundance, x=Surface.Area.cm2))+
  geom_jitter(cex=4, alpha=1, aes(color=Shape))+
  labs(y="log10 Abundance", x="Surface Area (cm2)")+
  #ggtitle("log10Abundance ~ Surface Area Terrebonne Bay")+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  scale_color_manual("Surface Area", labels = c("Small", "Medium", "Large", "Extra-large"), values=c( "cyan3", "darkgoldenrod3", "darkolivegreen3", "brown3"))

AbundanceM 

#### Richness predicted by Surface Area ####

RichnessM <- ggplot(data=spongeM, aes(y=Richness, x=Surface.Area.cm2))+
  geom_jitter(cex=4, alpha=1, aes(color=Shape))+
  labs(y="Species Richness", x="Surface Area (cm2)")+
  #ggtitle("Richness ~ Surface Area Terrebonne Bay")+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  geom_smooth(method="lm", aes(fullrange=TRUE), color = "gray50")+
  scale_color_manual("Surface Area", labels = c("Small", "Medium", "Large", "Extra-large"), values=c( "cyan3", "darkgoldenrod3", "darkolivegreen3", "brown3"))

RichnessM

## Simpsons diversity ##
head(spongebytaxaM)
row.names = "Sample.ID"
spongebytaxaM = subset(spongebytaxaM, select = -X) #remove column X (first column for sample ID b/c non-numeric)
head(spongebytaxaM)
spongeM$Simpson <- diversity(spongebytaxaM[,1:50], index = "simpson")

#### Simpsons Evenness predicted by Surface Area ####

SimpsonM <- ggplot(data=spongeM, aes(y=Simpson, x=Surface.Area.cm2))+
  geom_jitter(cex=4, alpha=1, aes(color=Shape))+
  labs(y="Simpson's Evenness", x="Surface Area (cm2)")+
  #ggtitle("Simpson's Evenness ~ Surface Area Terrebonne Bay")+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  scale_color_manual("Surface Area", labels = c("Small", "Medium", "Large", "Extra-large"), values=c( "cyan3", "darkgoldenrod3", "darkolivegreen3", "brown3"))

SimpsonM

########## Terrestrial Plots ##########

#### log10 Abundance predicted by Surface Area ####

spongeTlog10$Shape <- factor(spongeTlog10$Shape, levels = c("Cube", "Rectangle", "Board", "Longboard")) #order factor levels. But won't recognize the shape "Rectangle" for some reason. Converts it to NA. 
spongeTlog10[is.na(spongeTlog10)] <- "Rectangle" #Convert NA from previous step back to Rectangle. 

AbundanceT <- ggplot(data=spongeTlog10, aes(y=log10Abundance, x=Surface.Area.cm2))+
  geom_jitter(cex=4, alpha=1, aes(color=Shape))+
  labs(y="log10 Abundance", x="Surface Area (cm2)")+
  #ggtitle("log10Abundance ~ Surface Area Coastal Salt Marsh")+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  geom_smooth(method="lm", aes(fullrange=TRUE), color = "gray50")+
  scale_color_manual("Surface Area", labels = c("Small", "Medium", "Large", "Extra-large"), values=c( "cyan3", "darkgoldenrod3", "darkolivegreen3", "brown3"))

AbundanceT

#### Richness predicted by Surface Area ####

spongeT$Shape <- factor(spongeT$Shape, levels = c("Cube", "Rectangle", "Board", "Longboard")) #order factor levels. But won't recognize the shape "Rectangle" for some reason. Converts it to NA. 
spongeT[is.na(spongeT)] <- "Rectangle" #Convert NA from previous step back to Rectangle. 

RichnessT <- ggplot(data=spongeT, aes(y=Richness, x=Surface.Area.cm2))+
  geom_jitter(cex=4, alpha=1, aes(color=Shape))+
  labs(y="Species Richness", x="Surface Area (cm2)")+
  #ggtitle("Richness ~ Surface Area Coastal Salt Marsh")+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  geom_smooth(method="lm", aes(fullrange=TRUE), color = "gray50")+
  scale_color_manual("Surface Area", labels = c("Small", "Medium", "Large", "Extra-large"), values=c( "cyan3", "darkgoldenrod3", "darkolivegreen3", "brown3"))

RichnessT

## Simpsons diversity ##

head(spongebytaxaT)
row.names = "Sample.ID"
spongebytaxaT = subset(spongebytaxaT, select = -X) #remove column X (first column for sample ID b/c non-numeric)
head(spongebytaxaT)
spongeT$Simpson <- diversity(spongebytaxaT[,1:50], index = "simpson")

#### Simpsons Evenness predicted by Surface Area ####

SimpsonT <- ggplot(data=spongeT, aes(y=Simpson, x=Surface.Area.cm2))+
  geom_jitter(cex=4, alpha=1, aes(color=Shape))+
  labs(y="Simpson's Evenness", x="Surface Area (cm2)")+
  #ggtitle("Simpson (J) ~ Surface Area Coastal Salt Marsh")+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  scale_color_manual("Surface Area", labels = c("Small", "Medium", "Large", "Extra-large"), values=c( "cyan3", "darkgoldenrod3", "darkolivegreen3", "brown3"))

SimpsonT

##### Combine Terrestrial and Marine plots into 1 figure ##### 

CombinedPanel <- ggarrange(AbundanceM, RichnessM, SimpsonM, AbundanceT, RichnessT, SimpsonT, ncol = 3, nrow = 2, common.legend = T, legend = "bottom", labels = c("a", "b", "c", "d", "e", "f"), vjust =1)
annotate_figure(CombinedPanel, left = text_grob("          Coastal Salt Marsh                 Terrebonne Bay", size = 12, face = "bold", rot = 90))
CombinedPanel #generates 2 figures, one with titles and one without. 

########## Marine Analyses ##########

#### Abundance predicted by Surface Area ####  

Marinemodel1 <- lmer(log10Abundance ~ Surface.Area.cm2 + (1| Tag), data = spongeM)
Anova(Marinemodel1, type = "II") # use type II SOS because do not have an interaction effect.

#### Richness predicted by Surface Area ####

Marinemodel2 <- lmer(Richness ~ Surface.Area.cm2 + (1| Tag), data = spongeM)
Anova(Marinemodel2, type = "II")  # Use type II SOS because there is no interaction effect. 
getME(Marinemodel2, "theta") # this calls the random effect parameter estimate (random intercept). 
getME(Marinemodel2, "sigma") # this calls the residual standard error of the fitted model. 
getME(Marinemodel2, "fixef") # calls intercept and slope of fixed effect. 
# Regression equation for marine richness: Ysi = 5.33 + 1.0837 + (0.0076*Xi) + 1.45

#### Richness predicted by Surface Area excluding Longboard sponges ####

Marinemodel2A <- lmer(Richness ~ Surface.Area.cm2 + (1| Tag), data = spongeM.noLB)
Anova(Marinemodel2A, type = "II")  # Use type II SOS because there is no interaction effect.

#### Simpsons Evenness predicted by Surface Area ####

Marinemodel3 <- lmer(Simpson ~ Surface.Area.cm2 + (1 | Tag), data = spongeM) 
Anova(Marinemodel3, type = "II")

########## Terrestrial Analyses ##########

#### log10 Abundance predicted by Surface Area ####

Terrestrialmodel1 <- lmer(log10Abundance ~ Surface.Area.cm2 + (1 | Tag), data = spongeTlog10)
Anova(Terrestrialmodel1, type = "II")

#### Richness predicted by Surface Area (Cm2) ####

Terrestrialmodel2 <- lmer(Richness ~ Surface.Area.cm2 + (1 | Tag), data = spongeT)
Anova(Terrestrialmodel2, type = "II")
getME(Terrestrialmodel2, "theta") # this calls the random effect parameter estimate (random intercept). 
getME(Terrestrialmodel2, "sigma") # this calls the residual standard error of the fitted model. 
getME(Terrestrialmodel2, "fixef") # calls intercept and slope of fixed effect. 
# Regression equation for Terrestrial richness: Ysi = -0.17623065 + 0.7470094 + (0.01596865*Xi) + 1.910729

#### Richness predicted by SA excluding LB sponges ####

Terrestrialmodel2A <- lmer(Richness ~ Surface.Area.cm2 + (1 | Tag), data = spongeT.noLB)
Anova(Terrestrialmodel2A, type = "II")

#### Simpsons evenness predicted by Surface Area (Cm2) ####

Terrestrialmodel3 <- lmer(Simpson ~ Surface.Area.cm2 + (1 | Tag), data = spongeT)
Anova(Terrestrialmodel3, type = "II")

#### Marine and Terrestrial Combined Analysis ####

## Richness predicted by surface area and Locality and PWL ## 

MandTmodel2 <- lmer(Richness ~ Surface.Area.cm2  + Locality + PWL + (1 | Tag), data = sponge)
Anova(MandTmodel2, type = "II")
vif(MandTmodel2) 

## Richness predicted by surface area and locality and PWL and the interaction between PWL and SA ##

MandTmodel3 <- lmer(Richness ~ Surface.Area.cm2  + Locality + PWL + Surface.Area.cm2 * PWL + (1 | Tag), data = sponge)
Anova(MandTmodel3, type = "II")
vif(MandTmodel3) # SA, PWL, and SA*PWL show significant correlation. 

#### Richness Vs log10 Abundance Plot ####

spongelog10$Shape <- factor(spongelog10$Shape, levels = c("Cube", "Rectangle", "Board", "Longboard")) #order factor levels. But won't recognize the shape "Rectangle" for some reason. Converts it to NA. 
spongelog10[is.na(spongelog10)] <- "Rectangle" #Convert NA from preivous step back to Rectangle. 

RichnessVsAbundance <- ggplot(data = spongelog10, aes(x = log10Abundance, y = Richness))+
  geom_jitter(cex = 4, alpha = 1, shape = 21, stroke = 1.5, aes(color = Locality, fill = Shape))+ #using shape = 21 allows for both fill and color to be designated to a single point. Stroke argument controls point boarder thickness. 
  labs(y="Species Richness", x="log10 Abundance")+
  #ggtitle("Richness ~ log10 Abundance")+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  geom_smooth(method="lm", aes(color = Locality, fullrange=TRUE))+
  scale_fill_manual("Surface Area", labels = c("Small", "Medium", "Large", "Extra-large"), values=c("cyan3", "darkgoldenrod3", "darkolivegreen3", "brown3"))+
  scale_color_manual(labels = c("Terrebonne Bay", "Coastal Salt Marsh"), values = c("cornflowerblue", "orange4"))
RichnessVsAbundance
  
########### Richness ~ Abundance Analyses ###########

RichnessVsAbundanceM <- lmer(Richness ~ log10Abundance + (1 | Tag), data = spongeM) 
Anova(RichnessVsAbundanceM, type = "II") 

RichnessVsAbundanceT <- lmer(Richness ~ log10Abundance + (1 | Tag), data = spongeTlog10) 
Anova(RichnessVsAbundanceT, type = "II") 

########## Abundance of each species vs SA Marine ###########

spongebytaxaM = spongebytaxa %>% filter(!grepl("T", X)) # read matrix back in to recover site ID column
spongebytaxaM.triplet = spongebytaxaM %>% tidyr::gather(Species_ID, Abundance, -X) # convert site by species matrix to triplet format. 
spongebytaxaM.triplet$Species_ID <- as.factor(spongebytaxaM.triplet$Species_ID) # set Species_ID as factor
SurfaceAreacm2 <- rep(c(227, 262, 307, 450.5), times = 250) # Make Surface Area vector for triplet matrix
spongebytaxaM.triplet$Surface.Area.Cm2 <- SurfaceAreacm2 # add Surface Area vector to triplet matrix
spongebytaxaM.triplet$Abundance <- 1 + spongebytaxaM.triplet$Abundance # add 1 to all abundances so log10 transformation of 0 abundances can occur.
spongebytaxaM.triplet$log10Abundance <- log10(spongebytaxaM.triplet$Abundance) # add log10 Abundance column to matrix
spongebytaxaM.triplet1 <- subset(spongebytaxaM.triplet, Species_ID %in% c("Amph.1", "Amph.2", "Amph.3", "Amph.4", "Amph.5", "Amph.6", "Deca.1", "Deca.2", "Deca.larva", "Poly.1", "Poly.2", "Poly.3", "Poly.4", "Holo.1", "Bry.1", "Anem.1", "Hyd.1", "Gas.1", "Gas.2", "Biv.1", "Biv.2", "Nemt.1", "Nem.1", "Oli.1"))

SpeciesAbundancevsSAM1 <- ggplot(data = spongebytaxaM.triplet1, aes(x = Surface.Area.Cm2, y = log10Abundance))+
  geom_smooth(method = "lm", aes(group = Species_ID, color = Species_ID, linetype = Species_ID), se = FALSE)+
  scale_linetype_manual(values = c("solid", "dashed", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "dashed", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid"))+ 
  guides(linetype = "none")+ #removes legend for linetype 
  labs(x = "Surface Area (cm2)", y = "log10Abundance")+
  ggtitle("Terrebonne Bay")+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(legend.key.size = unit(.1, "mm"))+
  theme(legend.title = element_text(size=10))+
  theme(legend.text = element_text(size=5))+
  labs(color = "ID")+
  scale_color_manual(values=as.vector(stepped(24)))+
  scale_x_continuous(breaks = c(227, 262, 307, 450.5))

SpeciesAbundancevsSAM1



########## Abundance of each species vs SA Terrestrial ##########


spongebytaxaT = spongebytaxa %>% filter(!grepl("M", X))
spongebytaxaT.triplet = spongebytaxaT %>% tidyr::gather(Species_ID, Abundance, -X)
spongebytaxaT.triplet$Species_ID <- as.factor(spongebytaxaT.triplet$Species_ID)
SurfaceAreacm2 <- rep(c(227, 262, 307, 450.5), times = 250) # Make Surface Area vector for triplet matrix
spongebytaxaT.triplet$Surface.Area.Cm2 <- SurfaceAreacm2 # add Surface Area vector to triplet matrix
spongebytaxaT.triplet$Abundance <- 1 + spongebytaxaT.triplet$Abundance # add 1 to all abundances so log10 transformation of 0 abundances can occur.
spongebytaxaT.triplet$log10Abundance <- log10(spongebytaxaT.triplet$Abundance) # add log10 Abundance column to matrix
spongebytaxaT.triplet1 <- subset(spongebytaxaT.triplet, Species_ID %in% c("Acari.1", "Acari.2", "Amph.7", "pseudoscorp.1", "pseudoscorp.2", "Dipt.1", "Dipt.2", "Dipt.3", "Hemi.1", "Hemi.2", "Coll.1", "Coll.2", "Coll.3", "Coll.4", "Thysan.1", "Aran.1", "Aran.2", "Poly.5", "Poly.6", "Poly.7", "Gas.3", "Gas.4", "Gas.5", "Fish.1"))

SpeciesAbundancevsSAT1 <- ggplot(data = spongebytaxaT.triplet1, aes(x = Surface.Area.Cm2, y = log10Abundance))+
  geom_smooth(method = "lm", aes(group = Species_ID, color = Species_ID, linetype = Species_ID), se = FALSE)+
  scale_linetype_manual(values = c("solid", "solid", "dotted", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "dashed", "dashed", "solid", "dotted", "solid", "solid", "dotted", "solid", "solid"))+
  guides(linetype = "none")+ 
  labs(x = "Surface Area (cm2)", y = "log10Abundance")+
  ggtitle("Coastal Salt Marsh")+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(legend.key.size = unit(.1, "mm"))+
  theme(legend.title = element_text(size=10))+
  theme(legend.text = element_text(size=5))+
  labs(color = "ID")+
  scale_color_manual(values=as.vector(stepped(24)))+
  scale_x_continuous(breaks = c(227, 262, 307, 450.5))

SpeciesAbundancevsSAT1


#### Combine into 1 figure ####

SpeciesAbundancevsSA1 <- ggarrange(SpeciesAbundancevsSAM1, SpeciesAbundancevsSAT1, ncol=2, nrow=1, common.legend = F, legend="bottom", labels = c("a", "b"))
SpeciesAbundancevsSA1 

#### Visualize abundance of each species via histogram of slope values ####

## The code below runs a linear model predicting abundance of each species by SA and prints a summary of each model ##

Species_ID_T <- c("Acari.1", "Acari.2", "Amph.7", "Aran.1", "Aran.2", "Coll.1", "Coll.2", "Coll.3", "Coll.4", "Dipt.1", "Dipt.2", "Dipt.3", "Fish.1", "Gas.3", "Gas.4", "Gas.5", "Hemi.1", "Hemi.2", "Poly.5", "Poly.6", "Poly.7", "Thysan.1")
Species_modelsT = list()

for(i in Species_ID_T) {
  species_tripletT = spongebytaxaT.triplet1 %>% filter(Species_ID == i)
  Species_modelsT[[i]] = lm(log10Abundance ~ Surface.Area.Cm2, data = species_tripletT)
}
lapply(Species_modelsT, summary)

## Do it again for Marine sponges ##

Species_ID_M <- c("Amph.1", "Amph.2", "Amph.3", "Amph.4", "Amph.5", "Amph.6", "Anem.1", "Biv.1", "Biv.2", "Bry.1", "Deca.1", "Deca.2", "Deca.larva", "Gas.1", "Gas.2", "Holo.1", "Hyd.1", "Nem.1", "Nemt.1", "Oli.1", "Poly.1", "Poly.2", "Poly.3", "Poly.4")
Species_modelsM = list()

for(i in Species_ID_M) {
  species_tripletM = spongebytaxaM.triplet1 %>% filter(Species_ID == i)
  Species_modelsM[[i]] = lm(log10Abundance ~ Surface.Area.Cm2, data = species_tripletM)
}
lapply(Species_modelsM, summary)

## Now Plot the slope values via histogram to compare marine and terrestrial species responses to change in SA ##

Species_Slopes <- read.csv("Species_Slopes_Sponge2019.csv")
Species_Slopes$Locality <- as.factor(Species_Slopes$Locality)
Species_Slopes$Species_ID <- as.factor(Species_Slopes$Species_ID)

mu <- ddply(Species_Slopes, "Locality", summarise, grp.mean=mean(Slope)) # calculate the mean of each group (Terrestrial vs. Marine)
head(mu)
mu$Locality <- as.factor(mu$Locality)

ggplot(data = Species_Slopes, aes(x = Slope, color = Locality, fill = Locality))+
  geom_density(alpha = .5, size = 2)+
  labs(x = "Slope", y = "Density")+
  ggtitle("Species' Slopes")+
  geom_vline(data = mu, aes(xintercept = grp.mean, linetype = Locality), size = 1)+
  theme(panel.background = element_rect(fill="grey96", size=.5, linetype=1, color="black"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  scale_color_manual(values=c("cornflowerblue", "orange4"), labels = c("Terrebonne Bay", "Coastal Salt Marsh"))+
  scale_fill_manual(values=c("cornflowerblue", "orange4"), labels = c("Terrebonne Bay", "Coastal Salt Marsh"))+
  scale_linetype_manual(values = c(1,2), labels = c("Terrebonne Bay", "Coastal Salt Marsh"))

########## Multivariate Stuff ############

library(stringr) 

#start by reading matrix back in
sbt = read.csv("Sponge_by_Taxa_Vegan_2019.csv") 
sbt = spongebytaxa %>% select(c(1:49)) #drop unknowns
#convert NAs to 0s
sbt[is.na(sbt)] = 0
#Assign rownames
row.names(sbt) = sbt$X
#filter marine and terrestrial (because they are so different it swamps the NMDS)
sbtM = sbt %>% filter(str_detect(X, "M-"))
sbtT = sbt %>% filter(str_detect(X, "T-"))
#delete sponge id column in both
sbtM$X = NULL
sbtT$X = NULL

#need to remove sponges where no animals were found (rows which sum to 0)
#create a column that sums each row
sbtM$row_sum = rowSums(sbtM)
sbtT$row_sum = rowSums(sbtT)
#filter out row sums that are 0
sbtM_no0s = sbtM %>% filter(row_sum > 0)
sbtT_no0s = sbtT %>% filter(row_sum > 0)
#then delete row sum column for further analysis
sbtM_no0s$row_sum = NULL
sbtT_no0s$row_sum = NULL
#Run the NMDS
marine.mds = metaMDS(sbtM_no0s, k=2, trymax=100)
marine.mds

terrestrial.mds = metaMDS(sbtT_no0s, k=2, trymax=100)
terrestrial.mds

#### Now we plot ####
## Start with marine
# First have to extract "scores" aka the x and y coordinates of the sponges and species
marine.scores <- as.data.frame(scores(marine.mds, display = "sites"))
marine.scores$sponge_id = rownames(marine.scores)
#then add in the grouping variable of interest for the NMDS 
marine.scores$shape = spongedf$Shape[match(marine.scores$sponge_id, spongedf$ID)]

#do the same thing for the species now
speciesM.scores = as.data.frame(scores(marine.mds, display = "species"))
speciesM.scores$species = rownames(speciesM.scores) 

marine.scores$shape <- factor(marine.scores$shape, levels = c("Cube", "Rectangle", "Board", "Longboard")) #order factor levels. But won't recognize the shape "Rectangle" for some reason. Converts it to NA. 
marine.scores[is.na(marine.scores)] <- "Rectangle" #Convert NA from previous step back to Rectangle. 

## Plot NMDS in ggPlot ## 

NMDSTB <- ggplot() + 
  #geom_text(data=speciesM.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5, cex = 2) +  # add the species labels
  geom_point(data=marine.scores,aes(x=NMDS1,y=NMDS2,colour=shape),size=3, alpha=0.75) + # add the point markers
  ggtitle("Terrebonne Bay")+
  #geom_text(data=marine.scores,aes(x=NMDS1,y=NMDS2,label=sponge_id),size=4,vjust=0) +  # add the site labels
  coord_equal() +
  theme_bw()+
  stat_ellipse(data=marine.scores, aes(x=NMDS1,y=NMDS2, colour=shape))+ ## add ellipses according to shape
  scale_color_manual("Surface Area", labels = c("Small", "Medium", "Large", "Extra-large"), values=c("cyan3", "darkgoldenrod3", "darkolivegreen3", "brown3"))+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=14), # remove x-axis labels
        axis.title.y = element_text(size=14), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
NMDSTB


### do it all again for the terrestrial sponges ###  
terrestrial.scores = as.data.frame(scores(terrestrial.mds, display = "sites"))
terrestrial.scores$sponge_id = rownames(terrestrial.scores)
terrestrial.scores$shape = spongedf$Shape[match(terrestrial.scores$sponge_id, spongedf$ID)]
speciesT.scores = as.data.frame(scores(terrestrial.mds, "species"))
speciesT.scores$species = rownames(speciesT.scores) 

terrestrial.scores$shape <- factor(terrestrial.scores$shape, levels = c("Cube", "Rectangle", "Board", "Longboard")) #order factor levels. But won't recognize the shape "Rectangle" for some reason. Converts it to NA. 
terrestrial.scores[is.na(terrestrial.scores)] <- "Rectangle" #Convert NA from previous step back to Rectangle. 
#This is just for ordering the legend shapes in order from low SA to high SA. 

NMDSCSM <- ggplot() + 
  #geom_text(data=speciesT.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5, cex=2) +  # add the species labels
  geom_point(data=terrestrial.scores,aes(x=NMDS1,y=NMDS2,colour=shape),size=3, alpha = 0.75) + # add the point markers
  ggtitle("Coastal Salt Marsh")+
  #geom_text(data=terrestrial.scores,aes(x=NMDS1,y=NMDS2,label=sponge_id),size=4,vjust=0) +  # add the site labels
  coord_equal() +
  theme_bw()+ 
  stat_ellipse(data=terrestrial.scores, aes(x=NMDS1,y=NMDS2, colour=shape))+ ## add ellipses according to shape
  scale_color_manual("Surface Area", labels = c("Small", "Medium", "Large", "Extra-large"), values=c("cyan3", "darkgoldenrod3", "darkolivegreen3", "brown3"))+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=14), # remove x-axis labels
        axis.title.y = element_text(size=14), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
NMDSCSM


NMDSpanel <- ggarrange(NMDSTB, NMDSCSM,  widths = c(1, 1.4), nrow = 1, common.legend = T, legend = "bottom", labels = c("a", "b"), vjust = 9.5, hjust = -.5)
NMDSpanel


###### Test for effect of SA on community structure using envfit() ######
#### Start with marine ####

envDFM = sbtM_no0s # make new dataframe for marine sponges
envDFM$sponge_ID <- spongeM$ID # add sponge ID column
envDFM$shape = spongeM$Shape[match(envDFM$sponge_ID, spongeM$ID)] # add shape column
envDFM[,1:49] = NULL # drop species columns to leave only environmental DF. 

envfitM <- envfit(marine.mds, envDFM, permutations = 1000, na.rm = TRUE) # run the envfit test 
envfitM

#### Do it again for terrestrial ####

envDFT = sbtT_no0s # make new dataframe for terrestrial sponges
spongeT_ID <- c("T-5-C", "T-5-R", "T-5-B", "T-5-LB", "T-11-C", "T-11-R", "T-11-B", "T-11-LB", "T-17-R", "T-17-B", "T-17-LB", "T-22-C", "T-22-R", "T-22-B", "T-22-LB", "T-26-C", "T-26-R", "T-26-B", "T-26-LB") # do it manually because one sation was dropped with 0 abundance -> can't use match()
envDFT$sponge_ID <- spongeT_ID # add sponge ID column
envDFT$shape = spongeT$Shape[match(envDFT$sponge_ID, spongeT$ID)] # add shape column
envDFT[,1:49] = NULL # drop species columns to leave only environmental DF. 

envfitT <- envfit(terrestrial.mds, envDFT, permutations = 1000, na.rm = TRUE) # run the envfit test 
envfitT

#### Test for Nestedness using Vegan's nestedtemp() function #### 

spongebytaxaM_nest <- sbtM %>% select(-row_sum) %>% select(-c("Acari.1", "Acari.2", "Amph.7", "Aran.1", "Aran.2", "Coll.1", "Coll.2", "Coll.3", "Coll.4", "Dipt.1", "Dipt.2", "Dipt.3", "Fish.1", "Gas.3", "Gas.4", "Gas.5", "Hemi.1", "Hemi.2", "Poly.5", "Poly.6", "Poly.7", "Thysan.1", "Pseudoscorp.1", "Pseudoscorp.2")) #Make dataset containing only marine sponges and species. 
rownames(spongebytaxaM_nest) <- c("B1-S", "B1-M", "B1-L", "B1-XL", "B2-S",  "B2-M",  "B2-L", "B2-XL", "B3-S",  "B3-M",  "B3-L",  "B3-XL", "B4-S",  "B4-M", "B4-L",  "B4-XL", "B5-S",  "B5-M",  "B5-L",  "B5-XL") # Change row names to be intuitive (Block1-S, Block1-M, Block1-L, Block1-XL, etc.)
sponge_nest_marine <- nestedtemp(spongebytaxaM_nest, niter = 1000)
sponge_nest_marine

spongebytaxaT_nest <- sbtT %>% select(-row_sum) %>% select(-c("Amph.1", "Amph.2", "Amph.3", "Amph.4", "Amph.5", "Amph.6", "Anem.1", "Biv.1", "Biv.2", "Bry.1", "Deca.1", "Deca.2", "Deca.larva", "Gas.1", "Gas.2", "Holo.1", "Hyd.1", "Nem.1", "Nemt.1", "Oli.1", "Poly.1", "Poly.2", "Poly.3", "Poly.4")) #Make dataframe containing only terrestrial sponges and species. 
rownames(spongebytaxaT_nest) <- c("B10-S", "B10-M", "B10-L", "B10-XL", "B8-S",  "B8-M",  "B8-L", "B8-XL", "B6-S",  "B6-M",  "B6-L",  "B6-XL", "B7-S",  "B7-M", "B7-L",  "B7-XL", "B9-S",  "B9-M",  "B9-L",  "B9-XL") # Change row names to be intuitive (terrestrial to Coastal salt marsh)
sponge_nest_terrestrial <- nestedtemp(spongebytaxaT_nest, niter = 1000)
sponge_nest_terrestrial

par(mar = c(3,7,7,3)) #adjust margins so that text is not cut off. 

par(mfrow = c(1,1))

NestColorsM <- c("cornflowerblue", "lightgray")

sponge_nest_marine <- nestedtemp(spongebytaxaM_nest, niter = 1000)
NestPlotM <- plot(sponge_nest_marine, col=rev(NestColorsM), names = TRUE, kind = "incidence", cex.axis = .75) # plot using incidence 

NestColorsT <- c("orange4", "lightgray")

sponge_nest_terrestrial <- nestedtemp(spongebytaxaT_nest, niter = 1000)
NestPlotT <- plot(sponge_nest_terrestrial, col=rev(NestColorsT), names = TRUE, kind = "incidence", cex.axis = .75)

#### Nestedness Null Models ####

null_nest_marine <- oecosimu(spongebytaxaM_nest, nestfun = "nestedtemp", method = "r00", nsimul = 1000, alternative = "less") 
null_nest_marine
# Temperature of observed community is significantly less than (more nested) than null community simulation. 

null_nest_terrestrial <- oecosimu(spongebytaxaT_nest, nestfun = "nestedtemp", method = "r00", nsimul = 1000, alternative = "less")
null_nest_terrestrial
# Temperature of observed community is significantly less than (more nested) than null community simulation. 
