rm(list=ls())  #Erase workspace
library(sf)
library(sp)
library(rstudioapi)
library(dplyr)
library(ggplot2)

setwd(dirname(getActiveDocumentContext()$path)) #set file location as working directory

####Prepping and loading the surveillance data and shapefile####
rio_negro_samples <- read.csv("All_samples.csv")
colnames(rio_negro_samples) <- c("LAT", "LON", "Elisa_sample_1", "Elisa_pos_1", "Necropsy_sample_1", "Necrposy_pos_1", "Elisa_sample_2", "Elisa_pos_2", "Elisa_sample_3", "Elisa_pos_3", "Necropsy_sample_2", "Necrposy_pos_2", "Elisa_sample_4", "Elisa_pos_4")
rio_negro_samples <- rio_negro_samples[, -c(15,16)]
rio_negro_samples$n_sample <- rowSums(rio_negro_samples[, c(3, 5, 7, 9, 11, 13)], na.rm = TRUE)
rio_negro_samples$n_positive <- rowSums(rio_negro_samples[, c(4, 6, 8, 10, 12, 14)], na.rm = TRUE)

rio_negro_samples$n_sample[rio_negro_samples$n_sample == 0 & rio_negro_samples$n_positive == 0] <- NA
rio_negro_samples$n_positive[is.na(rio_negro_samples$n_sample) & rio_negro_samples$n_positive == 0] <- NA
rio_negro_samples$prev<- rio_negro_samples$n_positive / rio_negro_samples$n_sample
rio_negro_samples <- rio_negro_samples[, -c(3:14)]
library(leaflet)
library(raster)

#Plotting the locations of samples using leaflet
pal <- colorNumeric("viridis", c(0, 1), na.color = "grey")

leaflet(rio_negro_samples) %>%  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~LON, lat = ~LAT, color = ~pal(prev)) %>%
  addLegend("bottomright", pal = pal, values = ~prev, title = "Farms tested positive for Ec") %>%
  addScaleBar(position = c("bottomleft"))

pal_function <- scale_colour_viridis_c(guide = "legend", breaks = unique(rio_negro_samples$prev))

# Create the plot
ggplot(rio_negro_samples, aes(x = LON, y = LAT, color = prev)) +
  geom_point(aes(size = ifelse(is.na(prev), 1, 3))) +  # Size of the points based on NA status
  scale_colour_viridis_c(name = "Farm Ec Prevalence", na.value = "grey50") +  # Continuous color scale with a legend title and grey color for NA values
  scale_size_identity(guide = "none") +  # Use identity scale for size and don't show a size legend
  labs(x = "Longitude", y = "Latitude") +  # Custom axis labels
  coord_fixed(ratio = 1) +  # Keep the aspect ratio consistent
  theme_minimal() +  # Minimal theme for cleaner visualization
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()  # Remove axis text (numbers)
  )


r <- raster(ncol=100, nrow=100, xmn=-71.6, xmx=-70, ymn=-42, ymx=-41.1)
r[] <- 1:length(r)

#Building triangulation mesh over the sampled regions
library(INLA)
coo <- cbind(rio_negro_samples$LON, rio_negro_samples$LAT)
mesh <- inla.mesh.2d(loc = coo, max.edge = c(0.2, 5), cutoff = 0.01)

mesh$n

plot(mesh)
points(coo, col = "red")

#Building the SPDE model on the mesh
spde <- inla.spde2.matern(mesh = mesh, alpha = 0.1)

#Index set
indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)

#Projection matrix
A <- inla.spde.make.A(mesh = mesh, loc = coo)

#Prediction locations - area where the model will predict data 
dp <- rio_negro_samples[,c(2,1)]
colnames(dp) <- c("x", "y")
coop <- as.matrix(dp)

#Projector matrix
Ap <- inla.spde.make.A(mesh = mesh, loc = coop)

#stack for estimation stk.e
stk.e <- inla.stack(tag = "est",
                    data = list(y = rio_negro_samples$n_positive, numtrials = rio_negro_samples$n_sample),
                    A = list(1, A),
                    effects = list(data.frame(b0 = rep(1, times = nrow(rio_negro_samples))), s = indexs))

#stack for prediction stk.p
stk.p <- inla.stack(tag = "pred",
                    data = list(y = NA, numtrials = NA),
                    A = list(1, Ap),
                    effects = list(data.frame(b0 = rep(1, times = nrow(dp))), s = indexs))

#stk.full has stk.e and stk.p
stk.full <- inla.stack(stk.e, stk.p)

#Formula for model
formula <- y ~ 0 + b0 + f(s, model = spde)

#Running the model
res <- inla(formula, family = "binomial", Ntrials = numtrials,
            data = inla.stack.data(stk.full),
            control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)))

summary(res)
index <- inla.stack.index(stack = stk.full, tag = "pred")$data

prev_mean <- res$summary.fitted.values[index, "mean"]
prev_ll <- res$summary.fitted.values[index, "0.025quant"]
prev_ul <- res$summary.fitted.values[index, "0.975quant"]
prev_sd <- res$summary.fitted.values[index, "sd"]
prev_med <- res$summary.fitted.values[index, "0.5quant"]

save.image("INLAData_rio.RData")

#### Model output visualisation ####
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) #set file location as working directory
load("INLAData_rio.RData")


library(dplyr)
library(leaflet)
library(raster)
library(INLA)
library(ggplot2)
library(viridis)
library(grid)
library(gridExtra)

#Plotting the mean prevalence 
r_prev_mean <- as.data.frame(coop)
r_prev_mean$prev <- prev_mean
colnames(r_prev_mean) <- c("long", "lat", "prev")

r_prev_ul <- as.data.frame(coop)
r_prev_ul$prev <- prev_ul
colnames(r_prev_ul) <- c("long", "lat", "prev")

r_prev_ll <- as.data.frame(coop)
r_prev_ll$prev <- prev_ll
colnames(r_prev_ll) <- c("long", "lat", "prev")

pal_function <- scale_colour_viridis_c(guide = "legend", breaks = unique(r_prev_mean$prev))

m <- ggplot(rio_negro_samples, aes(x=long, y=lat)) +
  geom_point(data=r_prev_mean, mapping=aes(color=prev), size = 3) +
  scale_colour_viridis_c(name = "Farm Ec Prevalence", na.value = "grey50", labels=scales::percent, limits=c(0.07, 0.65)) +
  ggtitle("A")+
  coord_map() +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       color = "Mean Prevalence") +  # Change fill to color here as well
  theme(legend.position = c(0.20,0.7), 
        legend.key.size = unit(0.7, 'cm'), 
        legend.key.height = unit(0.6, 'cm'),
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = (16)), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title=element_text(size=16))

mul <- ggplot(rio_negro_samples, aes(x=long, y=lat)) +
  geom_point(data=r_prev_ul, mapping=aes(color=prev), size = 2) +
  scale_colour_viridis_c(name = "Farm Ec Prevalence", na.value = "grey50", labels=scales::percent, limits=c(0.07, 0.65)) +
  ggtitle("B")+
  coord_map() +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       color = "Mean Prevalence") +  # Change fill to color here as well
  theme(legend.position = "none", 
        legend.key.size = unit(2.8, 'cm'), 
        legend.key.height = unit(2, 'cm'),
        legend.title = element_text(size=42), 
        legend.text = element_text(size=38),
        plot.title = element_text(hjust = 0.5, face = "bold", size = (16)), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title=element_text(size=16))


mll <- ggplot(rio_negro_samples, aes(x=long, y=lat)) +
  geom_point(data=r_prev_ll, mapping=aes(color=prev), size = 2) +
  scale_colour_viridis_c(name = "Farm Ec Prevalence", na.value = "grey50", labels=scales::percent, limits=c(0.07, 0.65)) +
  ggtitle("C")+
  coord_map() +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       color = "Mean Prevalence") +  # Change fill to color here as well
  theme(legend.position = "none", 
        legend.key.size = unit(2.8, 'cm'), 
        legend.key.height = unit(2, 'cm'),
        legend.title = element_text(size=42), 
        legend.text = element_text(size=38),
        plot.title = element_text(hjust = 0.5, face = "bold", size = (16)), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title=element_text(size=16))

#png("Figure 2.png", width = 2900, height = 2000)
grid.arrange(
  grobs = list(m, mll, mul),
  name = "test",
  nrow = 2,
  ncol = 3,
  layout_matrix = rbind(c(1, 1, 3),
                        c(1, 1, 2)
  )
)
#dev.off()

prev_dt <- data.frame(mean = rep(NA, length(prev_mean)))
prev_dt$mean <- prev_mean #0.416723
prev_dt$ll <- prev_ll
prev_dt$ul <- prev_ul
prev_dt$sd <- prev_sd
prev_dt$median <- prev_med
write.csv(prev_dt, file = "Spatial prev distribution.csv")

prev_vis <- data.frame(Long = rio_negro_samples$LON, Lat= rio_negro_samples$LAT, Mean = prev_dt$mean, Lower = prev_dt$ll, Upper = prev_dt$ul)

ggplot(prev_vis) + 
  geom_density(aes(x=Lower, fill="5% Quantile"), alpha=0.7) + 
  geom_density(aes(x=Upper, fill="95% Quantile"), alpha=0.7) + 
  geom_density(aes(x=Mean, fill="Mean"), alpha=0.7) + 
  scale_fill_manual(values = c("5% Quantile"="lightyellow", "95% Quantile"="orange", "Mean"="lightblue"),
                    name = "Variable") +
  theme_minimal() + 
  labs(title="Density Plot of Prevalence in farms", x="Prevalence", y="Density")
