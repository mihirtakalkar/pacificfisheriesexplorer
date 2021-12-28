# Plot SST by West Coast Ecoregion #
plot_ecoregion_sst <- function(datasetmeow, datasetsst, region){
  # MEOWs
  meows_use <- c("Oregon, Washington, Vancouver Coast and Shelf",
                 "Northern California",
                 "Southern California Bight",
                 "Magdalena Transition",
                 "Cortezian",
                 "Mexican Tropical Pacific",
                 "Chiapas-Nicaragua")

  # Format MEOWs
  meows <- datasetmeow %>%
    filter(ecoregion %in% meows_use)

  # Plot data
  ################################################################################

  # Land
  states <- rnaturalearth::ne_states(country=c("Mexico", "United States of America"), returnclass="sf")
  countries <- rnaturalearth::ne_countries(returnclass="sf", scale = "large")

  # Function to plot time series
  # Format data
  ecoregion_do <- region
  edata <- datasetsst %>%
    filter(ecoregion==ecoregion_do)

  # Plot data
  g2 <- ggplot(edata, aes(x=year, y=sst_c)) +
    geom_line(size=0.5) +
    # Labels
    labs(y="SST (°C)", x="", title=ecoregion_do) +
    scale_x_continuous(breaks=seq(1880, 2020, 20)) +
    # Theme
    theme_bw() +
    theme(axis.text=element_text(size=6),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=7),
          plot.title=element_text(size=7),
          # Gridlines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))

  g2
}

plot_ecoregion <- function(region){
  g1 <- ggplot( ) +
    # Plot MEOWs
    geom_sf(data=meows, mapping=aes(fill=ecoregion), show.legend = F) +
    # Plot land
    geom_sf(data=countries, fill="grey90", color=NA) +
    geom_sf(data=states, fill="grey70", color="white", lwd=0.2) +
    geom_sf(data=countries, fill=NA, color="grey30", lwd=0.3) +
    # Label MEOWs
    geom_sf_label(data=meows, mapping=aes(label=ecoregion), show.legend = F, size=2.5) +
    # Crop
    coord_sf(xlim=c(-127, -91), ylim=c(11, 49)) +
    # Labels
    labs(x="", y="") +
    # Theme
    theme_bw() +
    theme(axis.text=element_text(size=6),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.title=element_blank(),
          plot.title=element_blank(),
          # Gridlines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  g1
}
