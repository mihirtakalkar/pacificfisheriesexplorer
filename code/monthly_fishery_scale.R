# Plot Monthly Fishery Scale Metrics #
plot_fishery_monthly <- function(dataset, varyear){
  years <- 2001:2019
  scaleData <- dataset %>%
    # Eliminate Columns #
    filter(fishery_type != "Unknown") %>%
    dplyr::select(year, month, fishery_type, sci_name, landings_kg, value_mxn) %>%
    # Calculate Total Landings for Fishery Type #
    group_by(fishery_type, year, month)%>%
    summarize(landings_kg_tot=sum(landings_kg),
              value_mxn_tot=sum(value_mxn) / 10e6) %>%
    ungroup() %>%
    filter(year == varyear)

  g1 <- ggplot(scaleData, mapping = aes(x = month, y = value_mxn_tot, color = fishery_type)) +
    geom_line(size = 1.5, alpha = 0.6) +
    labs(title = "Fishery Landing Values Over Time",
         subtitle = "How do artisanal and industrial fishery revenues differ?",
         x ="Month", y = "Value in Pesos (Millions)", color = "Fishery Type") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(), text = element_text(size = 14, family = "Segoe UI"))
  g1
}
