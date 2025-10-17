library(igraph)
library(ggplot2)
library(dplyr)

# i Created the complete movement data with all necessary columns
movement_data <- data.frame(
  from_id = c(88555, 164789, 124263, 166030, 125272, 67710, 169400, 148460, 76941, 128252),
  to_id = c(148790, 125789, 68383, 78803, 145810, 34770, 89644, 16169, 57178, 46876),
  cattle_moved = c(1, 1, 3, 1, 1, 1, 3, 6, 1, 1),
  from_class = c("B", "D", "M", "B", "S", "D", "B", "S", "D", "B"),
  to_class = c("B", "T", "B", "S", "F", "D", "T", "F", "T", "B")
)

# Get all unique farm IDs
all_farm_ids <- unique(c(movement_data$from_id, movement_data$to_id))
n_farms <- length(all_farm_ids)

# I Created farm metadata with realistic herd sizes and SIR states
farm_metadata <- data.frame(
  farm_id = all_farm_ids,
  herd_size = sample(20:200, n_farms, replace = TRUE),
  herd_type = sample(c("Beef", "Dairy", "Fattener", "Store", "Trade"), 
                     n_farms, replace = TRUE, 
                     prob = c(0.3, 0.3, 0.2, 0.1, 0.1))
)

# Initialize SIR states (ensuring S + I + R = herd_size)
for(i in 1:n_farms) {
  herd_size <- farm_metadata$herd_size[i]
  I <- sample(0:min(5, herd_size), 1)  # 0-5 infected
  R <- sample(0:min(10, herd_size - I), 1)  # 0-10 recovered
  S <- herd_size - I - R  # Remainder susceptible
  
  farm_metadata$S[i] <- S
  farm_metadata$I[i] <- I
  farm_metadata$R[i] <- R
}

# Merge movement data with farm metadata
complete_data <- movement_data %>%
  left_join(farm_metadata, by = c("from_id" = "farm_id")) %>%
  rename(from_herd_size = herd_size, from_herd_type = herd_type, 
         from_S = S, from_I = I, from_R = R) %>%
  left_join(farm_metadata, by = c("to_id" = "farm_id")) %>%
  rename(to_herd_size = herd_size, to_herd_type = herd_type, 
         to_S = S, to_I = I, to_R = R)

# Display the complete structured data
print("COMPLETE MOVEMENT AND EPIDEMIOLOGICAL DATA:")
print(complete_data)

# Create network for visualization
network_vertices <- farm_metadata
trade_network <- graph_from_data_frame(
  d = complete_data[, c("from_id", "to_id")],
  directed = TRUE,
  vertices = network_vertices
)

# Set visual properties
V(trade_network)$size <- network_vertices$herd_size / 10 + 5
V(trade_network)$color <- ifelse(network_vertices$I > 0, "red", 
                                 ifelse(network_vertices$R > 0, "orange", "green"))
V(trade_network)$label <- paste0(network_vertices$farm_id, "\n",
                                 "H:", network_vertices$herd_size, "\n",
                                 "S:", network_vertices$S, " I:", network_vertices$I, " R:", network_vertices$R)

E(trade_network)$width <- complete_data$cattle_moved * 2 + 1
E(trade_network)$color <- "gray"
E(trade_network)$label <- complete_data$cattle_moved

# Create visualization
set.seed(123)
layout <- layout_with_fr(trade_network)

par(mar = c(1, 1, 3, 1))
plot(trade_network, 
     layout = layout,
     vertex.frame.color = "black",
     vertex.label.cex = 0.6,
     edge.arrow.size = 0.5,
     edge.curved = 0.2,
     main = "Cattle Trade Network with SIR States\n(H: Herd Size, S: Susceptible, I: Infected, R: Recovered)"
)

legend("bottomright", 
       legend = c("Infected (I > 0)", "Recovered (R > 0, I = 0)", "Susceptible (S only)"),
       fill = c("red", "orange", "green"),
       title = "Epidemiological State",
       cex = 0.8)

# Export the clean data structure for your SEIR model
final_data_structure <- complete_data %>%
  select(from_id, to_id, cattle_moved, 
         from_herd_size, from_herd_type, from_S, from_I, from_R,
         to_herd_size, to_herd_type, to_S, to_I, to_R)

print("\nCLEAN DATA STRUCTURE FOR SEIR MODEL:")
print(final_data_structure)

# Summary statistics
cat("\n=== NETWORK SUMMARY ===\n")
cat("Total farms:", n_farms, "\n")
cat("Total cattle in network:", sum(farm_metadata$herd_size), "\n")
cat("Total movements:", nrow(movement_data), "\n")
cat("Total cattle moved:", sum(movement_data$cattle_moved), "\n")
cat("Infected farms:", sum(farm_metadata$I > 0), "/", n_farms, "\n")
cat("Average herd size:", mean(farm_metadata$herd_size), "\n")

###MOVEMENT OF CATTLES
library(igraph)
library(ggplot2)
library(dplyr)
library(lubridate)
library(visNetwork)

# Create the movement data with your actual month-year codes
movement_data <- data.frame(
  the_month = c(202009, 201804, 200904, 200910, 202210, 201012, 202004, 201112, 200804, 202207),
  from_id = c(88555, 164789, 124263, 166030, 125272, 67710, 169400, 148460, 76941, 128252),
  to_id = c(148790, 125789, 68383, 78803, 145810, 34770, 89644, 16169, 57178, 46876),
  cattle_moved = c(1, 1, 3, 1, 1, 1, 3, 6, 1, 1)
)

# Convert YYYYMM format to proper dates (first day of each month)
movement_data <- movement_data %>%
  mutate(
    movement_date = as.Date(paste0(the_month, "01"), format = "%Y%m%d"),
    month_year = format(movement_date, "%b %Y"),
    year = year(movement_date),
    month = month(movement_date)
  ) %>%
  arrange(movement_date)

# Get all unique farm IDs
all_farm_ids <- unique(c(movement_data$from_id, movement_data$to_id))
n_farms <- length(all_farm_ids)

# Create farm metadata with realistic herd sizes
set.seed(123)
farm_metadata <- data.frame(
  farm_id = all_farm_ids,
  herd_size = sample(20:200, n_farms, replace = TRUE),
  herd_type = sample(c("Beef", "Dairy", "Fattener", "Store", "Trade"), 
                     n_farms, replace = TRUE, 
                     prob = c(0.3, 0.3, 0.2, 0.1, 0.1))
)

# Initialize SIR states (ensuring S + I + R = herd_size)
for(i in 1:n_farms) {
  herd_size <- farm_metadata$herd_size[i]
  I <- sample(0:min(5, herd_size), 1)
  R <- sample(0:min(10, herd_size - I), 1)
  S <- herd_size - I - R
  
  farm_metadata$S[i] <- S
  farm_metadata$I[i] <- I
  farm_metadata$R[i] <- R
}

# Show the actual dates
print("Movement dates with original codes:")
print(movement_data[, c("the_month", "movement_date", "month_year")])

# Create individual monthly plots instead of GIF
create_monthly_plots <- function(movement_data, farm_metadata) {
  months <- unique(movement_data$movement_date)
  
  for (i in seq_along(months)) {
    current_month <- months[i]
    
    # Get movements up to this month
    current_movements <- movement_data %>% filter(movement_date <= current_month)
    
    if(nrow(current_movements) > 0) {
      current_network <- graph_from_data_frame(
        d = current_movements[, c("from_id", "to_id")],
        directed = TRUE,
        vertices = farm_metadata
      )
      
      # Set visual properties
      V(current_network)$size <- farm_metadata$herd_size / 10 + 5
      V(current_network)$color <- ifelse(farm_metadata$I > 0, "red", 
                                         ifelse(farm_metadata$R > 0, "orange", "green"))
      
      E(current_network)$width <- current_movements$cattle_moved * 3 + 1
      E(current_network)$color <- "blue"
      E(current_network)$label <- current_movements$cattle_moved
      
      # Create plot
      png(filename = paste0("movement_", format(current_month, "%Y%m"), ".png"), 
          width = 800, height = 600)
      
      plot(current_network, 
           main = paste("Cattle Movements -", format(current_month, "%b %Y")),
           vertex.label = farm_metadata$farm_id,
           vertex.label.cex = 0.7,
           edge.arrow.size = 0.4,
           edge.curved = 0.2,
           layout = layout_with_fr)
      
      legend("bottomright", 
             legend = c(paste("Movements:", nrow(current_movements)),
                        paste("Total cattle:", sum(current_movements$cattle_moved)),
                        paste("Date:", format(current_month, "%b %Y"))),
             bty = "n", cex = 0.8)
      
      dev.off()
    }
  }
  cat("Individual monthly plots saved as PNG files\n")
}

# Create the monthly plots
create_monthly_plots(movement_data, farm_metadata)

# Monthly movement summary
monthly_summary <- movement_data %>%
  group_by(the_month, month_year, movement_date) %>%
  summarize(
    movements = n(),
    total_cattle = sum(cattle_moved),
    .groups = 'drop'
  ) %>%
  arrange(movement_date)

print("Monthly Movement Summary:")
print(monthly_summary)

# Plot monthly movement trends
ggplot(monthly_summary, aes(x = movement_date, y = movements)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = movements), vjust = -0.5, size = 3) +
  labs(title = "Cattle Movements by Month",
       x = "Month", y = "Number of Movements",
       subtitle = "Temporal pattern of cattle trade (2008-2022)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Interactive timeline using visNetwork
create_interactive_timeline <- function(movement_data, farm_metadata) {
  nodes <- data.frame(
    id = farm_metadata$farm_id,
    label = paste0("Farm ", farm_metadata$farm_id),
    value = farm_metadata$herd_size / 10,
    color = ifelse(farm_metadata$I > 0, "red", 
                   ifelse(farm_metadata$R > 0, "orange", "green")),
    title = paste("Type:", farm_metadata$herd_type,
                  "<br>Herd size:", farm_metadata$herd_size,
                  "<br>S:", farm_metadata$S, "I:", farm_metadata$I, "R:", farm_metadata$R)
  )
  
  edges <- data.frame(
    from = movement_data$from_id,
    to = movement_data$to_id,
    value = movement_data$cattle_moved,
    title = paste(movement_data$cattle_moved, "cattle moved in", movement_data$month_year),
    color = "blue",
    arrows = "to"
  )
  
  visNetwork(nodes, edges) %>%
    visOptions(highlightNearest = TRUE) %>%
    visLayout(randomSeed = 123) %>%
    visEdges(smooth = FALSE) %>%
    visPhysics(stabilization = FALSE)
}

# Generate interactive timeline
interactive_plot <- create_interactive_timeline(movement_data, farm_metadata)
interactive_plot

# Export time-stamped data for SEIR model
time_series_data <- movement_data %>%
  left_join(farm_metadata, by = c("from_id" = "farm_id")) %>%
  rename(from_herd_size = herd_size, from_herd_type = herd_type, 
         from_S = S, from_I = I, from_R = R) %>%
  left_join(farm_metadata, by = c("to_id" = "farm_id")) %>%
  rename(to_herd_size = herd_size, to_herd_type = herd_type, 
         to_S = S, to_I = I, to_R = R) %>%
  select(the_month, movement_date, month_year, from_id, to_id, cattle_moved, everything())

print("Time-Stamped Movement Data for SEIR Model:")
print(time_series_data)

# Summary statistics
cat("\n=== TEMPORAL NETWORK ANALYSIS ===\n")
cat("Time period:", format(min(movement_data$movement_date), "%b %Y"), "to", 
    format(max(movement_data$movement_date), "%b %Y"), "\n")
cat("Total months with movements:", nrow(monthly_summary), "\n")
cat("Total movements:", nrow(movement_data), "\n")
cat("Total cattle moved:", sum(movement_data$cattle_moved), "\n")
cat("Month with most movements:", monthly_summary$month_year[which.max(monthly_summary$movements)], "\n")
cat("Month with most cattle moved:", monthly_summary$month_year[which.max(monthly_summary$total_cattle)], "\n")

# Show the date conversion
cat("\n=== DATE CONVERSION ===\n")
for(i in 1:nrow(movement_data)) {
  cat(sprintf("Original: %d -> Converted: %s\n", 
              movement_data$the_month[i], 
              movement_data$month_year[i]))
}

# Additional temporal analysis: Yearly summary
yearly_summary <- movement_data %>%
  group_by(year) %>%
  summarize(
    movements = n(),
    total_cattle = sum(cattle_moved),
    unique_farms = n_distinct(c(from_id, to_id)),
    .groups = 'drop'
  )

print("Yearly Summary:")
print(yearly_summary)

# Plot yearly trends
ggplot(yearly_summary, aes(x = year, y = movements)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = movements), vjust = -0.5, size = 4) +
  labs(title = "Cattle Movements by Year",
       x = "Year", y = "Number of Movements") +
  theme_minimal()


### UNCERTAINTY
library(igraph)
library(ggplot2)
library(dplyr)
library(lubridate)
library(visNetwork)
library(tidyr)  # Added for pivot_longer
library(patchwork)

#I  Created the movement data with enhanced parameters
movement_data <- data.frame(
  the_month = c(202009, 201804, 200904, 200910, 202210, 201012, 202004, 201112, 200804, 202207),
  from_id = c(88555, 164789, 124263, 166030, 125272, 67710, 169400, 148460, 76941, 128252),
  to_id = c(148790, 125789, 68383, 78803, 145810, 34770, 89644, 16169, 57178, 46876),
  cattle_moved = c(1, 1, 3, 1, 1, 1, 3, 6, 1, 1)
)

# I Converted dates and add movement rates (X_ji) with uncertainty
movement_data <- movement_data %>%
  mutate(
    movement_date = as.Date(paste0(the_month, "01"), format = "%Y%m%d"),
    month_year = format(movement_date, "%b %Y"),
    
    # Base movement rate (animals per month)
    movement_rate = cattle_moved * runif(n(), 0.8, 1.2),
    
    # Uncertainty ranges for movement rates
    rate_lower = movement_rate * runif(n(), 0.7, 0.9),
    rate_upper = movement_rate * runif(n(), 1.1, 1.3),
    
    # Confidence level
    confidence = runif(n(), 0.6, 0.95)
  ) %>%
  arrange(movement_date)

# Get all unique farm IDs
all_farm_ids <- unique(c(movement_data$from_id, movement_data$to_id))
n_farms <- length(all_farm_ids)

# Create farm metadata with epidemiological parameters and uncertainty
set.seed(123)
farm_metadata <- data.frame(
  farm_id = all_farm_ids,
  herd_size = sample(20:200, n_farms, replace = TRUE),
  herd_type = sample(c("Beef", "Dairy", "Fattener", "Store", "Trade"), 
                     n_farms, replace = TRUE, 
                     prob = c(0.3, 0.3, 0.2, 0.1, 0.1)),
  
  # Transmission rate (β) with uncertainty
  beta = runif(n_farms, 0.1, 0.3),
  beta_lower = runif(n_farms, 0.08, 0.25),
  beta_upper = runif(n_farms, 0.15, 0.35),
  
  # Recovery rate (γ) with uncertainty
  gamma = runif(n_farms, 0.05, 0.15),
  gamma_lower = runif(n_farms, 0.04, 0.12),
  gamma_upper = runif(n_farms, 0.08, 0.18)
)

# Initialize SIR states with uncertainty
for(i in 1:n_farms) {
  herd_size <- farm_metadata$herd_size[i]
  I <- sample(0:min(5, herd_size), 1)
  R <- sample(0:min(10, herd_size - I), 1)
  S <- herd_size - I - R
  
  farm_metadata$S[i] <- S
  farm_metadata$I[i] <- I
  farm_metadata$R[i] <- R
  farm_metadata$I_upper[i] <- I * runif(1, 1.1, 1.5)
  farm_metadata$I_lower[i] <- I * runif(1, 0.5, 0.9)
}

# 4. VISUALIZE MOVEMENT RATES (α_ab) WITH UNCERTAINTY
create_movement_rate_visualization <- function(movement_data) {
  # Movement rate heatmap
  movement_matrix <- movement_data %>%
    ggplot(aes(x = factor(from_id), y = factor(to_id), fill = movement_rate)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", movement_rate)), size = 3, color = "black") +
    scale_fill_gradient(low = "lightblue", high = "darkred", name = "Movement Rate\n(α_ab)") +
    labs(title = "Movement Rates between Farms (α_ab)",
         x = "Source Farm", y = "Destination Farm") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Movement rate distribution with uncertainty
  rate_distribution <- movement_data %>%
    ggplot(aes(x = reorder(paste(from_id, "→", to_id), movement_rate), y = movement_rate)) +
    geom_pointrange(aes(ymin = rate_lower, ymax = rate_upper), 
                    color = "steelblue", size = 0.5) +
    coord_flip() +
    labs(title = "Movement Rates with Uncertainty Ranges",
         x = "Farm Pair", y = "Movement Rate (α_ab)") +
    theme_minimal()
  
  return(list(heatmap = movement_matrix, distribution = rate_distribution))
}

# 5. IMPLEMENT INTERVENTION SCENARIOS
implement_interventions <- function(movement_data, farm_metadata, scenario) {
  intervention_data <- movement_data
  intervention_metadata <- farm_metadata
  
  if(scenario == "movement_restriction") {
    infected_farms <- farm_metadata$farm_id[farm_metadata$I > 0]
    intervention_data <- intervention_data %>%
      mutate(movement_rate = ifelse(from_id %in% infected_farms, 
                                    movement_rate * 0.1,
                                    movement_rate))
    
  } else if(scenario == "enhanced_biosecurity") {
    intervention_metadata <- intervention_metadata %>%
      mutate(beta = beta * 0.5,
             beta_lower = beta_lower * 0.5,
             beta_upper = beta_upper * 0.5)
    
  } else if(scenario == "targeted_surveillance") {
    high_risk_farms <- intervention_data$to_id[intervention_data$movement_rate > 2]
    intervention_metadata <- intervention_metadata %>%
      mutate(gamma = ifelse(farm_id %in% high_risk_farms, 
                            gamma * 1.5,
                            gamma))
  }
  
  return(list(movement_data = intervention_data, farm_metadata = intervention_metadata))
}

# 6. UNCERTAINTY VISUALIZATION
create_uncertainty_visualization <- function(farm_metadata) {
  # Beta parameter uncertainty
  beta_plot <- farm_metadata %>%
    ggplot(aes(x = reorder(factor(farm_id), beta), y = beta)) +
    geom_pointrange(aes(ymin = beta_lower, ymax = beta_upper), 
                    color = "red", size = 0.5) +
    coord_flip() +
    labs(title = "Transmission Rate (β) with Uncertainty",
         x = "Farm ID", y = "β value") +
    theme_minimal()
  
  # Gamma parameter uncertainty
  gamma_plot <- farm_metadata %>%
    ggplot(aes(x = reorder(factor(farm_id), gamma), y = gamma)) +
    geom_pointrange(aes(ymin = gamma_lower, ymax = gamma_upper), 
                    color = "green", size = 0.5) +
    coord_flip() +
    labs(title = "Recovery Rate (γ) with Uncertainty",
         x = "Farm ID", y = "γ value") +
    theme_minimal()
  
  return(list(beta = beta_plot, gamma = gamma_plot))
}

# Generate visualizations
movement_plots <- create_movement_rate_visualization(movement_data)
uncertainty_plots <- create_uncertainty_visualization(farm_metadata)

# Display movement rate visualizations
print(movement_plots$heatmap)
print(movement_plots$distribution)

# Display uncertainty visualizations
print(uncertainty_plots$beta)
print(uncertainty_plots$gamma)

# Compare intervention scenarios - FIXED VERSION
scenarios <- c("baseline", "movement_restriction", "enhanced_biosecurity", "targeted_surveillance")
intervention_results <- list()

for(scenario in scenarios) {
  if(scenario == "baseline") {
    intervention_results[[scenario]] <- list(
      movement_data = movement_data,
      farm_metadata = farm_metadata
    )
  } else {
    intervention_results[[scenario]] <- implement_interventions(movement_data, farm_metadata, scenario)
  }
}

# Create comparison data manually (alternative to pivot_longer)
scenario_comparison <- data.frame()
for(scenario in scenarios) {
  data <- intervention_results[[scenario]]
  scenario_comparison <- rbind(scenario_comparison, data.frame(
    Scenario = scenario,
    Total_Movement_Rate = sum(data$movement_data$movement_rate),
    Mean_Beta = mean(data$farm_metadata$beta),
    Mean_Gamma = mean(data$farm_metadata$gamma),
    Infected_Farms = sum(data$farm_metadata$I > 0)
  ))
}

# Manual reshaping for plotting
plot_data <- data.frame()
for(param in c("Total_Movement_Rate", "Mean_Beta", "Mean_Gamma", "Infected_Farms")) {
  for(i in 1:nrow(scenario_comparison)) {
    plot_data <- rbind(plot_data, data.frame(
      Scenario = scenario_comparison$Scenario[i],
      Parameter = param,
      Value = scenario_comparison[i, param]
    ))
  }
}

comparison_plot <- ggplot(plot_data, aes(x = Scenario, y = Value, fill = Scenario)) +
  geom_col() +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(title = "Intervention Scenario Comparison",
       subtitle = "Impact on key epidemiological parameters",
       y = "Parameter Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(comparison_plot)

# Enhanced network visualization
create_enhanced_network <- function(movement_data, farm_metadata, title_suffix = "") {
  network <- graph_from_data_frame(
    d = movement_data[, c("from_id", "to_id")],
    directed = TRUE,
    vertices = farm_metadata
  )
  
  # Node properties
  V(network)$size <- farm_metadata$beta * 50 + 10
  V(network)$color <- ifelse(farm_metadata$I > 0, "red", 
                             ifelse(farm_metadata$R > 0, "orange", "green"))
  
  # Edge properties
  E(network)$width <- movement_data$movement_rate * 2 + 1
  E(network)$color <- "blue"
  E(network)$label <- sprintf("%.1f", movement_data$movement_rate)
  
  plot(network, 
       main = paste("Network with Movement Rates", title_suffix),
       vertex.label = farm_metadata$farm_id,
       vertex.label.cex = 0.8,
       edge.arrow.size = 0.4,
       edge.curved = 0.2,
       layout = layout_with_fr)
  
  legend("bottomright", 
         legend = c("Infected", "Recovered", "Susceptible", "Movement Rate"),
         col = c("red", "orange", "green", "blue"),
         pch = c(15, 15, 15, NA),
         lty = c(NA, NA, NA, 1),
         lwd = c(NA, NA, NA, 3),
         title = "Legend")
}

# Create network visualizations for each scenario
par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))
for(i in 1:length(scenarios)) {
  data <- intervention_results[[scenarios[i]]]
  create_enhanced_network(data$movement_data, data$farm_metadata, 
                          paste0("(", scenarios[i], ")"))
}
par(mfrow = c(1, 1))

# Parameter uncertainty summary
cat("=== PARAMETER UNCERTAINTY SUMMARY ===\n")
cat("Movement Rates (X_ab):", 
    "Mean:", mean(movement_data$movement_rate),
    "Range:", min(movement_data$movement_rate), "-", max(movement_data$movement_rate), "\n")

cat("Transmission Rates (β):", 
    "Mean:", mean(farm_metadata$beta),
    "Range:", min(farm_metadata$beta), "-", max(farm_metadata$beta), "\n")

cat("Recovery Rates (γ):", 
    "Mean:", mean(farm_metadata$gamma),
    "Range:", min(farm_metadata$gamma), "-", max(farm_metadata$gamma), "\n")

# Export for SIR model
parameter_export <- list(
  movement_rates = movement_data %>% select(from_id, to_id, movement_rate, rate_lower, rate_upper),
  farm_parameters = farm_metadata %>% select(farm_id, contains("beta"), contains("gamma")),
  intervention_comparison = scenario_comparison
)

print("SIR Model Parameters:")
print(parameter_export)
