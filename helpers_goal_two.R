library(mclust)

deg2rad = function(x) {
  x * pi/180
}

rad2deg = function(x) {
  x * 180/pi
}


get_average_spin_info <- function(data, pitcher, pitch_type, date_range = NULL) {

  filtered_data <- data %>%
    filter(Pitcher == pitcher, PitchType == pitch_type)
  
  # Filter by date range if provided
  if (!is.null(date_range)) {
    filtered_data <- filtered_data %>%
      filter(as.Date(Date) >= as.Date(date_range[1]) & as.Date(Date) <= as.Date(date_range[2]))
  }
  
  # Compute averages for the specified columns
  avg_values <- filtered_data %>%
    summarise(
      avg_spin_backspin    = mean(spin_backspin, na.rm = TRUE),
      avg_spin_sidespin    = mean(spin_sidespin, na.rm = TRUE),
      avg_spin_gyrospin    = mean(spin_gyrospin, na.rm = TRUE),
      avg_start_speed      = mean(start_speed, na.rm = TRUE),
      avg_SpinRate         = mean(SpinRate, na.rm = TRUE),
      avg_break_z          = mean(break_z, na.rm = TRUE),
      avg_break_x          = mean(break_x, na.rm = TRUE),
      avg_SpinAxis_inf     = mean(SpinAxis_inf, na.rm = TRUE),
      avg_spin_efficiency  = mean(spin_efficiency, na.rm = TRUE)
    )
  
  
  dominant_seam <- get_dominant_seam_orientation(filtered_data)
  
  # Combine averages and dominant seam orientation into a single dictionary
  result <- as.list(avg_values)
  result$seam_orientation_lat <- dominant_seam$seam_orientation_lat
  result$seam_orientation_lon <- dominant_seam$seam_orientation_lon
  
  return(result)
}


get_dominant_seam_orientation <- function(filtered_data) {

  # Convert lat/lon to 3D coordinates
  cluster_data <- filtered_data %>%
    mutate(
      clust_x = cos(deg2rad(seam_orientation_lat)) * sin(deg2rad(seam_orientation_lon)),
      clust_y = sin(deg2rad(seam_orientation_lat)),
      clust_z = cos(deg2rad(seam_orientation_lat)) * cos(deg2rad(seam_orientation_lon))
    )
  
  # Filter and check minimum data requirement
  this_cluster_data <- cluster_data %>% 
    select(PitchUID, PitchType, contains('clust_')) %>% 
    filter(!is.na(clust_x))
  
  if (nrow(this_cluster_data) < 30) {
    warning(paste("Not enough data points for clustering. Found", nrow(this_cluster_data), "pitches, need at least 30."))
    return(NULL)
  }
  
  # Perform clustering
  cluster_mod <- this_cluster_data %>% 
    select(contains('clust_')) %>%
    mclust::Mclust(., 5, verbose = FALSE)
  
  # Tag data with cluster assignments
  this_tagged_data <- this_cluster_data %>% 
    mutate(cluster = as.factor(predict(cluster_mod,
                                       newdata = this_cluster_data %>%
                                         select(contains('clust_')))$classification))
  
  # Calculate cluster centers
  these_cluster_centers <- this_tagged_data %>% 
    group_by(cluster) %>% 
    summarize(clust_x = mean(clust_x, na.rm = T),
              clust_y = mean(clust_y, na.rm = T),
              clust_z = mean(clust_z, na.rm = T),
              num_pitches = n()) %>% 
    ungroup() %>% 
    mutate(freq = num_pitches/sum(num_pitches),
           seam_orientation_lat = rad2deg(asin(clust_y)),
           seam_orientation_lon = rad2deg(atan2(clust_x, clust_z)),
           radius = sqrt(clust_x^2 + clust_y^2 + clust_z^2))
  
  # Filter to surface clusters
  surface_clusters <- these_cluster_centers %>% 
    filter(radius >= .98, freq >= .05)
  
  if (nrow(surface_clusters) == 0) {
    warning("No valid surface clusters found.")
    return(NULL)
  }
  
  # Get the most frequent (dominant) cluster
  dominant_cluster <- surface_clusters %>%
    arrange(desc(num_pitches)) %>%
    slice(1)
  
  # Return the seam orientation from dominant cluster
  return(list(
    seam_orientation_lat = dominant_cluster$seam_orientation_lat,
    seam_orientation_lon = dominant_cluster$seam_orientation_lon,
    num_pitches = dominant_cluster$num_pitches,
    frequency = dominant_cluster$freq
  ))
}