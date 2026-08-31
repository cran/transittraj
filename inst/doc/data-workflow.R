## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 120,
  fig.width = 6,
  fig.height = 4
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(transittraj)
library(tidytransit)
library(dplyr)
library(sf)
library(ggplot2)

## -----------------------------------------------------------------------------
# Set our filtering parameters
filt_dir <- 0 # 0 is EB, 1 is WB
lineE_id <- "804" # the internal route ID for Line E

# Filter the entire AVL to just the EB
lineE_avl <- lacmta_avl %>%
  filter((route_id == lineE_id) & (direction_id == filt_dir))

## -----------------------------------------------------------------------------
# Pull attributes about our filtered AVL
total_obs <- dim(lineE_avl)[1]
num_trips <- length(unique(lineE_avl$trip_id_performed))
time_span <- round(max(lineE_avl$event_timestamp) - min(lineE_avl$event_timestamp),
                   2)

# Print attributes
cat("Total Observations: ", total_obs,
    "\nNumber of trips: ", num_trips,
    "\nTime span: ", time_span, " hr",
    sep = "")

## -----------------------------------------------------------------------------
# Filter the entire GTFS down to EB Line E
lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs,
                              route_ids = lineE_id,
                              dir_id = filt_dir)

## -----------------------------------------------------------------------------
summary(lineE_gtfs)

## -----------------------------------------------------------------------------
# Set our parameters
lineE_EB_shape_id <- "804EB_RC_221121"
la_CRS <- 32611

# Pull the shape we want
lineE_shape <- get_shape_geometry(gtfs = lineE_gtfs,
                                shape = lineE_EB_shape_id,
                                project_crs = la_CRS)

## -----------------------------------------------------------------------------
print(lineE_shape)

## ----fig.height = 2-----------------------------------------------------------
# Convert GPS points to spatial objects
# You typically won't need to do this -- this is just for visualization
lineE_sf <- lineE_avl %>%
  # As SF
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  # Project to DC
  st_transform(crs = la_CRS)

# Generate a map
avl_map <- ggplot() +
  # Basemap from OSM
  ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0,
                                 progress = "none") +
  # Add alignment & points
  geom_sf(data = lineE_shape, color = "#f43155", linewidth = 1.5) +
  geom_sf(data = lineE_sf, color = "#2f6ff8",
          alpha = 0.2, size = 0.3) +
  # Format our map
  theme_void() +
  labs(title = "Line E Shape & AVL") +
  theme(text = ggplot2::element_text(size = 5))
avl_map

## -----------------------------------------------------------------------------
# Set parameters
buffer = 50 # meters

# Run the cleaning function
lineE_distances <- get_linear_distances(avl_df = lineE_avl,
                                        shape_geometry = lineE_shape,
                                        project_crs = la_CRS,
                                        clip_buffer = buffer)

## -----------------------------------------------------------------------------
# Pull dimensions of each
step0_obs <- dim(lineE_avl)[1]
step1_obs <- dim(lineE_distances)[1]

# Print
cat("Initial: ", step0_obs, " obs",
    "\nAfter buffer: ", step1_obs, " obs",
    "\nDifference: ", (step0_obs - step1_obs), " obs removed")

## -----------------------------------------------------------------------------
head(lineE_distances)

## -----------------------------------------------------------------------------
# Set parameters
lineE_check_op <- FALSE
lineE_remove_singles <- TRUE
lineE_remove_non_overlap <- FALSE

# Run function
lineE_cleaned_subtrips <- clean_overlapping_subtrips(
  distance_df = lineE_distances,
  check_operator = lineE_check_op,
  remove_single_observations = lineE_remove_singles,
  remove_non_overlapping = lineE_remove_non_overlap
)

## -----------------------------------------------------------------------------
# Pull new dimensions
step3_obs <- dim(lineE_cleaned_subtrips)[1]
step1_trips <- length(unique(lineE_distances$trip_id_performed))
step3_trips <- length(unique(lineE_cleaned_subtrips$trip_id_performed))

# Print
cat("Initial: ", step1_obs, " obs, ", step1_trips, " trips",
    "\nAfter: ", step3_obs, " obs, ", step3_trips, " trips",
    "\nDifference: ", (step1_obs - step3_obs), " obs, ",
    (step1_trips - step3_trips), " removed")

## -----------------------------------------------------------------------------
# Get removals from previous function
lineE_step3_removals <- clean_overlapping_subtrips(
  # Same settigns as before
  distance_df = lineE_distances,
  check_operator = lineE_check_op,
  remove_single_observations = lineE_remove_singles,
  remove_non_overlapping = lineE_remove_non_overlap,
  # Return removals
  return_removals = TRUE
)

# Print removed point
print(lineE_step3_removals)

## -----------------------------------------------------------------------------
plot_times <- c(as.POSIXct("2026-05-27 07:35:00",
                           tz = "America/Los_Angeles"),
                as.POSIXct("2026-05-27 08:02:00",
                           tz = "America/Los_Angeles"))
plot_df <- lineE_distances %>%
  filter((trip_id_performed == "63384142") &
           (event_timestamp >= plot_times[1]) &
           (event_timestamp <= plot_times[2]))

subtrips_plot <- ggplot() +
  geom_line(data = plot_df,
            aes(x = event_timestamp, y = distance, color = vehicle_id),
            linewidth = 2, alpha = 0.6) +
  geom_point(data = plot_df,
             aes(x = event_timestamp, y = distance, color = vehicle_id),
             size = 2) +
  scale_color_manual(name = "Vehicle ID",
                     values = c("1065-1075-1093" = "#2f6ff8",
                                "452" = "#f43155")) +
  theme_minimal() +
  labs(x = "Time",
       y = "Distance (m)",
       title = "Overlapping Sub-Trips on Line E")
subtrips_plot

## -----------------------------------------------------------------------------
# Set parameters
lineE_max_jump <- 80 # meters
lineE_min_jump <- -1 * lineE_max_jump # meters

# Run function
lineE_no_jumps <- clean_jumps(distance_df = lineE_cleaned_subtrips,
                              max_median_deviation = lineE_max_jump,
                              min_median_deviation = lineE_min_jump,
                              t_cutoff = Inf)

## -----------------------------------------------------------------------------
# Pull dimensions
step4_obs <- dim(lineE_no_jumps)[1]

# Print
cat("Initial: ", step3_obs, " obs",
    "\nAfter: ", step4_obs, " obs",
    "\nDifference: ", (step3_obs - step4_obs), " obs removed")

## -----------------------------------------------------------------------------
# Get removals
lineE_step4_removals <- clean_jumps(
  # Same settings as before
  distance_df = lineE_cleaned_subtrips,
  max_median_deviation = lineE_max_jump,
                              min_median_deviation = lineE_min_jump,
                              t_cutoff = Inf,
  # Return removals
  return_removals = TRUE)

# Print the removed points
head(lineE_step4_removals)

## -----------------------------------------------------------------------------
# Filter dataframe to our tirp & distances
plot_df <- lineE_cleaned_subtrips %>%
  filter(trip_id_performed == "63383991") %>%
  filter((distance >= 6400) & (distance <= 7600)) %>%
  # Join removals
  left_join(y = (lineE_step4_removals %>% select(location_ping_id, all_ok)),
            by = "location_ping_id") %>%
  mutate(all_ok = tidyr::replace_na(all_ok, TRUE))

# Create a plot
jumps_plot <- ggplot() +
  # Plot the points
  geom_point(data = plot_df,
             aes(x = event_timestamp, y = distance,
                 color = all_ok, shape = all_ok),
             size = 3, stroke = 3) +
  # Format the points
  scale_color_manual(name = "Point OK?",
                     values = c("FALSE" = "#f43155",
                                "TRUE" = "#2f6ff8")) +
  scale_shape_manual(name = "Point OK?",
                     values = c("FALSE" = 4,
                                "TRUE" = 16)) +
  # Format the plot
  theme_minimal() +
  labs(x = "Time",
       y = "Distance (m)",
       title = "Outliers on Line E",
       subtitle = "Trip 63383991")
jumps_plot

## -----------------------------------------------------------------------------
# Set parameters
lineE_trim_type <- "both"

# Run function
lineE_trimmed <- trim_trips(distance_df = lineE_no_jumps,
                            trim_type = lineE_trim_type)

## -----------------------------------------------------------------------------
# Pull dimensions
step5_obs <- dim(lineE_trimmed)[1]

# Print
cat("Initial: ", step4_obs, " obs",
    "\nAfter: ", step5_obs, " obs",
    "\nDifference: ", (step4_obs - step5_obs), " obs removed")

## -----------------------------------------------------------------------------
lineE_step5_removals <- trim_trips(
  # Same settings as before
  distance_df = lineE_no_jumps,
  trim_type = lineE_trim_type,
  # Return removals
  return_removals = TRUE
)

head(lineE_step5_removals)

## -----------------------------------------------------------------------------
# Filter dataframe to our tirp & distances
plot_df <- lineE_no_jumps %>%
  filter(trip_id_performed == "63383991") %>%
  filter(distance <= 5000) %>%
  # Join removals
  left_join(y = (lineE_step5_removals %>% select(location_ping_id, obs_ok)),
            by = "location_ping_id") %>%
  mutate(obs_ok = tidyr::replace_na(obs_ok, TRUE))

# Create a plot
trimmed_plot <- ggplot() +
  # Plot the points
  geom_line(data = plot_df,
            aes(x = event_timestamp, y = distance,
                color = obs_ok),
            linewidth = 2) +
  # Format the points
  scale_color_manual(name = "Point OK?",
                     values = c("FALSE" = "#f43155",
                                "TRUE" = "#2f6ff8")) +
  # Format the plot
  theme_minimal() +
  labs(x = "Time",
       y = "Distance (m)",
       title = "Trimmed Trips on Line E",
       subtitle = "Trip 63383991")
trimmed_plot

## -----------------------------------------------------------------------------
# Set parameters
lineE_min_dist <- 1000 # meters
lineE_min_time <- 120 # seconds
lineE_max_gap <- 1000 # meters

# Run function
lineE_cleaned_incompletes <- clean_incomplete_trips(
  distance_df = lineE_trimmed,
  min_trip_distance = lineE_min_dist,
  min_trip_duration = lineE_min_time,
  max_distance_gap = lineE_max_gap
)

## -----------------------------------------------------------------------------
step6_obs <- dim(lineE_cleaned_incompletes)[1]
step6_trips <- length(unique(lineE_cleaned_incompletes$trip_id_performed))
step5_trips <- length(unique(lineE_trimmed$trip_id_performed))

cat("Initial: ", step5_obs, " obs, ", step5_trips, " trips",
    "\nAfter: ", step6_obs, " obs, ", step6_trips, " trips",
    "\nDifference: ", (step5_obs - step6_obs), " obs, ",
    (step5_trips - step6_trips), " trips removed",
    sep = "")

## -----------------------------------------------------------------------------
lineE_step6_removals <- clean_incomplete_trips(
  # Same settings as before
  distance_df = lineE_trimmed,
  min_trip_distance = lineE_min_dist, min_trip_duration = lineE_min_time,
  max_distance_gap = lineE_max_gap,
  # Return removals
  return_removals = TRUE
)
print(lineE_step6_removals)

## -----------------------------------------------------------------------------
# Filter dataframe to our tirp & distances
plot_df <- lineE_trimmed %>%
  filter(trip_id_performed == "63383948") %>%
  filter((distance >= 4000) & (distance <= 16000))

# Create a plot
gaps_plot <- ggplot() +
  # Plot the points
  geom_line(data = plot_df,
            aes(x = event_timestamp, y = distance),
            linewidth = 2, color = "lightcoral") +
  geom_point(data = plot_df,
             aes(x = event_timestamp, y = distance),
             size = 2, color = "firebrick4") +
  # Format the plot
  theme_minimal() +
  labs(x = "Time",
       y = "Distance (m)",
       title = "Gap on Line E",
       subtitle = "Trip 63383948")
gaps_plot

## -----------------------------------------------------------------------------
# Set parameters
lineE_dist_error <- 0.001
lineE_correct_speeds <- TRUE

# Run function
lineE_mono <- make_monotonic(distance_df = lineE_cleaned_incompletes,
                             correct_speed = lineE_correct_speeds,
                             add_distance_error = lineE_dist_error)

## -----------------------------------------------------------------------------
# Pull dimensions
step7_obs <- dim(lineE_mono)[1]

# Print
cat("Initial: ", step6_obs, " obs",
    "\nAfter: ", step7_obs, " obs",
    "\nDifference: ", (step6_obs - step7_obs), " obs removed")

## -----------------------------------------------------------------------------
# Trimmed DF
step6_val <- validate_monotonicity(distance_df = lineE_trimmed,
                                   check_speed = TRUE)
print(step6_val)

# Monotonic-corrected DF
step7_val <- validate_monotonicity(distance_df = lineE_mono,
                                   check_speed = TRUE)
print(step7_val)

## ----echo = FALSE, eval = FALSE-----------------------------------------------
# # Get monotonic changes
# lineE_mono_changes <- make_monotonic(distance_df = lineE_trimmed,
#                            correct_speed = TRUE,
#                            add_distance_error = lineE_dist_error,
#                            return_changes = TRUE)
# 
# # Print head
# head(lineE_mono_changes)

## -----------------------------------------------------------------------------
# Set filter parameters
plot_trip <- "63383915"
plot_dists <- c(18300, 19300)
# Get old DF
plot_df_before <- lineE_trimmed %>%
  filter(trip_id_performed == plot_trip) %>%
  filter((distance >= plot_dists[1]) & (distance <= plot_dists[2])) %>%
  mutate(speed_label = paste(round(speed, 1), " m/s", sep = ""))
# Get corrected DF
plot_df_after <- lineE_mono %>%
  filter(trip_id_performed == plot_trip) %>%
  filter((distance >= plot_dists[1]) & (distance <= plot_dists[2])) %>%
  mutate(speed_label = paste(round(speed, 1), " m/s", sep = ""))

# Plot
mono_plot <- ggplot() +
  geom_point(data = plot_df_before,
             aes(x = event_timestamp, y = distance,
                 color = "Uncorrected"),
             size = 4, alpha = 0.6) +
  geom_point(data = plot_df_after,
             aes(x = event_timestamp, y = distance,
                 color = "Corrected"),
             size = 3, alpha = 1) +
  geom_label(data = plot_df_before,
             aes(x = event_timestamp, y = distance,
                 color = "Uncorrected", label = speed_label),
            nudge_y = -50, size = 2.5, show.legend = FALSE) +
  geom_label(data = plot_df_after,
             aes(x = event_timestamp, y = distance,
                 color = "Corrected", label = speed_label),
            nudge_y = 50, size = 2.5, show.legend = FALSE) +
  scale_color_manual(name = "Correction",
                     values=  c("Uncorrected" = "#f43155",
                                "Corrected" = "#2f6ff8")) +
  # Format the plot
  theme_minimal() +
  labs(x = "Time",
       y = "Distance (m)",
       title = "Monotonic Correction on Line E",
       subtitle = paste("Trip ", plot_trip, sep = ""))
mono_plot

