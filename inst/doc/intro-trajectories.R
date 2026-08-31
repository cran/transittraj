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
library(tidyr)
library(sf)
library(ggplot2)

## ----echo = FALSE-------------------------------------------------------------
# --- Setup ---
filt_dir <- 0 # 0 is EB, 1 is WB
lineE_id <- "804" # the internal route ID for Line E

lineE_avl <- lacmta_avl %>%
  filter((route_id == lineE_id) & (direction_id == filt_dir))

lineE_gtfs <- filter_by_route(gtfs = lacmta_gtfs,
                              route_ids = lineE_id,
                              dir_id = filt_dir)
lineE_EB_shape_id <- "804EB_RC_221121"
la_CRS <- 32611
lineE_shape <- get_shape_geometry(gtfs = lineE_gtfs,
                                shape = lineE_EB_shape_id,
                                project_crs = la_CRS)

# --- Workflow ---
# 1 & 2
buffer = 50 # meters
lineE_distances <- get_linear_distances(avl_df = lineE_avl,
                                        shape_geometry = lineE_shape,
                                        project_crs = la_CRS,
                                        clip_buffer = buffer)


# 3
lineE_check_op <- FALSE
lineE_remove_singles <- TRUE
lineE_remove_non_overlap <- FALSE
lineE_cleaned_subtrips <- clean_overlapping_subtrips(
  distance_df = lineE_distances,
  check_operator = lineE_check_op,
  remove_single_observations = lineE_remove_singles,
  remove_non_overlapping = lineE_remove_non_overlap
)

# 4
lineE_max_jump <- 80 # meters
lineE_min_jump <- -1 * lineE_max_jump # meters
lineE_no_jumps <- clean_jumps(distance_df = lineE_cleaned_subtrips,
                              max_median_deviation = lineE_max_jump,
                              min_median_deviation = lineE_min_jump,
                              t_cutoff = Inf)

# 5
lineE_min_dist <- 1000 # meters
lineE_min_time <- 120 # seconds
lineE_max_gap <- 1000 # meters
lineE_cleaned_incompletes <- clean_incomplete_trips(
  distance_df = lineE_no_jumps,
  min_trip_distance = lineE_min_dist,
  min_trip_duration = lineE_min_time,
  max_distance_gap = lineE_max_gap
)

# 6
lineE_trim_type <- "both"
lineE_trimmed <- trim_trips(distance_df = lineE_cleaned_incompletes,
                          trim_type = lineE_trim_type)

# 7
lineE_dist_error <- 0.001
lineE_correct_speeds <- TRUE
lineE_mono <- make_monotonic(distance_df = lineE_trimmed,
                           correct_speed = lineE_correct_speeds,
                           add_distance_error = lineE_dist_error)

## -----------------------------------------------------------------------------
# Run function
lineE_traj <- get_trajectory_fun(distance_df = lineE_mono,
                                interp_method = "monoH.FC",
                                use_speeds = TRUE,
                                find_inverse_fun = TRUE)

## -----------------------------------------------------------------------------
summary(lineE_traj)

## -----------------------------------------------------------------------------
# Run interpolating function
lineE_time_interp <- predict(
  object = lineE_traj,
  new_times = c(1779887000, 1779887500)
)

# Print full results
print(lineE_time_interp)

## -----------------------------------------------------------------------------
# Run interpolating function
lineE_speed_interp <- predict(
  object = lineE_traj,
  new_times = c(1779887000, 1779887500),
  deriv = 1
)

# Print results
print(lineE_speed_interp)

## -----------------------------------------------------------------------------
# Run interpolating function
lineE_vec_interp <- predict(
  object = lineE_traj,
  new_times = c(1779887000, 1779887500),
  deriv = c(0, 1)
)

# Print results
print(lineE_vec_interp)

## -----------------------------------------------------------------------------
# First, find stop IDs served by Line A
lineA_stop_ids <- filter_by_route(gtfs = lacmta_gtfs,
                                  route_ids = "801")$stops %>%
  pull(stop_id)

# Next, find stop distances and join the timepoints column
lineE_stops <- get_stop_distances(gtfs = lineE_gtfs,
                                 shape_geometry = lineE_shape,
                                 project_crs = la_CRS) %>%
  # Find whether they are shared with Line A
  mutate(Shared = (stop_id %in% lineA_stop_ids),
         Shared = if_else(condition = Shared,
                          true = "Yes",
                          false = "No")) %>%
  # Polish up the result
  select(stop_id, stop_name, Shared, distance) %>%
  arrange(distance)

# Print header
head(lineE_stops)

## -----------------------------------------------------------------------------
# Run interpolating function
lineE_stop_crossings <- predict(
  object = lineE_traj,
  new_distances = lineE_stops
)

# Print header
head(lineE_stop_crossings)

## -----------------------------------------------------------------------------
# Get distance limits of U St between 13th and 14th
downtown_stops <- lineE_stops %>%
  filter(Shared == "Yes") %>%
  pull(distance)
downtown_lims <- c(min(downtown_stops),
                   max(downtown_stops))

print(downtown_lims)

## -----------------------------------------------------------------------------
# Run interpolating function
lineE_downtown_interp <- predict(
  object = lineE_traj,
  distance_lims = downtown_lims,
  timestep = 1,
  deriv = c(0, 1)
)

# Print header
head(lineE_downtown_interp)

## -----------------------------------------------------------------------------
# Pivot, for seprate columns for dist & speed
lineE_downtown_pivot <- lineE_downtown_interp %>%
  # Order by time, then filter to the first three complete
  arrange(event_timestamp) %>%
  filter(trip_id_performed %in% unique(trip_id_performed)[2:4]) %>%
  # Pivot to make distance & speed separate columns
  pivot_wider(id_cols = c("trip_id_performed", "event_timestamp"),
              names_from = "deriv", names_glue = "interp_{.name}",
              values_from = "interp") %>%
  # Convert to timezone
  mutate(event_timestamp = as.POSIXct(event_timestamp,
                                      tz = "America/Los_Angeles"))

head(lineE_downtown_pivot)

## -----------------------------------------------------------------------------
# Create plot
downtown_plot <- ggplot(data = lineE_downtown_pivot) +
  # Add points
  geom_line(aes(group = trip_id_performed,
                x = event_timestamp,
                y = interp_0, # y from interp at deriv 0, i.e. distnace
                color = interp_1), # color from interp at deriv 1, i.e. speed
             linewidth = 3, alpha = 1) +
  # Color points by trip
  scale_color_viridis_c(name = "Speed\n(m/s)") +
  # Theming
  theme_minimal() +
  labs(x = "Time (s)",
       y = "Distance (m)",
       title = "Line E Second-by-Second Speed Profiles",
       subtitle = "Downtown LA")
downtown_plot

## -----------------------------------------------------------------------------
plot(lineE_traj)

## -----------------------------------------------------------------------------
# Set formatting options for Line E stops
stop_formatting <- data.frame(Shared = c("Yes", "No"),
                              color = c("firebrick", "grey50"),
                              linetype = c("longdash", "dashed"))

## -----------------------------------------------------------------------------
# Run plotting function
traj_plot <- plot_trajectory(
  # Provide input data
  trajectory = lineE_traj,
  feature_distances = lineE_stops,
  # Format features
  feature_color = stop_formatting,
  feature_type = stop_formatting,
  feature_width = 0.5, feature_alpha = 0.5,
  # Format trajectories
  traj_color = "#2f6ff8",
  traj_width = 0.4, traj_alpha = 1
)
traj_plot

## -----------------------------------------------------------------------------
# Set parameters
flower_st_lims <- c(21900, 22800)

# Run function
flower_st_plot <- plot_trajectory(
  # Provide input data
  trajectory = lineE_traj,
  feature_distances = lineE_stops,
  center_trajectories = TRUE,
  distance_lim = flower_st_lims,
  timestep = 1,
  # Format fetures
  feature_color = stop_formatting,
  feature_type = stop_formatting,
  feature_width = 1, feature_alpha = 0.8,
  # Format trajectories
  traj_width = 0.8, traj_alpha = 0.5, traj_color = "#2f6ff8",
  # Add labels
  label_field = "stop_name", label_pos = "right",
  label_alpha = 0.8
)
flower_st_plot

## -----------------------------------------------------------------------------
# Set parameters
stop_formatting <- data.frame(Shared = c("Yes", "No"),
                              outline = c("firebrick4", "grey30"),
                              shape = c(22, 21))

## ----eval = FALSE-------------------------------------------------------------
# # Set distance limits
# downtown_lims <- c(20500, 29000)
# 
# # Run function
# line_anim <- plot_animated_line(
#   # Add input data
#   trajectory = lineE_traj,
#   feature_distances = lineE_stops,
#   distance_lim = downtown_lims,
#   timestep = 1,
#   # Format vehicles
#   veh_outline = "#2f6ff8", veh_stroke = 2,
#   # Format features
#   feature_outline = stop_formatting,
#   feature_shape = stop_formatting,
#   feature_size = 4, feature_stroke = 1.5,
#   # Add labels
#   label_field = "stop_name",
#   label_pos = "right", label_size = 3,
#   # Format route & vehicles
#   route_color = "#f43155",
#   veh_alpha = 0.9, veh_size = 4
# )
# line_anim

## ----eval = FALSE, echo = FALSE-----------------------------------------------
# gganimate::animate(line_anim,
#                    duration = 60, fps = 30,
#                    height = 1080, width = 400, units = "px",
#                    renderer = gganimate::av_renderer())

## ----eval = FALSE-------------------------------------------------------------
# # Run function
# map_anim <- plot_animated_map(
#   # Add trajectory, shape, & feature data
#   trajectory = lineE_traj,
#   shape_geometry = lineE_shape,
#   feature_distances = lineE_stops,
#   # Format features
#   feature_outline = stop_formatting,
#   feature_shape = stop_formatting,
#   feature_size = 4, feature_stroke = 3,
#   # Format route
#   route_color = "#f43155", route_width = 4,
#   bbox_expand = 1000,
#   # Format vehicles
#   veh_size = 6, veh_stroke = 3,
#   veh_outline = "#2f6ff8", veh_alpha = 0.9
# )
# map_anim

## ----echo = FALSE, eval = FALSE-----------------------------------------------
# gganimate::animate(map_anim + theme(text = element_text(size = 16)),
#                    duration = 60, fps = 30,
#                    width = 2160, units = "px",
#                    renderer = gganimate::av_renderer())

