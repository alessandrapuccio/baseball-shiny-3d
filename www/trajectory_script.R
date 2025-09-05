# # ... HELPER FN --------------
# rad2deg = function(rad) {
#   deg = 180*rad/pi
#   return(deg)
# }
# 
# calculate_spin <- function(total_spin, gyro_degree, tilt_degree) {
# 
#   gyro_radian <- gyro_degree * pi / 180
#   tilt_radian <- 3 * pi/2 - tilt_degree * pi / 180
# 
#   transverse_spin <- total_spin * cos(gyro_radian)
# 
#   gyro_spin <- total_spin * sin(gyro_radian)
# 
#   backspin <- transverse_spin * sin(tilt_radian)
# 
#   sidespin <- transverse_spin * -cos(tilt_radian)
# 
#   return(list(
#     Backspin = backspin,
#     Gyro_Spin = gyro_spin,
#     Sidespin = sidespin
#   ))
# }
# 
# 
# estimate_phys_break = function(traj_data, verbose = F) {
# 
#   t1 = Sys.time()
# 
#   # FUNCTIONS --------------------
# 
#   #function for drag coefficient approximated
#   #from Adair's Physics of Baseball
#   Cd = function(s) {
#     #Convert speed to match Adair's convention
#     s = mps2mph(s)
#     #Piecewise-linear approximation:
#     if (s < 40) {
#       Cd = .5
#     } else if (s < 105) {
#       Cd = -.1*(s-65)/40 + .4
#     } else {
#       Cd = .3
#     }
#   }
# 
#   #function for lift coefficient
#   #approximated from Alan Nathan paper.
#   Cl = function(S) {
#     #S is the 'spinniness' parameter, defined as R*omega/v
#     if (S < .1) {
#       Cl = 1.5*S
#     } else if (S < 1) {
#       Cl = .6*S + .1
#     } else {
#       Cl = 0.7
#     }
#   }
# 
#   #Angle conversion function
#   deg2rad = function(deg) {
#     rad = pi*deg/180
#     return(rad)
#   }
# 
#   #Distance conversion function
#   ft2m = function(feet) {
#     m = .3048*feet
#     return(m)
#   }
# 
#   #Distance conversion function
#   m2ft = function(meters) {
#     feet = meters/.3048
#     return(feet)
#   }
# 
#   #Speed conversion function
#   mph2mps = function(mph) {
#     mps = .44704*mph
#     return(mps)
#   }
# 
#   #Speed conversion function
#   mps2mph = function(mps) {
#     mph = mps/.44704
#     return(mph)
#   }
# 
#   #Angle conversion function
#   rad2deg = function(rad) {
#     deg = 180*rad/pi
#     return(deg)
#   }
# 
#   #Spin rate conversion function
#   radps2RPM = function(radps) {
#     RPM = radps*9.5493
#     return(RPM)
#   }
# 
#   #Spin rate conversion function
#   RPM2radps = function(RPM) {
#     radps = RPM/9.5493
#     return(radps)
#   }
# 
#   # ASSUMPTIONS -------------------
# 
#   R = 0.07468/2
#   A = pi*R^2
#   g = 9.81
#   m = .145
#   rho = 1.2041
# 
#   initial_conditions = data.frame(
#     time = 0,
#     #assume release right above the mound, extension in front of it
#     pos_x = 0,
#     pos_y = ft2m(traj_data$Extension[1]),
#     pos_z = 2,
#     #assume release is straight-on cannon (we only care about diff from gravity-only arc)
#     v_x = 0,
#     v_y = mph2mps(traj_data$ReleaseSpeed[1]),
#     v_z = 0,
#     #since ball starts in perfect cannon shot, spin x/y/z matches back/side/gyro in frame 1
#     wx = RPM2radps(traj_data$spin_backspin[1]),
#     wy = RPM2radps(traj_data$spin_gyrospin[1]),
#     wz = RPM2radps(traj_data$spin_sidespin[1]))
# 
#   # TRAJECTORY SIMULATION --------------------
# 
#   dt = .01
# 
#   traj_list = list()
#   i = 0
#   still_travelling = T
# 
#   while (still_travelling) {
#     i = i + 1
# 
#     if (i == 1) {
#       curr_timestep = initial_conditions
#     } else {
#       curr_timestep = next_timestep
#     }
# 
#     acc = curr_timestep %>%
#       mutate(speed = sqrt(v_x^2 + v_y^2 + v_z^2),
#              spin = sqrt(wx^2 + wy^2 + wz^2),
#              S = abs(R*spin/speed),
#              #deduce angle of flight
#              phi = asin(v_z/speed),
#              theta = asin(v_x/(cos(phi)*speed))) %>%
#       mutate(vx_hat = v_x/speed,
#              vy_hat = v_y/speed,
#              vz_hat = v_z/speed,
#              wx_hat = wx/spin,
#              wy_hat = wy/spin,
#              wz_hat = wz/spin) %>%
#       mutate(
#         #compute instantaneous accelerations
#         A_drag = .5*rho*speed^2*Cd(mph2mps(speed))*A/m,
#         A_grav = g,
#         A_magn = .5*rho*speed^2*Cl(S)*A/m,
#         #decompose accelerations
#         a_x = -A_drag*vx_hat + ifelse(spin == 0, 0, A_magn*(wy_hat*vz_hat - wz_hat*vy_hat)),
#         a_y = -A_drag*vy_hat + ifelse(spin == 0, 0, A_magn*(wz_hat*vx_hat - wx_hat*vz_hat)),
#         a_z = -A_drag*vz_hat + ifelse(spin == 0, 0, A_magn*(wx_hat*vy_hat - wy_hat*vx_hat)) - A_grav)
# 
#     traj_list[[i]] = acc
# 
#     next_timestep = data.frame(
#       time = acc$time + dt,
#       #assume release right above the mound, extension in front of it
#       pos_x = acc$pos_x + dt*acc$v_x + .5*dt^2*acc$a_x,
#       pos_y = acc$pos_y + dt*acc$v_y + .5*dt^2*acc$a_y,
#       pos_z = acc$pos_z + dt*acc$v_z + .5*dt^2*acc$a_z,
#       #assume release is straight-on cannon (we only care about diff from gravity-only arc)
#       v_x = acc$v_x + dt*acc$a_x,
#       v_y = acc$v_y + dt*acc$a_y,
#       v_z = acc$v_z + dt*acc$a_z,
#       wx = acc$wx,
#       wy = acc$wy,
#       wz = acc$wz
#     )
# 
#     if (next_timestep$pos_y >= ft2m(60.5)) {
#       still_travelling = F
#     }
#   }
# 
#   # TRAJECTORY RESULTS ---------------
# 
#   traj_coords_raw = do.call(rbind, traj_list)
# 
#   # traj_coords = traj_coords_raw %>%
#   #   mutate_at(vars(speed, v_x, v_y, v_z), list(~mps2mph(.))) %>%
#   #   mutate_at(vars(pos_x, pos_y, pos_z), list(~m2ft(.))) %>%
#   #   mutate_at(vars(spin, wx, wy, wz), list(~radps2RPM(.))) %>%
#   #   filter(pos_y <= 60.5)
# 
#   mvmt = traj_coords_raw %>%
#     filter(m2ft(pos_y) <= 60.5) %>%
#     arrange(dt) %>%
#     transmute(phys_VB = last(pos_z) - first(pos_z),
#               phys_grav_VB = first(v_z)*last(time) - .5*g*last(time)^2,
#               phys_HB = last(pos_x) - first(pos_x),
#               phys_IVB = phys_VB - phys_grav_VB) %>%
#     distinct(., .keep_all = T) %>%
#     # select(contains('phys')) %>%
#     mutate_all(list(~m2ft(.)*12)) %>%
#     # mutate(PitchUID = traj_data$PitchUID) %>%
#     mutate_if(is.numeric, list(~round(., 3)))
# 
#   t2 = Sys.time()
# 
#   if (verbose) {
#     cat('----------------')
#     cat('\n')
#     print(traj_data %>%
#             select(-PitchUID) %>%
#             mutate_if(is.numeric, list(~round(., 3))))
#     cat('\n')
#     print(mvmt %>% select(-PitchUID))
#     cat('\n')
#     print(paste0(round(difftime(t2, t1, units = 'secs'), 3), ' seconds'))
#     cat('----------------')
#     cat('\n')
#   }
# 
#   return(mvmt)
# }
# 
# predict_opt_seams = function(pitches,type='ssw_break'){
# 
#   api_url = paste0('https://testendpoint1.westus2.inference.ml.azure.com/score')
#   api_key = ''
# 
#   authz_hdr = paste('Bearer', api_key, sep=' ')
# 
#   my_body = toJSON(list(df = pitches,
#                         type = type))
# 
#   upload = POST(api_url,
#                 body = my_body,
#                 timeout(900),
#                 httr::add_headers(c('Content-Type' = "application/json",
#                                     'Authorization' = authz_hdr,
#                                     'azureml-model-deployment' = "deploytm1")))
# 
#   pitches = content(upload,as='text')
#   pitches = fromJSON(pitches)
#   pitches = as.data.frame(pitches) %>% unnest()
# 
# 
#   # pitches %>%
#   #   select(break_x, break_z, x_break_x, x_break_z) %>%
#   #   head()
# 
# 
#   return(pitches)
# 
# }
# 
# pull_focus_cols <- function(){
#   focus_cols <-  c(
#     'PitchUID',
#     'pitcher',
#     'game_id',
#     'pitcher_id',
#     'seam_orientation_lat',
#     'seam_orientation_lon',
#     'spin_backspin',
#     'spin_sidespin',
#     'spin_gyrospin',
#     'spin_efficiency',
#     'pitch_type',
#     'pitcher_throws',
#     'bat_side',
#     'phys_break_x',
#     'phys_break_z',
#     'break_z',
#     'break_x',
#     'start_speed',
#     'release_x',
#     'release_z',
#     'release_ext',
#     'vert_release_angle',
#     'horz_release_angle',
#     'spin_rate',
#     'pitch_group',
#     'date',
#     'year',
#     'platoon_state',
#     'game_avg_fb_break_x',
#     'avg_fb_velo',
#     'game_avg_fb_break_z',
#     'game_avg_fb_release_x',
#     'game_avg_fb_release_z',
#     'year_avg_fb_break_x',
#     'year_avg_fb_break_z',
#     'year_avg_fb_release_x',
#     'year_avg_fb_release_z',
#     'avg_fb_break_x',
#     'avg_fb_break_z',
#     'avg_fb_release_x',
#     'avg_fb_release_z',
#     'pitch_type_code',
#     'pitcher_l'
#   )
#   return(focus_cols)
# }
# 
# pull_sfx_year_data <- function(player_id = NULL,
#                                year_input = NULL){
# 
#   sfx_data <- pull_pitch_data_dw01(
#     player_type = 'pitcher',
#     conn = marinersbb::getDb(type = 'DBI'),
#     players = player_id,
#     years = year_input
#   )
# 
#   pitches_df = sfx_data %>%
#     rename("pitcher" = Pitcher, "spin_rate" = SpinRate, 'pitch_side' = pitcher_side, 'bat_side' = batter_side)
# 
#   pitches <- pitches_df %>%
#     mutate(pitch_type_code = as.integer(factor(
#       PitchType,
#       levels = c("CH", "CU", "FA", "FC", "FS", "SI", "SL", "SW"),
#       ordered = T
#     )),
#     pitcher_l = as.integer(pitch_side == "Left")) %>%
#     rename("pitch_type" = PitchType)
# 
#   ps <- data.frame(
#     platoon_state = c("lvl", "rvr", "lvr", "rvl"),
#     bat_side = c("Left", "Right", "Left", "Right"),
#     pitch_side = c("Left", "Right", "Right", "Left")
#   )
# 
#   pitch_shapes = pitches %>%
#     inner_join(ps, by = join_by("bat_side", "pitch_side")) %>%
#     mutate(pitch_group = case_when(
#       pitch_type %in% c("FA", "FC", "SI") ~ "fb",
#       pitch_type %in% c("CU", "SL", "SW") ~ "bb",
#       pitch_type %in% c("FS", "CH") ~ "ch",
#       T ~ NA_character_
#     )) %>%
#     group_by(pitcher_id, game_id) %>%
#     mutate(
#       game_avg_fb_break_x = mean(break_x[pitch_group == 'fb'], na.rm = T),
#       game_avg_fb_break_z = mean(break_z[pitch_group == 'fb'], na.rm =
#                                    T),
#       game_avg_fb_release_x = mean(release_x[pitch_group == 'fb'], na.rm =
#                                      T),
#       game_avg_fb_release_z = mean(release_z[pitch_group == 'fb'], na.rm =
#                                      T)
#     ) %>%
#     ungroup() %>%
#     group_by(pitcher_id, year) %>%
#     mutate(
#       year_avg_fb_break_x = mean(break_x[pitch_group == 'fb'], na.rm = T),
#       year_avg_fb_break_z = mean(break_z[pitch_group == 'fb'], na.rm =
#                                    T),
#       year_avg_fb_release_x = mean(release_x[pitch_group == 'fb'], na.rm =
#                                      T),
#       year_avg_fb_release_z = mean(release_z[pitch_group == 'fb'], na.rm =
#                                      T)
#     ) %>%
#     ungroup() %>%
#     arrange(desc(date)) %>%
#     group_by(pitcher_id) %>%
#     dplyr::slice(1:900) %>%
#     ungroup() %>%
#     mutate(
#       avg_fb_break_x = coalesce(game_avg_fb_break_x,
#                                 year_avg_fb_break_x),
#       avg_fb_break_z = coalesce(game_avg_fb_break_z,
#                                 year_avg_fb_break_z),
#       avg_fb_release_x = coalesce(game_avg_fb_release_x,
#                                   year_avg_fb_release_x),
#       avg_fb_release_z = coalesce(game_avg_fb_release_z,
#                                   year_avg_fb_release_z),
#       pitch_type_code = as.integer(factor(
#         pitch_type,
#         levels = c("CH", "CU", "FA", "FC", "FS", "SI", "SL", "SW"),
#         ordered = T
#       )),
#       pitcher_l = as.integer(pitch_side == "Left")
#     ) %>%
#     filter(!is.na(pitch_type_code)) %>%
#     rename("pitcher_throws" = pitch_side)
# 
#   return(pitch_shapes)
# 
# }
# 
# # ... MAGIC -----------------
# library(BiomechR)
# library(tidyverse)
# library(DBI)
# 
# pitch_type_averages <-
#   read.csv(
#     "//AVS-FILESVR-01/Baseball/Analytics Share/upd_run_environment/pitch_type_averages.csv"
#   )
# 
# avg_action_values <-
#   read.csv("//AVS-FILESVR-01/Baseball/Analytics Share/upd_run_environment/avg_action_values.csv")
# 
# averages_oa <-
#   read.csv("//AVS-FILESVR-01/Baseball/Analytics Share/upd_run_environment/oa_averages.csv")
# names(averages_oa) <- tolower(names(averages_oa))
# 
# load("//AVS-FILESVR-01/Baseball/Analytics Share/upd_run_environment/fxrv_rescale_model.RData")
# 
# 
# create_trajectory <- function(player_id = 669923,
#                               year_input = 2024,
#                               goal_pitchtype_input = 'SW', # pitch type action for future action score
#                               reference_pitchtype_input = 'CU', # filter for this pitch type
#                               releasespeed_input = 84,
#                               spin_rate_input = 2300,
#                               spin_efficiency_input = .4,
#                               # gyro_degree_input = ,
#                               gyro_degree_sign_input = 1,
#                               spin_axis_input = 45,
#                               seam_lat = 0,
#                               seam_lon = 0,
#                               batterside_input = 'Right',
# ){
# 
#   sfx_year_data <- pull_sfx_year_data(player_id = player_id,
#                                       year_input = year_input)
# 
#   focus_sfx_year_data <- sfx_year_data %>%
#     filter(pitch_type == reference_pitchtype_input)
# 
# 
#   focus_cols <- pull_focus_cols()
# 
#   pitch_group_input <- case_when(
#     goal_pitchtype_input %in% c("FA", "FC", "SI") ~ "fb",
#     goal_pitchtype_input %in% c("CU", "SL", "SW") ~ "bb",
#     goal_pitchtype_input %in% c("FS", "CH") ~ "ch",
#     T ~ NA_character_)
# 
#   ## create start df
#   ## need 3 rows in order for prediction to run. Idk why
#   start_df <- as_tibble(setNames(replicate(length(focus_cols), 0, simplify = FALSE), focus_cols))
#   dummy_data1 <- as_tibble(setNames(replicate(length(focus_cols), 1, simplify = FALSE), focus_cols))
#   dummy_data2 <- as_tibble(setNames(replicate(length(focus_cols), 2, simplify = FALSE), focus_cols))
# 
#   gyro_degree_input <- sign(gyro_degree_sign_input) * rad2deg(acos(spin_efficiency_input))
# 
#   spin_data <- calculate_spin(spin_rate_input, gyro_degree_input, spin_axis_input)
# 
#   traj_data <- tibble(
#     ReleaseSpeed = releasespeed_input,
#     spin_backspin = spin_data$Backspin,
#     spin_sidespin = spin_data$Sidespin,
#     spin_gyrospin = spin_data$Gyro_Spin,
#     Extension = round(median(focus_sfx_year_data$Extension, na.rm = T), 2)
#   )
# 
# 
#   ## physics break
#   phys_breaks <- estimate_phys_break(traj_data)
# 
#   ## seam data
#   start_df$seam_orientation_lat = seam_lat
#   start_df$seam_orientation_lon = seam_lon
# 
#   ## calculated inputs
#   start_df$spin_backspin = spin_data$Backspin
#   start_df$spin_sidespin = spin_data$Sidespin
#   start_df$spin_gyrospin = spin_data$Gyro_Spin
#   start_df$spin_rate = spin_rate_input
# 
#   start_df$spin_efficiency = spin_efficiency_input
# 
#   # reference release
#   start_df$release_ext = traj_data$Extension
#   start_df$release_x = round(median(focus_sfx_year_data$release_x, na.rm = T), 2)
#   start_df$release_z = round(median(focus_sfx_year_data$release_z, na.rm = T), 2)
# 
# 
#   start_df$phys_break_x = phys_breaks$phys_HB
#   start_df$phys_break_z = phys_breaks$phys_IVB
# 
#   start_df$pitch_type = goal_pitchtype_input
#   start_df$pitch_group = pitch_group_input
#   start_df$pitcher_throws = unique(sfx_year_data$pitcher_throws)
#   start_df$pitcher_l = as.integer(unique(sfx_year_data$pitcher_throws) == "Left")
#   start_df$platoon_state = tolower(paste0(substr(batterside_input, 1, 1), "v", substr(unique(sfx_year_data$pitcher_throws), 1, 1)))
#   start_df$bat_side = batterside_input
# 
# 
# 
#   ## manual inputs
#   # spin_rate_input = 2300
#   # velo_input = 84
#   # spin_efficiency_input = .50
#   # extension_input = 5.6
#   # gyro_degree_sign_input = 1
# 
#   # spin_axis_input = 45
# 
#   ## spin_data
# 
#   # traj_data <- tibble(
#   #   ReleaseSpeed = velo_input,
#   #   spin_backspin = spin_data$Backspin,
#   #   spin_sidespin = spin_data$Sidespin,
#   #   spin_gyrospin = spin_data$Gyro_Spin,
#   #   Extension = extension_input
#   # )
# 
# 
# 
#   ## old inputs working -----
#   # spin_data <- calculate_spin(spin_rate_input, gyro_degree, 45)
#   #
#   # pitchtype_input = 'SW'
#   # reference_pitchtype_input = 'FA'
#   # batterside_input = 'Left'
#   # pitcherthrows_input = unique(pitch_shapes$pitcher_throws)
#   #
#   # ## calculated inputs
#   # start_df$spin_backspin = spin_data$Backspin
#   # start_df$spin_sidespin = spin_data$Sidespin
#   # start_df$spin_gyrospin = spin_data$Gyro_Spin
#   #
#   # start_df$phys_break_x = phys_breaks$phys_HB
#   # start_df$phys_break_z = phys_breaks$phys_IVB
# 
#   ## inputs
#   # start_df$start_speed = velo_input
#   # start_df$pitch_type = pitchtype_input
#   # start_df$bat_side = batterside_input
#   # start_df$pitcher_throws = pitcherthrows_input
#   # start_df$spin_efficiency = spin_efficiency_input
#   # start_df$spin_rate = spin_rate_input
#   #
#   # start_df$pitcher_id
#   # start_df$year = 2025
#   # start_df$game_id = 66666
#   # start_df$pitcher_id = 000000
#   #
#   # start_df$break_x = 10
#   # start_df$break_z = 10
#   # start_df$seam_orientation_lat = 0
#   # start_df$seam_orientation_lon = 0
#   # start_df$release_x =  mean(pitch_shapes$release_x[pitch_shapes$pitch_type == reference_pitchtype_input], na.rm = T)
#   # start_df$release_z =  mean(pitch_shapes$release_z[pitch_shapes$pitch_type == reference_pitchtype_input], na.rm = T)
#   # start_df$horz_release_angle = -2.6
#   # start_df$vert_release_angle = -1.5
#   #
#   # start_df$avg_fb_velo =  mean(pitch_shapes$start_speed[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$avg_fb_break_x = mean(pitch_shapes$break_x[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$avg_fb_break_z = mean(pitch_shapes$break_z[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$avg_fb_release_x = mean(pitch_shapes$release_x[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$avg_fb_release_z = mean(pitch_shapes$release_z[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   #
#   #
#   # start_df$game_avg_fb_break_x = mean(pitch_shapes$break_x[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$game_avg_fb_break_z = mean(pitch_shapes$break_z[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$game_avg_fb_release_x = mean(pitch_shapes$release_x[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$game_avg_fb_release_z = mean(pitch_shapes$release_z[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   #
#   # start_df$year_avg_fb_break_x = mean(pitch_shapes$break_x[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$year_avg_fb_break_z = mean(pitch_shapes$break_z[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$year_avg_fb_release_x = mean(pitch_shapes$release_x[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   # start_df$year_avg_fb_release_z = mean(pitch_shapes$release_z[pitch_shapes$pitch_type == 'FA'], na.rm = T)
#   #
#   # start_df$pitch_group =  dplyr::case_when(
#   #   pitchtype_input %in% c("FA", "FC", "SI") ~ "fb",
#   #   pitchtype_input %in% c("CU", "SL", "SW") ~ "bb",
#   #   pitchtype_input %in% c("FS", "CH") ~ "ch",
#   #   T ~ NA_character_
#   # )
#   #
#   # start_df$release_ext = mean(pitch_shapes$release_ext, na.rm = T)
#   # start_df$pitch_type_code = as.integer(factor(
#   #   pitchtype_input,
#   #   levels = c("CH", "CU", "FA", "FC", "FS", "SI", "SL", "SW"),
#   #   ordered = T))
#   # start_df$pitcher_l = '1'
#   # start_df$platoon_state = tolower(paste0(substr(batterside_input, 1, 1), "v", substr(pitcherthrows_input, 1, 1)))
# 
#   # start_df$release_z = 6
#   # start_df$release_x =  2
# 
#   start_df_final = bind_rows(start_df,
#                              dummy_data1 %>%
#                                mutate(pitch_type = as.character(pitch_type),
#                                       pitcher_throws = as.character(pitcher_throws),
#                                       bat_side = as.character(bat_side),
#                                       pitch_group = as.character(pitch_group),
#                                       platoon_state = as.character(platoon_state)
#                                       # pitcher_l = as.character(pitcher_l)
#                                ),
#                              dummy_data2 %>%
#                                mutate(pitch_type = as.character(pitch_type),
#                                       pitcher_throws = as.character(pitcher_throws),
#                                       bat_side = as.character(bat_side),
#                                       pitch_group = as.character(pitch_group),
#                                       platoon_state = as.character(platoon_state)
#                                       # pitcher_l = as.character(pitcher_l)
#                                )
#   )
# 
#   start_df_final <- start_df_final %>%
#     mutate(PitchUID = as.character(row_number()),
#            game_id = as.character(row_number()),
#            date = as.Date(date)) %>%
#     dplyr::select(PitchUID, everything())
# 
#   ## Optimize
#   pitch_shapes_opt = predict_opt_seams(start_df_final,'ssw_break')  %>%
#     filter(pitcher_id == 0)
# 
#   pitch_shapes_action = predict_opt_seams(pitch_shapes_opt,'ssw_action')
# 
#   data_seams = predict_opt_seams(pitch_shapes_action,'ssw_grid_search')
# 
#   spin_dat = pitch_shapes_opt %>%
#     dplyr::select(pitcher_id, pitch_type, seam_orientation_lat, seam_orientation_lon, spin_gyrospin, spin_backspin, spin_sidespin, spin_efficiency) %>%
#     dplyr::slice(1)
# 
# 
# 
#   df_optim_new = data_seams %>%
#     mutate(
#       action_fxwoba = 0.321+1.384*3.892697 * action_fxrv) %>%
#     filter(action_fxwoba == min(action_fxwoba)) |>
#     dplyr::slice(1) %>%
#     rename("optim_lat" = seam_orientation_lat,
#            "optim_lon" = seam_orientation_lon,
#            "optim_act" = action_fxwoba)
# 
#   return(df_optim_new)
# }
# 
# 
# 
# 
# 
# 
# 
