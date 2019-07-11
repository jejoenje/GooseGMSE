source('ggmse_default_test_pars.R')

proj_yrs   <- years

goose_data <- goose_clean_data(file = data_file)

last_year  <- goose_data[dim(goose_data)[1], 1]

gmse_res   <- gmse_apply(res_mod = goose_gmse_popmod, 
                         obs_mod = goose_gmse_obsmod,
                         man_mod = goose_gmse_manmod,
                         use_mod = goose_gmse_usrmod,
                         dat = goose_data, obs_error = obs_error,
                         manage_target = manage_target, max_HB = max_HB,
                         use_est = 0, stakeholders = 1, 
                         get_res = "full")

goose_data <- sim_goose_data(gmse_results = gmse_res$basic,
                             goose_data = goose_data)

goose_data$Npred_mn[nrow(goose_data)] <- Npred_mn-gmse_res$basic$user_results
goose_data$Npred_lo[nrow(goose_data)] <- Npred_lo-gmse_res$basic$user_results
goose_data$Npred_hi[nrow(goose_data)] <- Npred_hi-gmse_res$basic$user_results

goose_data$y[goose_data$y<0] <- 0
goose_data$Npred_mn[goose_data$Npred_mn<0] <- 0