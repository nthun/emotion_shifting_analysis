# Function to calculate regular time series interpolated HR from irregular time series IBI
# INPUT: df: a data frame with ibi data, 
#        time_col <chr> name of the column that contains time
#        ibi_col <chr> the name of the ibi column (obligatory)
#        window <int> the widht of the window in miliseconds
#        ... parameters passed to rollmean
# OUTPUT: A df with time <int> and interpolated hr <dbl>
# EXAMPLE: calculate_hr(temp, "ibi_time", "ibi", 6000)
# TODO: let the user choose between cubic spline and linear interpolation (stats::approx) purrr::invoke()
# TODO: Is ibi_time in the last row a hardcoded variable?

calculate_hr <- function(df, time_col, ibi_col, window = 1000L, ...){
    
    df %>%
        mutate(!!rlang::sym(time_col) := round(!!rlang::sym(time_col)),
               !!rlang::sym(ibi_col) := round(!!rlang::sym(ibi_col))) %>% 
        # Complete the data frame to contain all secods
        tidyr::complete(!!rlang::sym(time_col) := tidyr::full_seq(c(0, !!rlang::sym(time_col)), 
                                                                  period = 1, 
                                                                  tol = 1)) %>%
       # Use a cubic spline to interpolate data (is linear better?)
        dplyr::mutate(ibi = stats::spline(
                                            x = !!rlang::sym(time_col),
                                            y = !!rlang::sym(ibi_col),
                                            xout = !!rlang::sym(time_col)
                            )$y,
       # Linear interpolation
       # dplyr::mutate(ibi = stats::approx(
       #                                    x = !!rlang::sym(time_col),
       #                                    y = !!rlang::sym(ibi_col),
       #                                    xout = !!rlang::sym(time_col)
       #                                )$y,
       # Calculate a rolling mean of bpm using the window parameter
                      hr = 60000 / RcppRoll::roll_mean(
                                                        x = ibi, # This is new variable!
                                                        n = window,
                                                        by = 1,
                                                        # normalize = FALSE,
                                                        fill = NA,
                                                        na.rm = TRUE,
                                                        ...)) %>%
        
        # Keep only every full second
        dplyr::filter(!!rlang::sym(time_col) %% window == 0) %>%
        # Keep the time and hr cols only, transform time to seconds
        dplyr::transmute(time = !!rlang::sym(time_col)/1000,
                         hr)
}





