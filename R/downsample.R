# Downsampling function
# INPUT: df data frame containing time series data without time
#        variable a quoted variable name <str>
#        from original sampling rate <int>
#        to new sampling rate <int>
# OUTPUT: a df with time and value variables
# EXAMPLE: downsample(df, "scr", 1024, 32)

downsample <- function(df, variable, from, to){
    # We need to have a time variable for all outputs
    # First we create epochs using the original and new sampling rate
    temp <-
        df %>% 
        mutate(time = seq(0L, nrow(.) - 1, 1L) %/% (from/to)/to)
    # If the sampling rate is the same, don't do any calculation (faster)
    if (from/to == 1) return(select(temp, time, value = !!variable))    
    # If the sampling rate is different, calculate the mean of values for each epoch
    temp %>% 
        group_by(time) %>%
        summarise(value = mean(!!rlang::sym(variable), na.rm = TRUE))
}