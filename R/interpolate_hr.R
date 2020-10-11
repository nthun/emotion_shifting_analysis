# Intrapolate IBI (using both original and corrected IBI)

library(tidyverse)
library(fs)
source("R/calculate_hr.R")
theme_set(theme_light())

ibi <-
    tibble(artifact_file = dir_ls(path = "data/4_ibi_corrected/", regexp = "IBI_artifacts_IBIs_from_"),
           corrected_file = dir_ls(path = "data/4_ibi_corrected/", regexp = "IBI_artifactCorrected_IBIs_from_")) %>% 
    extract(artifact_file, 
            into = "id", 
            regex = "^.*_from_(\\d+.*)_task.txt.csv$", 
            remove = FALSE) %>% 
    mutate(id,
           artifact_data = map(artifact_file, ~read_csv(.x, col_types = "di")),
           corrected_data = map(corrected_file, ~read_csv(.x, col_types = "d"))) %>% 
    transmute(id,
              data = map2(artifact_data,
                          corrected_data,
                          ~bind_cols(.x, .y))) %>%
    unnest(data) %>% 
    rename(ibi = IBI, ibi_corrected = IBI_artefactCorrected)

# Plot all ibis against corrected ibis for each session
# It is a good way to explore artifacts, and really big differences (that make the green line diverge from 45 degree angle)
ibi %>% 
    ggplot() +
    aes(x = ibi, y = ibi_corrected, color = (artifact != 1)) +
    geom_point(size = .1) +
    facet_wrap(~id, scales = "free")

hr <-
    ibi %>%
    group_by(id) %>%
    mutate(ibi_time = cumsum(ibi)) %>%
    select(-ibi,-artifact) %>%
    nest() %>%
    mutate(hr_data = purrr::map(
                        data,
                        ~calculate_hr(
                            df = .x,
                            time_col = "ibi_time",
                            ibi_col = "ibi_corrected",
                            window = 500,
                            align = "center"
                        )
                    )
    )

beepr::beep(sound = 1)

hr %>%
    select(-data) %>% 
    unnest(hr_data) %>%
    write_excel_csv("data/5_hr/hr_berntson_cubic.csv")

hr %>% 
    mutate(avg_hr = map_dbl(hr_data, ~mean(.x$hr, na.rm = TRUE))) %>% 
    arrange(avg_hr) %>% 
    view()
    

    