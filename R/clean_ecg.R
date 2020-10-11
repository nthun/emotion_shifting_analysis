library(tidyverse)
library(vroom)
library(hms)
library(fs)
library(lubridate)
library(hms)
source("R/downsample.R")
source("R/read_all_zip.R")

# Setup parallell processing
theme_set(theme_light())

# File properties
raw_dir = "data/0_raw/ecg/01_ecg_task/"
downsampled_dir = "data/1_downsampled_ecg/"
driftless_dir = "data/2_driftless_ecg/"

header_length = 14L
footer_length = 3L
pattern = ".txt"

start_rate = 2048L
end_rate = 256L


# Downsample raw data from 2048 Hz to 256 Hz ----------------------------------------

# Read raw data
raw_df <-
  vroom(dir_ls(raw_dir, regexp = pattern, recurse = TRUE), 
        id = "file",
        delim = "\t",
        skip = header_length,
        col_names = c("time", "ecg", "gsr", "rsp","event", "event2"),
        col_types = "_n__c_") %>%  
        # The original time column should be changed
        group_by(file) %>% 
        mutate(time = row_number()) %>% 
        ungroup()

raw_df2 <-
  read_all_zip(
        "data/0_raw/K3task-20200916T111610Z-001.zip",
        id = "file",
        delim = "\t",
        skip = header_length,
        col_names = c("time", "ecg", "event", "event2"),
        guess_max = 1e6) %>%  
  # The original time column should be changed
  group_by(file) %>% 
  mutate(time = row_number()) %>% 
  ungroup()



# Downsampling ----------------------------------------------------------------------
# No need to repeat this, just ise the files from the downsampled library


downsampled_df <-
  raw_df %>%
  group_nest(file) %>%
  mutate(
         ds_file = str_replace(file, raw_dir, downsampled_dir) %>%
                   str_remove("ecg_rest/|ecg_task/"),
         ecg_ds = map(data,
                             .progress = TRUE,
                             ~downsample(.x,
                                         variable = "ecg",
                                         from = start_rate,
                                         to = end_rate))
  )
  
downsampled_df <-
  downsampled_df %>% 
  mutate(ds_file = str_replace_all(ds_file, "0_raw/K3task-20200916T111610Z-001.zip:K3task", "1_downsampled_ecg"))
  
# Save the files separately to a new library
walk2(downsampled_df$ecg_ds,
      downsampled_df$ds_file,
      ~vroom_write(x = .x,
                   path = .y,
                   na = "",
                   col_names = FALSE))


# Correct movement drift ------------------------------------------------------------

# No need to repeat this, just use the files in the driftless library
# Read the downsampled data
downsampled_df <- vroom(dir_ls(downsampled_dir, regexp = pattern, recurse = TRUE),
                        id = "file",
                        delim = "\t",
                        col_names = c("time", "ecg"))


# Removing movement drift from the data (using 1s window rolling mean)

driftless_df <- 
  downsampled_df %>%
  group_by(file) %>%
  transmute(
            # time,
            # Keep only the corrected value as the only variable
            corrected = ecg - caTools::runmean(ecg, end_rate, align = "center")) %>%
  nest() %>%
  mutate(driftless_file = str_replace(file, downsampled_dir, driftless_dir)) %>% 
  ungroup()

walk2(driftless_df$data,
      driftless_df$driftless_file,
      ~vroom_write(x = .x,
                   path = .y,
                   delim = "/t",
                   na = "",
                   col_names = FALSE))

# Add time to be able to plot this
driftless_df %>% 
  slice(1) %>% 
  unnest(data) %>% 
  mutate(time = row_number()) %>% 
  ggplot() +
  aes(x = time, y = corrected) +
  geom_line() +
  coord_cartesian(xlim = c(50, 1000), ylim = c(-1000, 2000))

# Process the markers ---------------------------------------------------------------

markers1 <- 
  raw_df %>%
  transmute(id = str_remove(file,
                            "data/0_raw/ecg/01_ecg_task/"),
            time,
            event) %>% 
  drop_na(event)

markers2 <-
  raw_df2 %>% 
  transmute(id = str_remove(file,
                            "data/0_raw/K3task-20200916T111610Z-001.zip:K3task/"),
            time,
            event) %>% 
  drop_na(event)

markers <- 
  bind_rows(markers1, markers2)

# There is a large variability in the number of markers per person
markers %>% 
  count(id, event) %>% 
  count(id) %>% 
  view()


# Create marker file
marker_times <-
  markers %>% 
  mutate(time = (time/start_rate) %>% plyr::round_any(1) %>% as_hms()) %>% 
  # Drop markers repeated in a very short interval (within 1 second)
  distinct(id, event, time) %>% 
  # Delete markers where from participants where more then a 100 markers are present
  group_by(id) %>% 
  filter(str_detect(event, "Trigger")) %>% 
  filter(n() < 100 & n() > 35) %>% 
  ungroup() %>% 
  arrange(id, time)

marker_times %>% 
  count(event) 

marker_times %>% 
  # filter(event == "Light Trigger") %>% 
  count(id) %>% 
  count(n)

invalid_times %>% 
  count(id, name = "markers") %>% 
  write_tsv("data/participants_to_exclude.txt")

write_tsv(marker_times, "data/markers.txt")


invalid_markers %>% 
  count(file) %>% 
  mutate(file = str_remove(file, "^.*(?=(/))")) %>% 
  write_tsv("invalid_markers.txt")

markers %>% 
  count(file) %>% 
  right_join(distinct(raw_df, file), by = "file") %>% 
  transmute(file = str_remove(file, "^.*(?=(/))"),
            valid_trigger = if_else(is.na(n), 0L, n)) %>% 
  arrange(file) %>% 
  write_tsv("valid_markers.txt")


markers %>% 
  mutate(time_ds = time/start_rate*end_rate) %>% 
  # count(file) %>% 
  print(n = 100)

markers %>% 
  count(file) %>% 
  count(n)
  ggplot() +
  aes(x = n) +
  geom_histogram() +
  coord_cartesian(xlim = c(0, 150))
  
all_markers <-  
  raw_df %>% 
  drop_na(event)
  
