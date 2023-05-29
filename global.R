library(shiny)
library(shinyWidgets)
library(sortable)

library(DT)
library(gt)
library(gtExtras)

library(tidyverse)
library(tidyfast)

library(qualtRics)
library(cregg)

q2c <- function (data,
                 prefix,
                 id,
                 outcome,
                 covariates,
                 type,
                 legacy) {
    
    # Qualtricsからexportしたファイルの場合、2、3行目は不要なので
    # 2、3行目を除いて読み込む
    if (is.character(data)) {
        col_names <- read_csv(data, show_col_types = FALSE, n_max = 0) |>
            names()
        
        if (legacy) {
            data <- read_csv(data, skip = 2, show_col_types = FALSE,
                             col_names = col_names)
        } else {
            data <- read_csv(data, skip = 3, show_col_types = FALSE,
                             col_names = col_names)
        }
        
    }
    
    temp_data <- data |>
        drop_na({{ outcome }})
    
    prefix <- paste0(prefix, "-")
    
    temp_cj <- temp_data |>
        select(ID = {{ id }}, starts_with(prefix))
    
    temp_out <- temp_data |>
        select(ID = {{ id }}, {{ outcome }})
    
    temp_cov <- temp_data |>
        select(ID = {{ id }}, {{ covariates }})
    
    if (type %in% c("rating", "rank")) {
        n_task    <- (ncol(temp_out) - 1) / 2
        n_profile <- ncol(temp_out) / n_task
        names(temp_out)[-1] <- paste(rep(paste0("Out", 1:n_task), 
                                         each = n_profile),
                                     rep(1:n_profile), sep = "_")
        
        temp_out <- temp_out |>
            pivot_longer(cols      = -ID,
                         names_to  = "Task",
                         values_to = "Outcome") |>
            separate(col  = Task,
                     into = c("Task", "Profile")) |>
            mutate(Task    = parse_number(Task),
                   Profile = as.numeric(Profile))
    } else if (type == "choice") {
        n_task    <- ncol(temp_out) - 1
        names(temp_out)[-1] <- paste0("Out", 1:(ncol(temp_out) - 1))
        
        temp_out <- temp_out |>
            pivot_longer(cols = -ID,
                         names_to = "Task",
                         values_to = "Outcome") |>
            mutate(Task = parse_number(Task))
    }
    
    temp_cj <- temp_cj |>
        pivot_longer(cols      = -ID,
                     names_to  = "Full_Attr",
                     values_to = "Levels") |>
        dt_separate(col = Full_Attr,
                    into = c("Prefix", "Task", "Profile", "Attr"),
                    sep = "-",
                    fill = NA,
                    remove = FALSE) |>
        as_tibble() |>
        select(-Prefix, -Full_Attr) |>
        mutate(across(Task:Attr, as.numeric),
               Attr_D  = ifelse(is.na(Attr), TRUE, FALSE),
               Attr    = ifelse(Attr_D, Profile, Attr),
               Profile = ifelse(Attr_D, 0, Profile)) |>
        arrange(ID, Task, Attr, Profile) |>
        mutate(Attr = ifelse(Attr_D, Levels, NA)) |>
        fill(Attr) |>
        filter(!Attr_D) |>
        select(-Attr_D) |>
        pivot_wider(id_cols = c(ID, Task, Profile),
                    names_from  = "Attr",
                    values_from = "Levels")
    
    if (type == "choice") {
        result <- left_join(temp_cj, temp_out, by = c("ID", "Task")) |>
            relocate(Outcome, .after = Profile) |>
            mutate(Outcome = if_else(Profile == Outcome, 1, 0))
    } else {
        result <- left_join(temp_cj, temp_out, 
                            by = c("ID", "Task", "Profile")) |>
            relocate(Outcome, .after = Profile)
    }
    
    result <- left_join(result, temp_cov, by = "ID")
    
    result
}