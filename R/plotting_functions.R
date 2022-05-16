

## Likert questions

format_question_likert <- function(sod_data,
                                   pigweb_data,
                                   levels) {
  level_key <- 1:length(levels)
  names(level_key) <- levels
  sod_data_recoded <- dplyr::recode(sod_data, !!!level_key)
  
  tibble::tibble(survey = c(rep("SoD", length(sod_data)),
                          rep("PigWeb", length(pigweb_data))),
                 response = c(sod_data_recoded,
                              pigweb_data))
}



plot_likert <- function(data,
                        levels,
                        question,
                        xlab) {
  
  ggplot2::ggplot(data) +
    geom_step(aes(x = response, colour = survey), stat = "ecdf") +
    scale_colour_manual(values = c("#c78b90", "black")) +
    scale_x_discrete(limits = levels,
                     guide = guide_axis(n.dodge = 2)) +
    xlab(xlab) +
    ggtitle(question)
  
}


## Numeric questions


format_question_numeric <- function(sod_data,
                                    pigweb_data) {
  
  pigweb_data <- as.numeric(pigweb_data)
  pigweb_data <- pigweb_data[pigweb_data != 0]
  
  sod_data <- sod_data[sod_data != 0]
  
  tibble::tibble(survey = c(rep("SoD", length(sod_data)),
                            rep("PigWeb", length(pigweb_data))),
                 response = c(sod_data,
                              pigweb_data))
}


plot_numeric <- function(data,
                         question,
                         xlab) {
  
  ggplot2::ggplot(data) +
    geom_step(aes(x = response, colour = survey), stat = "ecdf") +
    scale_colour_manual(values = c("#c78b90", "black")) +
    xlab(xlab) +
    ggtitle(question)
  
}


## Single choice questions

format_question_single_choice <- function(sod_data,
                                          pigweb_data,
                                          levels) {
  
  data_recoded <- format_question_likert(sod_data,
                                         pigweb_data,
                                         levels)
  ## Count the responses
  counts <- na.exclude(dplyr::count(dplyr::group_by(data_recoded, survey, response)))
  
  ## Augment the counts with zeros for options that weren't used
  add_missing_responses <- function(data) {
    
    used_responses <- na.exclude(unique(data$response))
    
    missing_responses <- setdiff(1:length(levels),
                                 used_responses)
    rbind(data,
          tibble::tibble(survey = rep(unique(data$survey), length(missing_responses)),
                         response = missing_responses,
                         n = rep(0, length(missing_responses))))
  }
  
  counts_split <- split(counts, counts$survey)
  
  counts_augmented <- purrr::map_dfr(counts_split,
                                     add_missing_responses)
  
  ## Calculate percentages
  survey_totals <- dplyr::count(dplyr::group_by(data_recoded, survey), name = "survey_total")
  
  counts_totals <- dplyr::inner_join(counts_augmented, survey_totals, by = "survey")
  
  counts_totals$percent <- counts_totals$n/counts_totals$survey_total * 100
  
  counts_totals
}


plot_single_choice <- function(data,
                               levels,
                               question,
                               xlab) {
  
  ggplot2::ggplot(data) +
    geom_bar(aes(x = factor(response), y = percent, fill = survey),
             stat = "identity",
             position = position_dodge()) +
    scale_x_discrete(labels = levels,
                     guide = guide_axis(n.dodge = 2)) +
    scale_fill_manual(values = c("#c78b90", "black")) +
    xlab(xlab) +
    ggtitle(question)
  
}


## Multiple choice questions

format_question_multiple_choice <- function(sod_data,
                                            pigweb_data,
                                            levels) {
  
  sod_data_recoded <- map_dfc(sod_data,
                              function(d) ifelse(d == "0", NA, 1))
  
  
  count_data <- function(data, level) {
    tibble::tibble(response = level,
                   n = sum(data == 1, na.rm = TRUE))
    
  }
  
  n_pigweb <- nrow(pigweb_data)
  n_sod <- nrow(sod_data)
  
  pigweb_counts <- purrr::pmap_dfr(list(pigweb_data,
                                        levels),
                                   count_data)
  
  pigweb_counts$survey <- "PigWeb"
  pigweb_counts$percent <- pigweb_counts$n / n_pigweb * 100
  
  sod_counts <- purrr::pmap_dfr(list(sod_data_recoded,
                                     levels),
                                count_data)
  
  sod_counts$survey <- "SoD"
  sod_counts$percent <- sod_counts$n / n_sod * 100
  
  
  counts <- rbind(pigweb_counts, sod_counts)
  
  counts
}


plot_multiple_choice <- function(data,
                                 levels,
                                 question,
                                 xlab) {
  
  ggplot2::ggplot(data) +
    geom_bar(aes(x = factor(response, levels = levels),
                 y = percent, fill = survey),
             stat = "identity",
             position = position_dodge()) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_fill_manual(values = c("#c78b90", "black")) +
    xlab(xlab) +
    ggtitle(question)
  
}
