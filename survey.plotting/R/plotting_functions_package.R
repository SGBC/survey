###############################################################################
## Helper functions
###############################################################################
## If there are individuals with no answer, add the "no answer" level

#' Add a "no answer" level
#' 
#' @description If there are individuals with no answer, add the "no answer" level
#' 
#' @param data survey replies with a value of 0, which equals "no answer"
#' @param levels Adding a new "No answer level" to a vector containing all levels
#'
#' @return
#' @export
#'
#' @examples
#' add_no_answer_level(data, Q1_levels)
#'
add_no_answer_level = function(data,
                               levels) {
  
  if (any(data$response == 0)) {
    levels = c("No answer", levels)
  }
  
  levels
}


###############################################################################
## Likert questions
###############################################################################

## Create a data frame for plotting that combines SoD and PigWeb data
## Arguments:
## * sod_data and pigweb_data are vectors of answers (i.e. the column of
##   the respective data frame that contains the answers)
## * levels is a vector of the levels in order from low to high

#'Create data frame for plotting likert questions
#' 
#' @description Create a data frame for plotting that combines SoD and PigWeb data
#' 
#' @param sod_data a vector of answers from the sod_data data frame that contains the answers to the specific question
#' @param pigweb_data a vector of answers from the pigw_data data frame that contains the answers to the specific question
#' @param levels A vector of the levels of response categories in order from low to high
#' 
#' @return
#' @export
#'
#' @examples 
#' format_question_likert(sod21, pigw21, Q1_levels)
#'
format_question_likert = function(sod_data,
                                  pigweb_data,
                                  levels) {
  if (!is.null(pigweb_data)) {
    ## Remove NA observations
    pigweb_data = na.exclude(pigweb_data)
  }
  
  if (is.null(sod_data)) {
    sod_data_recoded <- NULL
  } else  {
    
    ## Remove NA observations
    sod_data = na.exclude(sod_data)
    pigweb_data = na.exclude(pigweb_data)

    
    
    ## Recode the SoD data to numbers based on levels
    level_key = c(1:(length(levels)), 0)
    names(level_key) = c(levels, "0")
    sod_data_recoded = dplyr::recode(sod_data, !!!level_key)
  }

  tibble::tibble(survey = c(rep("SoD", length(sod_data)),
                            rep("PigWeb", length(pigweb_data))),
                 response = c(sod_data_recoded,
                              pigweb_data))
}


## Create a cumulative step graph of the answers for a Likert-scale question
## Argument:
## * data is a data frame produced by format_question_likert
## * levels is a vector of levels in order from low to high
## * question is a character string of the question which will be the title
## * xlab is a character string for the x-axis label

#' Plot a cumulative step graph of the answers for a Likert-scale question
#' 
#' @description Create a cumulative step graph of the answers for a Likert-scale question
#' 
#' @param data data frame produced by function format_question_likert()
#' @param levels vector of the levels of response categories in order from low to high
#' @param question character string of the question, will be the title of the plot
#' @param xlab character string for the x-axis label
#' 
#' @return
#' @export
#'
#' @examples 
#' plot_likert(Q1_data, Q1_levels, Q1,"Time since most recent publication")
#'
plot_likert = function(data,
                       levels,
                       question,
                       xlab) {
  
#  levels = add_no_answer_level(data, levels)
  
  ggplot2::ggplot(data) +
    geom_step(aes(x = response, colour = survey), stat = "ecdf") +
    scale_colour_manual(values = c("#c78b90", "black")) +
    scale_x_discrete(limits = levels,
                     labels = stringr::str_wrap(levels, width = 30),
                     guide = guide_axis(n.dodge = 2)) +
    ylab("Proportion of respondents") +
    xlab(xlab) +
    ggtitle(stringr::str_wrap(question, width = 60))
  
}

###############################################################################
## Reverses the order of Likert scale question responses.
###############################################################################
#'Reverses the order of Likert scale question responses.
#'
#' @param x is a scalar from or a vector of Likert scores to reverse code
#' @param min is the minimum value of the Likert scale
#' @param max is the maxium value of the Likert scale
#'
#' @return
#' @export
#'
#' @examples
#' reverseCode(2, min = 1, max = 5) # reverse code "2" to a "4" on a Likert scale of 1-5
#' reverseCode(2:7, min = 2, max = 7) # reverse code a vector of scores on a Likert scale of 2-7
#' reverseCode(1, min = 0, max = 1) # reverse code binary response
#'
reverseCode <- function(x, min = 1, max = 5){
  
  # Written by Parker Tichko, May 2020
  # Email: my first name DOT my last name @ gmail.com
  # Source: https://ptichko.github.io/2020/06/04/R-Function-To-Reverse-Code-Likert-Scale.html
  
  if(min(x, na.rm = TRUE) < min | max(x, na.rm = TRUE) > max){
    warning("Warning: input is outside the range of the scale.")
  }
  
  sort(min:max, decreasing = TRUE)[x+(1-min)]
  
}


###############################################################################
## Numeric questions
###############################################################################

## Create a data frame for plotting numeric questions 
## Arguments:
## * sod_data and pigweb_data are vectors of answers (i.e. the column of
##   the respective data frame that contains the answers)

#' Create a data frame for plotting numeric questions
#' 
#' @description Create a data frame for plotting numeric questions
#' 
#' @param sod_data vector of answers from the data frame that contains the answers from the SOD 2021 survey
#' @param pigweb_data vector of answers from the data frame that contains the answers from the PigWeb 2021 survey
#'
#' @return
#' @export
#'
#' @examples
#' format_question_numeric(sod21, pigw21)
#'
format_question_numeric = function(sod_data,
                                   pigweb_data) {
  
  if (!is.null(pigweb_data)) {
    pigweb_data = as.numeric(pigweb_data)
    pigweb_data = pigweb_data[pigweb_data != 0]
  }
  
  if (!is.null(sod_data)) {
    sod_data = sod_data[sod_data != 0]
  }
  
  tibble::tibble(survey = c(rep("SoD", length(sod_data)),
                            rep("PigWeb", length(pigweb_data))),
                 response = c(sod_data,
                              pigweb_data))
}


## Create a cumulative step graph of the answers for a numeric question
## Argument:
## * data is a data frame produced by format_question_numeric
## * question is a character string of the question which will be the title
## * xlab is a character string for the x-axis label

#' Plot a cumulative step graph of the answers for a numeric question
#' 
#' @description Create a cumulative step graph of the answers for a numeric question
#' 
#' @param data data frame produced by function format_question_numeric()
#' @param question character string of the question, will be the title of the plot
#' @param xlab character string for the x-axis label
#' 
#' @return
#' @export
#'
#' @examples
#' plot_numeric(Q2_data, Q2, "Year of first publication by respondent")
#'
plot_numeric = function(data,
                        question,
                        xlab) {
  
  ggplot2::ggplot(data) +
    geom_step(aes(x = response, colour = survey), stat = "ecdf") +
    scale_colour_manual(values = c("#c78b90", "black")) +
    xlab(xlab) +
    ggtitle(stringr::str_wrap(question, width = 60))
  
}


###############################################################################
## Single choice questions
###############################################################################

## Create a data frame for plotting for single-choice questions.
## The resulting data frame contains percentages for each option.
## Arguments:
## * sod_data and pigweb_data are vectors of answers (i.e. the column of
##   the respective data frame that contains the answers)
## * levels is a vector of the options

#' Create a data frame for plotting for single-choice questions
#' 
#' @description Create a data frame for plotting for single-choice questions
#' 
#' @param sod_data vector of answers from the data frame that contains the answers from the SOD 2021 survey
#' @param pigweb_data vector of answers from the data frame that contains the answers from the PigWeb 2021 survey
#' @param levels vector of the levels of response categories
#'
#' @return
#' @export
#'
#' @examples
#' format_question_single_choice(sod21, pigw21, Q3_levels)
#'
format_question_single_choice = function(sod_data,
                                         pigweb_data,
                                         levels) {
  
  ## Recode the data in the same way as for a Likert question
  data_recoded = format_question_likert(sod_data,
                                        pigweb_data,
                                        levels)
  ## Count the responses
  counts = na.exclude(dplyr::count(dplyr::group_by(data_recoded, survey, response)))
  
  add_unused_responses = function(data) {
    
    used_responses = na.exclude(unique(data$response))
    
    unused_responses = setdiff(0:length(levels),
                               used_responses)
    rbind(data,
          tibble::tibble(survey = rep(unique(data$survey), length(unused_responses)),
                         response = unused_responses,
                         n = rep(0, length(unused_responses))))
  }
  
  counts_split = split(counts, counts$survey)
  
  counts_augmented = purrr::map_dfr(counts_split,
                                    add_unused_responses)
  
  ## Calculate percentages
  survey_totals = dplyr::count(dplyr::group_by(data_recoded, survey), name = "survey_total")
  
  counts_totals = dplyr::inner_join(counts_augmented, survey_totals, by = "survey")
  
  counts_totals$percent = counts_totals$n/counts_totals$survey_total * 100
  
  counts_totals
}


## Create a bar chart of percentages for a single response question
## Argument:
## * data is a data frame produced by format_question_single_choice
## * levels is a vector of the options
## * question is a character string of the question which will be the title
## * xlab is a character string for the x-axis label

#' Plot a bar chart of percentages for a single response question
#' 
#' @description Create a bar chart of percentages for a single response question
#' 
#' @param data data frame produced by function format_question_single_choice()
#' @param levels vector of the levels of response categories
#' @param question character string of the question, will be the title of the plot
#' @param xlab character string for the x-axis label
#' 
#' @return
#' @export
#'
#' @examples
#' plot_single_choice(Q3_data, Q3_levels, Q3, "Area of interest")
#'
plot_single_choice = function(data,
                              levels,
                              question,
                              xlab) {
  
  levels = add_no_answer_level(data, levels)
  
  ggplot2::ggplot(data) +
    geom_bar(aes(x = factor(response), y = percent, fill = survey),
             stat = "identity",
             position = position_dodge()) +
    scale_x_discrete(labels = stringr::str_wrap(levels, width = 30),
                     guide = guide_axis(n.dodge = 2)) +
    scale_fill_manual(values = c("#c78b90", "black")) +
    xlab(xlab) +
    ggtitle(stringr::str_wrap(question, width = 60))
  
}


###############################################################################
## Multiple choice questions
###############################################################################

## Create a data frame for plotting for multiple-choice questions.
## The resulting data frame contains percentages for each option.
## Arguments:
## * sod_data and pigweb_data are data frames of answers (i.e. the multiple
##   columns of the respective data frame that contains the answers)
## * levels is a vector of the options

#' Create a data frame for plotting for multiple-choice questions
#' 
#' @description Create a data frame for plotting for multiple-choice questions
#' 
#' @param sod_data vector of answers from the data frame that contains the answers from the SOD 2021 survey
#' @param pigweb_data vector of answers from the data frame that contains the answers from the PigWeb 2021 survey
#' @param levels vector of the levels of response categories
#' 
#' @return
#' @export
#'
#' @examples
#' format_question_multiple_choice(sod21Q4, pigw21Q4, Q4_levels)
#'
format_question_multiple_choice = function(sod_data,
                                           pigweb_data,
                                           levels) {
  
  ## Remove genuinely NA values from SoD, thus removing those who
  ## never saw the question
  sod_data = na.exclude(sod_data)
  sod_data = sod_data[!apply(sod_data == "", 1, all),]
  
  ## Recode the SoD data to 1s and NAs
  sod_data_recoded = map_dfc(sod_data,
                             function(d) ifelse(d == "0", NA, 1))
  
  ## Remove rows with no answers from Pigweb, thus removing those
  ## who have checked no choices
  to_remove = rowSums(is.na(pigweb_data)) == ncol(pigweb_data)
  pigweb_data = pigweb_data[!to_remove,]
  
  ## Count the responses
  count_data = function(data, level) {
    tibble::tibble(response = level,
                   n = sum(data == 1, na.rm = TRUE))
    
  }
  
  n_pigweb = nrow(pigweb_data)
  n_sod = nrow(sod_data)
  
  pigweb_counts = purrr::pmap_dfr(list(pigweb_data,
                                       levels),
                                  count_data)
  
  ## Calculate percentages
  pigweb_counts$survey = "PigWeb"
  pigweb_counts$percent = pigweb_counts$n / n_pigweb * 100
  
  sod_counts = purrr::pmap_dfr(list(sod_data_recoded,
                                    levels),
                               count_data)
  
  sod_counts$survey = "SoD"
  sod_counts$percent = sod_counts$n / n_sod * 100
  
  
  counts = rbind(pigweb_counts, sod_counts)
  
  counts
}


## Create a bar chart of percentages for a multiple response question
## Argument:
## * data is a data frame produced by format_question_multiple_choice
## * levels is a vector of the options
## * question is a character string of the question which will be the title
## * xlab is a character string for the x-axis label

#' Plot a bar chart of percentages for a multiple response question
#' 
#' @description Create a bar chart of percentages for a multiple response question
#' 
#' @param data data frame produced by function format_question_multiple_choice()
#' @param levels vector of the levels of response categories
#' @param question character string of the question, will be the title of the plot
#' @param xlab character string for the x-axis label
#' 
#' @return
#' @export
#'
#' @examples
#' plot_multiple_choice(Q4_data, Q4_levels, Q4, "Response")
#'
plot_multiple_choice = function(data,
                                levels,
                                question,
                                xlab) {
  
  ggplot2::ggplot(data) +
    geom_bar(aes(x = factor(response, levels = levels),
                 y = percent, fill = survey),
             stat = "identity",
             position = position_dodge()) +
    scale_x_discrete(labels = stringr::str_wrap(levels, width = 30),
                     guide = guide_axis(n.dodge = 2)) +
    scale_fill_manual(values = c("#c78b90", "black")) +
    xlab(xlab) +
    ggtitle(stringr::str_wrap(question, width = 60))
  
}


###############################################################################
## Likert questions only included in the PigWeb survey
###############################################################################

## Create a data frame for plotting that only include PigWeb data
## Arguments:
## * Pigweb_data is a vector of answers (i.e. the column of
##   the respective data frame that contains the answers)
## * levels is a vector of the levels in order from low to high

#' Create a data frame for plotting that only include PigWeb data
#' 
#' @description  Create a data frame for plotting that only include PigWeb data
#' 
#' @param pigweb_data vector of answers from the data frame that contains the answers from the PigWeb 2021 survey
#' @param levels vector of the levels of response categories in order from low to high
#' 
#' @return
#' @export
#'
#' @examples
#' format_question_likert_pigweb(pigw21Q14$Q14, Q14_levels)
#'
format_question_likert_pigweb = function(pigweb_data,
                                  levels) {
  if (!is.null(pigweb_data)) {
    ## Remove NA observations
    pigweb_data = na.exclude(pigweb_data)
  }
  
  else  {
    
    ## Remove NA observations
    pigweb_data = na.exclude(pigweb_data)
    
    
    
  }
  
  tibble::tibble(survey = rep("PigWeb", length(pigweb_data)),
                 response = pigweb_data)
}


###############################################################################
## Single choice questions only included in Pigweb survey
###############################################################################

## Create a data frame for plotting for single-choice questions.
## The resulting data frame contains percentages for each option.
## Arguments:
## * sod_data and pigweb_data are vectors of answers (i.e. the column of
##   the respective data frame that contains the answers)
## * levels is a vector of the options

#' Create a data frame for plotting for single-choice questions
#' 
#' @description Create a data frame for plotting for single-choice questions
#' 
#' @param pigweb_data vector of answers from the data frame that contains the answers from the PigWeb 2021 survey
#' @param levels vector of the levels of response categories
#'  
#' @return
#' @export
#'
#' @examples
#' format_question_single_choice_pigweb(pigw21, Q9_levels)
#'
format_question_single_choice_pigweb = function(pigweb_data,
                                         levels) {
  
  ## Recode the data in the same way as for a Likert question
  data_recoded = format_question_likert_pigweb(pigweb_data,
                                        levels)
  ## Count the responses
  counts = na.exclude(dplyr::count(dplyr::group_by(data_recoded, survey, response)))
  
  add_unused_responses = function(data) {
    
    used_responses = na.exclude(unique(data$response))
    
    unused_responses = setdiff(0:length(levels),
                               used_responses)
    rbind(data,
          tibble::tibble(survey = rep(unique(data$survey), length(unused_responses)),
                         response = unused_responses,
                         n = rep(0, length(unused_responses))))
  }
  
  counts_split = split(counts, counts$survey)
  
  counts_augmented = purrr::map_dfr(counts_split,
                                    add_unused_responses)
  
  ## Calculate percentages
  survey_totals = dplyr::count(dplyr::group_by(data_recoded, survey), name = "survey_total")
  
  counts_totals = dplyr::inner_join(counts_augmented, survey_totals, by = "survey")
  
  counts_totals$percent = counts_totals$n/counts_totals$survey_total * 100
  
  counts_totals
}


###############################################################################
## Multiple choice questions pigweb survey
###############################################################################

## Create a data frame for plotting for multiple-choice questions only included.
## in the Pigweb survey survey.
## The resulting data frame contains percentages for each option.
## Arguments:
## * sod_data and pigweb_data are data frames of answers (i.e. the multiple
##   columns of the respective data frame that contains the answers)
## * levels is a vector of the options

#' doCreate a data frame for plotting for multiple-choice questions only included
#' 
#' @description Create a data frame for plotting for multiple-choice questions only included
#' 
#' @param survey_data vector of answers from the data frame that contains the answers from the PigWeb 2021 survey
#' @param levels vector of the levels of response categories
#'  
#' @return
#' @export
#'
#' @examples
#' format_question_multiple_choice_pigweb(pigw21, Q15_levels)
#'
format_question_multiple_choice_pigweb = function(survey_data,
                                           levels) {
  survey_data = pigw21Q15
  ## Remove rows with no answers from Pigweb, thus removing those
  ## who have checked no choices
  to_remove = rowSums(is.na(survey_data)) == ncol(survey_data)
  survey_data = survey_data[!to_remove,]
  
  ## Count the responses
  count_data = function(data, level) {
    tibble::tibble(response = level,
                   n = sum(data == 1, na.rm = TRUE))
    
  }
  
  n_pigweb = nrow(survey_data)

  pigweb_counts = purrr::pmap_dfr(list(survey_data,
                                       levels),
                                  count_data)
  
  ## Calculate percentages
  pigweb_counts$survey = "PigWeb"
  pigweb_counts$percent = pigweb_counts$n / n_pigweb * 100
  
  counts = pigweb_counts
  
  counts
}
