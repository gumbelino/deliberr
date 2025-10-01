#' Surveys
#'
#' All survey data used in deliberations.
#'
#' @format A data frame with six variables:
#' \code{type}, \code{order}, \code{statement}, \code{name}, \code{scale_max} and \code{q_method}.
"surveys"

#' Prompts
#'
#' Prompts used in DRI surveys for humans and LLMs.
#'
#' @format A data frame with two variables:
#' \code{type} and \code{prompt}
"prompts"

#' Roles
#'
#' Roles used to generate LLM role-playing data.
#'
#' @format A data frame with five variables:
#' \code{uid}, \code{type}, \code{article}, \code{role} and \code{description}.
"roles"

#' Human data
#'
#' Pre- and post-deliberation DRI survey data from 24 deliberation cases around
#' the world. Some cases used the same survey.
#'
#' @format A data frame with 67 variables, including
#' \code{survey}, \code{case}, \code{stage_id}, \code{C1...C50} and \code{P1...P10}.
"human_data"


