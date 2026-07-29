#' Build a table of date ranges (phases) for temporal aggregation
#'
#' Generates a sequence of consecutive date intervals between a start and an
#' end year-month, and returns them as a \code{tibble} with the columns
#' \code{Year}, \code{Phase}, \code{start_date} and \code{end_date} — ready
#' to use as-is as the \code{phases_df} argument of \code{\link{get_gee_data}}
#' (or, when split, as a named list of smaller \code{phases_df} chunks).
#'
#' @details
#' \strong{How the interval sequence is built}
#'
#' \code{start_dates} are generated with \code{\link[base]{seq}} from the
#' first day of \code{start_year_month} to the first day of
#' \code{end_year_month}, stepping by \code{by}. Each \code{end_date} is the
#' following \code{start_date} in that sequence (i.e. an exclusive upper
#' bound), except for the very last row, whose \code{end_date} is instead the
#' first day of the month right after \code{end_year_month}. This mirrors
#' how \code{\link{get_gee_data}} filters images (\code{start_date} inclusive,
#' \code{end_date} exclusive), so consecutive phases never overlap and never
#' leave a gap between them.
#'
#' \strong{Phase labels}
#'
#' The \code{Phase} column is built differently depending on \code{by}:
#' \itemize{
#'   \item \code{"day"}: \code{"YYYY_Month_DD"}.
#'   \item \code{"week"}: \code{"YYYY_Week_WW"}.
#'   \item \code{"month"}: \code{"YYYY_Month"}.
#'   \item \code{"2 months"} through \code{"6 months"}: a range label such as
#'   \code{"2023_Oct_2023_Nov"}, built from the start month and the month
#'   right before \code{end_date}.
#'   \item \code{"year"}: \code{"YYYY"}.
#' }
#'
#' \strong{Splitting the output}
#'
#' When \code{split_by > 1}, the rows of the resulting table are split into
#' \code{split_by} contiguous groups (via \code{\link[base]{cut}}), and a
#' \emph{named list} of tibbles is returned instead of a single tibble — one
#' element per group, named \code{"G1"}, \code{"G2"}, etc. (zero-padded to a
#' constant width). This is convenient when a long interval needs to be
#' processed by \code{\link{get_gee_data}} in smaller batches, e.g. across
#' separate calls or in parallel. If \code{split_by} is larger than the
#' number of rows generated, it is capped to that number of rows.
#'
#' @param start_year_month A single string in \code{"YYYY-MM"} format giving
#'   the first month of the overall interval (e.g. \code{"2023-10"}).
#' @param end_year_month A single string in \code{"YYYY-MM"} format giving
#'   the last month of the overall interval (e.g. \code{"2024-03"}). Must not
#'   be earlier than \code{start_year_month}.
#' @param by A single string giving the step used to build the date
#'   sequence. One of \code{"day"}, \code{"week"}, \code{"month"},
#'   \code{"2 months"}, \code{"3 months"}, \code{"4 months"},
#'   \code{"5 months"}, \code{"6 months"} or \code{"year"}.
#' @param split_by Integer, default \code{1L}. Number of contiguous groups to
#'   split the output table into (see Details). Values \code{< 1} fall back
#'   to the default (\code{1L}, i.e. no splitting), and non-integer numeric
#'   values are coerced to integer, both with a message.
#'
#' @return If \code{split_by == 1} (the default), a \code{tibble} with
#'   columns:
#'   \describe{
#'     \item{Year}{Integer year of \code{start_date}.}
#'     \item{Month}{Factor (levels \code{month.name}) with the month name of
#'     \code{start_date}.}
#'     \item{Phase}{Factor identifying each date range (see Details).}
#'     \item{start_date}{\code{Date}, inclusive lower bound of the phase.}
#'     \item{end_date}{\code{Date}, exclusive upper bound of the phase.}
#'   }
#'   If \code{split_by > 1}, a named list of such tibbles (\code{"G1"},
#'   \code{"G2"}, \dots), one per group.
#'
#' @examples
#' # A single table with one row per month
#' create_time_table("2023-10", "2024-03", by = "month")
#'
#' # Quarterly (3-month) phases
#' create_time_table("2023-10", "2024-09", by = "3 months")
#'
#' # Split a full year of monthly phases into 4 chunks, e.g. to run
#' # get_gee_data() in smaller batches
#' create_time_table("2023-01", "2023-12", by = "month", split_by = 4)
#'
#' @export
create_time_table <- function(
    start_year_month,
    end_year_month,
    by = c(
      "day",
      "week",
      "month",
      "2 months",
      "3 months",
      "4 months",
      "5 months",
      "6 months",
      "year"
    ),
    split_by = 1L
) {


  by <- match.arg(by)


  if (is.numeric(split_by) && (split_by >= 1L)) {

    if (!is.integer(split_by)) {

      message("ATENTION! split_by must be an integer number.")

      split_by <- as.integer(split_by)

    } else {

      split_by <- as.integer(split_by)

    }


  } else {

    message("ATENTION! split_by must be an integer number and equal or heigher than 1.")
    message("Using the default argument: 1.")

    split_by = 1L

  }


  # Convert inputs to Date class (Pure dates, no hours/tz)
  start_date <- as.Date(paste0(start_year_month, "-01"))

  # Calculate the absolute end boundary (First day of the next month following the end boundary)
  end_year <- as.integer(substr(end_year_month, 1, 4))
  end_month <- as.integer(substr(end_year_month, 6, 7))

  if (end_month == 12) {

    next_month_str <- paste0(end_year + 1, "-01-01")

  } else {

    next_month_str <- sprintf("%04d-%02d-01", end_year, end_month + 1)
  }

  absolute_max_end <- as.Date(next_month_str)

  # Base end date for the sequence generation
  end_month_date <- as.Date(paste0(end_year_month, "-01"))

  # Generate initial start sequence
  start_dates <- seq(from = start_date, to = end_month_date, by = by)

  # Generate the next sequence starts (which serve as the exclusive end_dates for GEE)
  end_dates <- c(start_dates[-1], seq(
    from = utils::tail(start_dates, 1),
    length.out = 2,
    by = by
  )[2])

  # Cap the final end date at the absolute user maximum
  end_dates <- pmin(end_dates, absolute_max_end)

  # Helper to generate custom phase names for multi-month intervals
  generate_multi_month_phase <- function(starts, ends) {
    start_lbl <- format(starts, "%Y_%b")
    # Subtract 1 day just for the label to correctly represent the month name
    end_lbl <- format(ends - 1, "%Y_%b")
    paste0(start_lbl, "_", end_lbl)
  }

  # Define Phase column dynamically based on selected interval
  phase_vector <- switch(
    by,
    day = format(start_dates, "%Y_%B_%d"),
    week = format(start_dates, "%Y_Week_%U"),
    month = format(start_dates, "%Y_%B"),
    `2 months` = generate_multi_month_phase(start_dates, end_dates),
    `3 months` = generate_multi_month_phase(start_dates, end_dates),
    `4 months` = generate_multi_month_phase(start_dates, end_dates),
    `5 months` = generate_multi_month_phase(start_dates, end_dates),
    `6 months` = generate_multi_month_phase(start_dates, end_dates),
    year = format(start_dates, "%Y")
  )

  phase_factor <- factor(phase_vector, levels = unique(phase_vector))

  out_tibble <- tibble::tibble(
    Year = as.integer(format(start_dates, "%Y")),
    Month = factor(format(start_dates, "%B"), levels = month.name),
    Phase = phase_factor,
    start_date = start_dates,
    end_date = end_dates
  ) |>
    dplyr::arrange(start_date)

  # Handle splitting outputs
  if (split_by == 1) {

    return(out_tibble)

  } else {

    n_row <- nrow(out_tibble)

    if (split_by > n_row) {

      split_by = n_row

    }


    seq_row <- seq_len(n_row)

    seq_group <- seq_len(split_by)

    pad_width <- nchar(split_by)

    group_names <-
      paste0("G", stringr::str_pad(seq_group, pad = "0", width =  pad_width))

    groups <-
      cut(seq_row, breaks = split_by, labels = group_names)

    out_tibble_list <-
      out_tibble |>
      dplyr::mutate(group_id = groups) |>
      dplyr::group_by(group_id) |>
      dplyr::group_split() |>
      purrr::map( ~ dplyr::select(.x, -group_id)) |>
      purrr::set_names(group_names)

    return(out_tibble_list)

  }

}
