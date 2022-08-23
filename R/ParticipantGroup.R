#' A Reference Class for representing a group of consistency test participants
#' 
#' @field participants A list of \code{\link{Participant}}
#' class instances.
#' @importFrom methods new
#' @export ParticipantGroup
#' @exportClass ParticipantGroup

ParticipantGroup <- setRefClass(
  "ParticipantGroup",
  fields = list(participants = "list"),
  methods = list(
    add_participant = function(participant) {
      "Add a passed participant to the participantgroup's list
      of participants. The participant's entry in
      the list is named based on the participant's
      id. Note that if you try to add
      a participant with an id that's identical
      to one of the participants already in the
      participantgroup's list of participants, the
      already existing same-id participant
      is overwritten."
      if (!length(participant$id)) {
        stop(paste0("I was passed a participant without an id. ",
          "You must assign the participant an id before using ",
          "<participantgroup>.add_participant()."
        ))
      }
      p_id <- participant$id
      participants[[p_id]] <<- participant
    },

    add_participants = function(participant_list) {
      "Go through a passed list of Participant instances
      and add each one using the add_participant() method."
      for (p in participant_list) {
        add_participant(p)
      }
      rm(p)
    },

    check_valid_get_twcv_scores = function(
      min_complete_graphemes = 5,
      dbscan_eps = 20,
      dbscan_min_pts = 4,
      max_var_tight_cluster = 150,
      max_prop_single_tight_cluster = 0.6,
      safe_num_clusters = 3,
      safe_twcv = 250,
      complete_graphemes_only = TRUE,
      symbol_filter = NULL
    ) {
    "
    Checks if participants' data are valid based on passed arguments.
    This method aims to identify participants who had too few responses or
    varied their response colors too little, by marking them as invalid.
    Note that there are no absolutely correct values, as what is 'too little
    variation' is highly subjective. You might need to tweak parameters to be
    in line with your project's criteria, especially if you use another color
    space than CIELUV, since the default values are based on what seems
    to make sense in a CIELUV context. If you use the results in a
    research article, make sure to reference synr and specify what parameter
    values you passed to the function.

    This method relies heavily on the DBSCAN algorithm and the package
    'dbscan', and involves calculating a synr-specific 'Total Within-Cluster
    Variance' (TWCV) score. You can find more information, and
    what the parameters here mean, in
    the documentation for the function \\code{validate_get_twcv}. Note
    that DBSCAN clustering and related calculations are performed on
    a per-participant basis, before they are summarized in the data frame
    returned by this method.
    \\subsection{Parameters}{
      \\itemize{
        \\item{\\code{min_complete_graphemes} The minimum number of graphemes
          with complete (all non-NA color) responses that a participant's data
          must have for them to not be categorized as invalid based on
          this criterion. Defaults to 7.
        }
        \\item{\\code{dbscan_eps} Radius of 'epsilon neighborhood' when applying
          (on a per-participant basis) DBSCAN clustering. Defaults to 30.
        }
        \\item{\\code{dbscan_min_pts} Minimum number of points required in the
          epsilon neighborhood for core points (including the core point
          itself). Defaults to 4.
        }
        \\item{\\code{max_var_tight_cluster} Maximum variance for an identified
          DBSCAN cluster to be considered 'tight-knit'. Defaults to 150.
        }
        \\item{\\code{max_prop_single_tight_cluster} Maximum proportion of
          points allowed to be within a single 'tight-knit' cluster (if a
          participant's data exceed this limit, they are classified as
          invalid). Defaults to 0.6.
        }
        \\item{\\code{safe_num_clusters} Minimum number of identified DBSCAN
          clusters (including 'noise' cluster only if it consists of at least
          'dbscan_min_pts' points) that guarantees validity of
          a participant's data if points are 'non-tight-knit'. Defaults to 3.
        }
        \\item{\\code{safe_twcv} Minimum total within-cluster variance (TWCV)
          score that guarantees a participant's data's validity if points are
          'non-tight-knit'. Defaults to 250.
        }
        \\item{\\code{complete_graphemes_only} A logical vector. If TRUE, 
          only data from graphemes that have all non-NA color responses
          are used; if FALSE, even data from graphemes with some NA color
          responses are used. Defaults to TRUE.
        }
        \\item{\\code{symbol_filter} A character vector (or NULL) that specifies
          which graphemes' data to use. Defaults to NULL, meaning data from
          all of the participants' graphemes will be used.
        }
      }
    }

    \\subsection{Returns}{
      A data frame with columns
      \\itemize{
        \\item{\\code{valid} Holds TRUE for participants whose data were
        classified as valid, FALSE for participants whose data were
        classified as invalid.}
        \\item{\\code{reason_invalid} Strings which describe for each
          participant why their data were deemed invalid. Participants
          whose data were classified as valid have empty strings here.
        }
        \\item{\\code{twcv} Numeric column which holds participants'
          calculated TWCV scores (NA for participants who had no/too
          few graphemes with complete responses).
        }
        \\item{\\code{num_clusters} One-element numeric (or NA if there are no/too few
          graphemes with complete responses) vector indicating
          the number of identified clusters counting toward the
          tally compared with 'safe_num_clusters'.
        }
      }
    }
    "
      if (!has_participants()) {
        stop(paste0("Tried to check validity of color responses and get ",
          "TWCV scores for participantgroup without participants. Please ",
          "add participants before calling ",
          "check_valid_get_twcv_scores()."
        ))
      }
      participant_level_val_class <- logical(length(participants))
      participant_level_reason_inv <- numeric(length(participants))
      participant_level_twcv <- numeric(length(participants))
      participant_level_num_clusters <- numeric(length(participants))
      loop_index <- 1
      for (p in participants) {
        p_res_list <- p$check_valid_get_twcv(
          min_complete_graphemes = min_complete_graphemes,
          dbscan_eps = dbscan_eps,
          dbscan_min_pts = dbscan_min_pts,
          max_var_tight_cluster = max_var_tight_cluster,
          max_prop_single_tight_cluster = max_prop_single_tight_cluster,
          safe_num_clusters = safe_num_clusters,
          safe_twcv = safe_twcv,
          complete_graphemes_only = complete_graphemes_only,
          symbol_filter = symbol_filter
        )
        participant_level_val_class[loop_index] <- p_res_list$valid
        participant_level_reason_inv[loop_index] <- p_res_list$reason_invalid
        participant_level_twcv[loop_index] <- p_res_list$twcv
        participant_level_num_clusters[loop_index] <- p_res_list$num_clusters
        loop_index <- loop_index + 1
      }
      all_data_df <- data.frame(
        valid = participant_level_val_class,
        reason_invalid = participant_level_reason_inv,
        twcv = participant_level_twcv,
        num_clusters = participant_level_num_clusters
      )
      return(all_data_df)
    },

    has_participants = function() {
      "Returns TRUE if there is at least one
  participant in the participantgroup's participants list,
  otherwise returns FALSE"
      return(length(participants) > 0)
    },

    get_ids = function() {
      "Returns a character vector with all ids for
  participants associated with the participantgroup."
      return(names(participants))
    },

    get_mean_consistency_scores = function(
      method="euclidean",
      symbol_filter = NULL,
      na.rm = FALSE
    ) {
      "Returns a vector of mean consistency scores for
      participants in the group. If na.rm=FALSE, for each
      participant calculates the mean consistency score if
      all of the participants' graphemes only have response
      colors that are non-NA, otherwise puts an NA value
      for that participant in returned vector. If na.rm=TRUE,
      for each participant calculates the mean consistency score for
      all of the participant's graphemes that only have
      non-NA response colors, while ignoring graphemes
      that have at least one NA response color value. Note that
      for participants whose graphemes ALL have at least one NA
      response color value, an NA is put in the returned vector for
      that participant, regardless of what na.rm is set to.

      If a character vector is passed to symbol_filter, only
      data from graphemes with symbols in the passed vector
      are used when calculating each participant's mean score.

      Use the method argument to specify what kind of color space
      distances should be used when calculating consistency scores
      (usually 'manhattan' or 'euclidean' - see documentation for
      the base R dist function for all options)"
      if (!has_participants()) {
        stop(paste0("Tried to fetch mean numbers of all colored graphemes ",
          "for participantgroup without participants. Please add ",
          "participants before calling get_numbers_all_colored_graphemes()."
        ))
      }
      participant_level_mean_cons <- numeric(length(participants))
      loop_index <- 1
      for (p in participants) {
        p_mean_c_score <- p$get_mean_consistency_score(
          method = method,
          symbol_filter = symbol_filter,
          na.rm = na.rm
        )
        participant_level_mean_cons[loop_index] <- p_mean_c_score
        loop_index <- loop_index + 1
      }
      return(participant_level_mean_cons)
    },

    get_mean_colors = function(
      symbol_filter = NULL,
      na.rm = FALSE
    ) {
      "Returns an nx3 data frame of mean colors for
      participants in the group, where the columns
      represent chosen color space axis 1, 2, and 3, respectively
      (e.g. 'R', 'G', 'B' if 'sRGB' was specified upon participantgroup
      creation).
      
      If na.rm=FALSE, for each
      participant calculates the mean color if
      all of the participants' graphemes only have response
      colors that are non-NA, otherwise puts NA values
      for that participant's row in matrix. If na.rm=TRUE,
      for each participant calculates the mean color
      for all of the participant's valid response colors,
      while ignoring NA response colors. Note that
      for participants whose graphemes ALL have at least one NA
      response color value, an NA is put in the row corresponding to
      that participant, regardless of what na.rm is set to.

      If a character vector is passed to symbol_filter, only
      data from graphemes with symbols in the passed vector
      are used when calculating each participant's mean color." 
      if (!has_participants()) {
        stop(paste0("Tried to fetch mean colors ",
          "for participantgroup without participants. Please add ",
          "participants before calling get_mean_colors()."
        ))
      }
      mean_color_mat <- matrix(
        nrow=length(participants), ncol=3,
        dimnames=list(
          NULL,
          c("color_axis_1_mean", "color_axis_2_mean", "color_axis_3_mean")
        )
      )
      loop_index <- 1
      for (p in participants) {
        mean_color_vec <- p$get_participant_mean_color(
          symbol_filter = symbol_filter,
          na.rm = na.rm
        )
        mean_color_mat[loop_index, ] <- mean_color_vec
        loop_index <- loop_index + 1
      }
      return(data.frame(mean_color_mat))
    },

    get_mean_response_times = function(
      symbol_filter = NULL,
      na.rm = FALSE
    ) {
      "Returns the mean response times, with respect to
      Grapheme instances associated with each participant.
      If na.rm=TRUE, for each participant returns mean response time even
      if there are missing response times. If na.rm=FALSE, returns
      mean response time if there is at least one response time
      value for at least one of the participants' graphemes. If a
      character vector is passed to symbol_filter, only data from
      graphemes with symbols in the passed vector are used when
      calculating each participant's mean response time."
      if (!has_participants()) {
        stop(paste0("Tried to fetch mean response times for ",
          "participantgroup without participants. Please add participants ",
          "before calling get_mean_response_times()."
        ))
      }
      participant_level_resp_times <- numeric(length(participants))
      loop_index <- 1
      for (p in participants) {
        p_time <- p$get_mean_response_time(
          symbol_filter = symbol_filter,
          na.rm = na.rm
        )
        participant_level_resp_times[loop_index] <- p_time
        loop_index <- loop_index + 1
      }
      return(participant_level_resp_times)
    },

    get_numbers_all_colored_graphemes = function(symbol_filter = NULL) {
      "Returns a vector with numbers representing how many
      graphemes with all-valid (non-na) response colors that each
      participant has.  If a character vector is passed to symbol_filter,
      only data connected to graphemes with symbols in the passed vector
      are used."
      if (!has_participants()) {
        stop(paste0("Tried to fetch mean numbers of all colored graphemes ",
          "for participantgroup without participants. Please add ",
          "participants before calling ",
          "get_numbers_all_colored_graphemes()."
        ))
      }
      participant_level_num_allcol <- numeric(
        length(participants)
      )
      loop_index <- 1
      for (p in participants) {
        p_num_all_colored <- p$get_number_all_colored_graphemes(
          symbol_filter = symbol_filter
        )
        participant_level_num_allcol[loop_index] <- p_num_all_colored
        loop_index <- loop_index + 1
      }
      return(participant_level_num_allcol)
    },

    save_plots = function(
      save_dir = NULL,
      file_format='png',
      dpi = 300,
      cutoff_line = FALSE,
      mean_line = FALSE,
      grapheme_size = 2,
      grapheme_angle = 0,
      foreground_color = "black",
      background_color = "white",
      symbol_filter = NULL,
      ...
    ) {
    "Goes through all participants and for each one produces and saves
    a ggplot2 plot that describes the participant's
    grapheme color responses and per-grapheme consistency scores,
    using the ggsave function.

    If a character vector is passed to symbol_filter, only data for graphemes
    with symbols in the passed vector are used.

    If path is not specified, plots are saved to the current
    working directory. Otherwise, plots are saved to the specified
    directory. The file is saved using the specified file_format,
    e. g. JPG (see ggplot2::ggsave documentation for list of
    supported formats), and the resolution specified with
    the dpi argument.

    If cutoff_line=TRUE, each plot will include a blue line that
    indicates the value 135.30, which is the synesthesia cut-off score
    recommended by Rothen, Seth, Witzel & Ward (2013) for the L*u*v
    color space. If mean_line=TRUE, the plot will include a green line
    that indicates the participant's mean consistency score for
    graphemes with all-valid response colors (if the participant
    has any such graphemes). If a vector is passed to symbol_filter,
    this green line represents the mean score
    for ONLY the symbols included in the filter.

    Pass a value to grapheme_size to adjust the size of graphemes
    shown at the bottom of the plot, e. g. increasing the size if
    there's empty space otherwise, or decreasing the size if the
    graphemes don't fit. Similarly, you can use the grapheme_angle
    argument to rotate the graphemes, which might help them fit better.

    Apart from the ones above, all other arguments
    that ggsave accepts (e. g. 'scale') also work with this function, since
    all arguments are passed on to ggsave. "
      if (!has_participants()) {
        stop(paste0("Tried to fetch mean numbers of all colored graphemes ",
          "for participantgroup without participants. Please add ",
          "participants before calling ",
          "get_numbers_all_colored_graphemes()."
        ))
      }
      for (p in participants) {
        p$save_plot(
          save_dir = save_dir, 
          file_format = file_format,
          dpi = dpi,
          cutoff_line = cutoff_line,
          mean_line = mean_line,
          grapheme_size = grapheme_size,
          grapheme_angle = grapheme_angle,
          foreground_color = foreground_color,
          background_color = background_color,
          symbol_filter = symbol_filter,
          ...
        )
      }
    }
  )
)
