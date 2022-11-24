# ============================== MAIN FUNCTION =================================

#' Create BPMN object.
#'
#' This creates a BPMN object by specifying a set of tasks, sequence flows,
#' gateways, and a start and end event.
#'
#' @param tasks A data.frame of all tasks and their attributes.
#' @param sequenceFlows A data.frame of all sequence flows and their attributes.
#' @param gateways A data.frame of all gateways and their attributes.
#' @param startEvent A data.frame containing the start event and its attributes.
#' @param endEvent A data.frame containing the end event and its attributes.
#' @param ... Additional arguments passed to methods.
#'
#' @return A BPMN object as a list of data.frames for the BPMN elements and an
#'   XML document for the XML-based interchange format for the BPMN process.
#'
#' @author Alessio Nigro
#'
#' @importFrom purrr imap
#' @importFrom purrr keep
#' @importFrom purrr transpose
#' @importFrom purrr map
#' @importFrom purrr map_lgl
#' @importFrom purrr compose
#' @importFrom purrr as_mapper
#' @importFrom assertive assert_is_data.frame
#' @importFrom assertive is_non_empty
#' @importFrom knitr combine_words
#'
#' @export
create_bpmn <-
  function(tasks,
           sequenceFlows,
           gateways,
           startEvent,
           endEvent,
           ...) {
    # Stores arguments of create_bpmn as a list of data.frames
    bpmn <- as.list(environment())

    # Checks if arguments are data.frames
    assert_is_data.frame(tasks)
    assert_is_data.frame(sequenceFlows)
    assert_is_data.frame(gateways)
    assert_is_data.frame(startEvent)
    assert_is_data.frame(endEvent)

    # Defines every data structure that can be changed
    singular_of_bpmn_elements <- list(
      tasks = "task",
      sequenceFlows = "sequenceFlow",
      gateways = "gateway",
      startEvent = "startEvent",
      endEvent = "endEvent"
    )
    minimal_subset_attributes_list <-
      list(
        tasks = c("id", "name"),
        sequenceFlows = c("id", "name", "sourceRef", "targetRef"),
        gateways = c("id", "name", "gatewayType", "gatewayDirection"),
        startEvent = c("id", "name"),
        endEvent = c("id", "name")
      )
    elements_empty_allowed <- c("gateways")
    attributes_to_factors <- c("gatewayType", "gatewayDirection")

    # Checks for empty data.frames
    bpmn %>%
      .check.for.empty.data.frames(elements_empty_allowed = elements_empty_allowed)

    # Checks per BPMN element if required attributes are present
    bpmn %>%
      .check.for.minimal.subset.attributes(
        minimal_subset_attributes_list = minimal_subset_attributes_list,
        singular_of_bpmn_elements = singular_of_bpmn_elements
      )

    # Converts missing values in data.frames to empty string
    for (element in names(bpmn)) {
      bpmn[[element]][is.na(bpmn[[element]])] <- ""
    }

    # Converts all values to character type
    for (element in names(bpmn)) {
      bpmn[[element]][] <- sapply(bpmn[[element]], as.character)
    }

    # Converts certain attributes to a factor
    for (element in names(bpmn)) {
      for (attribute in names(bpmn[[element]])) {
        if (attribute %in% attributes_to_factors) {
          bpmn[[element]][, attribute] <-
            as.factor(bpmn[[element]][, attribute])
        }
      }
    }

    # Sets class attribute to "bpmn"
    class(bpmn) <- "bpmn"

    # Creates XML document from BPMN object and attaches this XML document to the BPMN object
    bpmn[["xml"]] <- create_xml(bpmn)

    # Prints BPMN object without the XML document
    print(bpmn, view_xml = FALSE)

    return(bpmn)
  }

# ============================= HELPER FUNCTIONS ===============================

# Gives warning message without stopping execution
.give.warning <- purrr::compose(message, paste)

# Stops execution and gives a simple error message
.stop.script <- purrr::compose(stop, simpleError, paste)

# Prints output based on elements that are allowed to be empty or not
.print.output <-
  function(bpmn_element,
           message_string,
           elements_empty_allowed) {
    if (bpmn_element %in% elements_empty_allowed) {
      .give.warning(bpmn_element, message_string)
      cat("\n")
    } else {
      .stop.script(bpmn_element, message_string)
    }
  }

# Prints message about the BPMN elements
.print.message <-
  function(bpmn_lgl,
           message_string,
           elements_empty_allowed) {
    names(bpmn_lgl)[bpmn_lgl] %>%
      map(~ .print.output(.x, message_string, elements_empty_allowed))
  }

# Compares attributes available in the data with a minimal subset of attributes
.compare.attributes <-
  function(bpmn_attributes,
           bpmn_element,
           minimal_subset_attributes_list,
           singular_of_bpmn_elements) {
    minimal_subset_attributes <-
      minimal_subset_attributes_list[[bpmn_element]]
    list_of_logicals <-
      minimal_subset_attributes %in% bpmn_attributes
    if (!(all(list_of_logicals))) {
      errors_attributes <-
        combine_words(minimal_subset_attributes[which(!list_of_logicals)])
      if (sum(!list_of_logicals) == 1) {
        error_message <- " is needed as an attribute of the BPMN element "
      } else {
        error_message <- " are needed as attributes of the BPMN element "
      }
      .stop.script(errors_attributes,
                   error_message,
                   singular_of_bpmn_elements[[bpmn_element]],
                   ".",
                   sep = "")
    }
  }

# Checks for empty data.frames
.check.for.empty.data.frames <-
  function(bpmn, elements_empty_allowed) {
    retrieve_empty_data_frames <- as_mapper(~ nrow(.x) == 0)
    bpmn %>%
      map_lgl(retrieve_empty_data_frames) %>%
      .print.message("is an empty data.frame.", elements_empty_allowed = elements_empty_allowed)
  }

# Checks per BPMN element if required attributes are present
.check.for.minimal.subset.attributes <-
  function(bpmn,
           minimal_subset_attributes_list,
           singular_of_bpmn_elements) {
    bpmn %>%
      map(~ names(.x)) %>%
      keep(is_non_empty) %>%
      imap(
        ~ .compare.attributes(
          .x,
          .y,
          minimal_subset_attributes_list,
          singular_of_bpmn_elements
        )
      )
  }
