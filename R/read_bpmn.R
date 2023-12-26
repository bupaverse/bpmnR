


#' Read bpmn file
#'
#' @param bmpn_file File name
#'
#' @return
#' @export
#'
read_bpmn <- function(bmpn_file) {

  #
  pm4py_ret <- reticulate::import("pm4py", convert = T)

  pm4py_ret$read_bpmn(file_path = bmpn_file) -> bpmntmp


  reticulate::iterate(bpmntmp$get_nodes(), function(p) ensure_str(p$name)) -> nodes_name
  reticulate::iterate(bpmntmp$get_nodes(), function(p) ensure_str(p$id)) -> nodes_id
  reticulate::iterate(bpmntmp$get_nodes(), function(p) (class(p))) -> nodes_class
  reticulate::iterate(bpmntmp$get_nodes(), function(p) if(stringr::str_detect(ensure_str(class(p)), "Gateway"))
    p$get_gateway_direction()) -> nodes_directions

  tibble(name = nodes_name, id =  nodes_id, nodes_class, gatewayDirection = nodes_directions) %>%
    mutate(class = stringr::str_remove(map_chr(nodes_class, ~.x[[1]]), "pm4py.objects.bpmn.obj."),
           gatewayDirection = map(gatewayDirection, as.character)) %>%
    tidyr::unnest(gatewayDirection, keep_empty = T) %>%
    mutate(gatewayDirection = stringr::str_remove(gatewayDirection, "Direction."),
           gatewayType = class) %>%
    select(-nodes_class) -> nodes


  # reticulate::iterate(bpmntmp$get_nodes(), function(p) if(stringr::str_detect(pm4py:::ensure_str(class(p)), "Gateway"))
  # 	p$get_out_arcs()) -> nodes_id

  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$get_name())) -> arcs_name
  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$get_id())) -> arcs_id
  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$source$name)) -> arcs_from_name
  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$source$id)) -> arcs_from_id

  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$target$name)) -> arcs_to_name
  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$target$id)) -> arcs_to_id

  tibble(id = arcs_id, name = arcs_name, sourceRef = arcs_from_id, targetRef = arcs_to_id) -> flows


  suppressWarnings({
    nodes %>%
      mutate(class =  forcats::fct_recode(class, "startEvent" = "StartEvent",
                                          "endEvent" = "EndEvent",
                                          "endEvent" = "NormalEndEvent",
                                          "task" = "Task",
                                          "exclusiveGateway" = "ExclusiveGateway",
                                          "inclusiveGateway" = "InclusiveGateway",
                                          "parallelGateway" = "ParallelGateway")) %>%
      mutate(gatewayDirection = forcats::fct_recode(gatewayDirection,
                                                    "Converging" = "CONVERGING",
                                                    "Diverging" = "DIVERGING")) %>%
      select(-gatewayType) -> nodes

  })

  events <- dplyr::filter(nodes, class %in% c("startEvent", "endEvent")) %>% dplyr::rename(objectType = class)
  nodes <- dplyr::filter(nodes, !class %in% c("startEvent", "endEvent")) %>% dplyr::rename(objectType = class)
  flows <- dplyr::mutate(flows, objectType = "sequenceFlow") %>%
    mutate(id = paste0("id_", id))

  create_bpmn(nodes, flows, events)

}
