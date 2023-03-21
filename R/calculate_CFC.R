# # Control Flow Complexity (CFC) -------------------------------------------
# readRDS("bpmn.Rds") -> tmp
#' Title
#'
#' @param bpmn
#'
#' @return
#' @export
#'
#' @examples
calculate_CFC <- function(bpmn) {

  seqs <- bpmn$sequenceFlows
  gw <- bpmn$gateways

  gw_types <- gw$gatewayType %>% unique()
  gw_ids <- gw %>% filter(gatewayDirection == "diverging") %>% distinct(gatewayType, id)

  seqs_ids <- seqs %>% select(sourceRef)
  gateways_as_source <- left_join(gw_ids, seqs_ids, by = c("id" = "sourceRef"))

  #cat("left_join of gateway id's and sequenceFlows sourceRef (unique)", sep = "\n")

  output <- list_along(gw_types)

  for(i in 1:length(gw_types))

  if (gw_types[i] == "ExclusiveGateway") {
    n_splits <- gateways_as_source %>%
      filter(gatewayType == gw_types[i]) %>%
      group_by(id) %>% count()
    output[i] <- n_splits %>% pull(n) %>% sum()
  }

  else if (gw_types[i] == "InclusiveGateway") {
    n_splits <- gateways_as_source %>%
      filter(gatewayType == gw_types[i]) %>%
      group_by(id) %>%
      count()
    output[i] <- n_splits %>% mutate(cfc_OR = 2**n - 1) %>% pull(cfc_OR) %>% sum()
    # print(paste0("2^n - 1: ", "\n"))
    # print(n_splits %>% mutate(cfc_OR = 2**n - 1))
  }

  else if (gw_types[i] == "ParallelGateway") {
    output[i] <- gateways_as_source %>%
      filter(gatewayType == gw_types[i]) %>%
      n_distinct()
  }

  return(sum(unlist(output)))
}

# gw <- tmp$gateways
# seqs <- tmp$seqs
#
# gw <- gw %>% mutate(gatewayType = stringr::str_replace(gatewayType, pattern = "E", replacement = "e"))
#
# gw %>% rbind(gw %>% mutate(gatewayType = "inclusiveGateway")) %>%
#   rbind(gw %>% mutate(gatewayType = "parallelGateway")) -> art_gateways
#
# # split by gatewayType
# gateways_list <- art_gateways %>% split(art_gateways$gatewayType)
#
# map(gateways_list, ~CFC(.x, seqs))
#
# #





# gateways_list$exclusiveGateway %>% filter(gatewayDirection == "diverging") %>% nrow()
# tmp %>% glimpse()
# tmp
#
# ## def. 8: XOR-split
# gw <- tmp$gateways
# gw <- gw %>% mutate(gatewayType = stringr::str_replace(gatewayType, pattern = "E", replacement = "e"))
# gw %>% filter(gatewayDirection == "diverging") %>% nrow()
#
# gw %>% filter(gatewayDirection == "diverging") %>% n_distinct()
# ## def. 9: OR-split (=inclusiveGateway?)
# gw %>% filter(gatewayDirection == "diverging",
#               gatewayType %in% c("exclusiveGateway", "inclusiveGateway")) %>% group_by(gatewayType) %>%
#   mutate(n_splits = ifelse(gateway))






