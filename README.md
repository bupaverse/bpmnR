# bpmnR

- [x] task 1: make sure package can be installed
- [ ] task 2: what inputs does __create_bpmn()__ expect
- [ ] task 3: 

## Task 2
    minimal_subset_attributes_list <-
      list(
        tasks = c("id", "name"),
        sequenceFlows = c("id", "name", "sourceRef", "targetRef"),
        gateways = c("id", "name", "gatewayType", "gatewayDirection"),
        startEvent = c("id", "name"),
        endEvent = c("id", "name")
      )
