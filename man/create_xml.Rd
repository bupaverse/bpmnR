% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_xml2.R
\name{create_xml}
\alias{create_xml}
\alias{create_xml.bpmn}
\title{Create XML document from BPMN object.}
\usage{
create_xml(bpmn, ...)

\method{create_xml}{bpmn}(bpmn, ...)
}
\arguments{
\item{bpmn}{A BPMN object as a list of data.frames for the BPMN elements.}

\item{...}{Additional arguments passed to methods.}
}
\value{
An XML document for the XML-based interchange format for the BPMN process.
}
\description{
This creates an XML document based on a BPMN object.
}
\section{Methods (by class)}{
\itemize{
\item \code{create_xml(bpmn)}: Create xml

}}
\examples{

library(dplyr)
nodes <- tibble(id = "task", name = "Task name", objectType = "task",
gatewayDirection = NA)
events <- tibble(id = c("start","end"), name = c("Start event","End event"),
objectType = c("startEvent","endEvent"))
flows <- tibble(id = c("flow1","flow2"), name = c("flow1","flow2"),
sourceRef = c("start","task"), targetRef = c("task","end"),
objectType = c("sequenceFlow","sequenceFlow"))
model <- create_bpmn(nodes, flows, events)
create_xml(model)

}
\author{
Alessio Nigro
}
