% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_bpmn.R
\name{write_bpmn}
\alias{write_bpmn}
\alias{write_bpmn.bpmn}
\title{Write XML or HTML to disk.}
\usage{
write_bpmn(bpmn, file, ..., options = "format", encoding = "UTF-8")

\method{write_bpmn}{bpmn}(bpmn, file, ..., options = "format", encoding = "UTF-8")
}
\arguments{
\item{bpmn}{A BPMN object as a list of data.frames for the BPMN elements and
an XML document for the XML-based interchange format for the BPMN process.}

\item{file}{Path to file or connection to write to.}

\item{...}{Additional arguments passed to methods.}

\item{options}{default: \sQuote{format}. Zero or more of
\Sexpr[results=rd]{xml2:::describe_options(xml2:::xml_save_options())}}

\item{encoding}{The character encoding to use in the document. The default
encoding is \sQuote{UTF-8}. Available encodings are specified at
<http://xmlsoft.org/html/libxml-encoding.html#xmlCharEncoding>.}
}
\description{
This writes out both XML and normalised HTML. The default behavior will
output the same format which was read. If you want to force output pass
`option = "as_xml"` or `option = "as_html"` respectively.
}
\author{
Alessio Nigro
}
