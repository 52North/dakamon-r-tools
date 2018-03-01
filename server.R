# server DaKaMon Import
library(shiny)
library(DT)

library(rpostgis)
library(httr)
library(rjson)

SOSWebApp <- "http://localhost:8080/52n-sos-webapp/"
verbose <- FALSE

## tools
SOSinsFoI <- function(gmlId, name, lat, lon, super_FoI="unknown") {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
          <ifoi:InsertFeatureOfInterest service=\"SOS\" version=\"2.0.0\"
          xmlns:ifoi=\"http://www.opengis.net/ifoi/1.0\"
          xmlns:gml=\"http://www.opengis.net/gml/3.2\"
          xmlns:xlink=\"http://www.w3.org/1999/xlink\"
          xmlns:sams=\"http://www.opengis.net/samplingSpatial/2.0\"
          xmlns:sf=\"http://www.opengis.net/sampling/2.0\"
          xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.opengis.net/ifoi/1.0 http://52north.org/schema/ifoi/1.0/InsertFeatureOfInterest.xsd http://www.opengis.net/gml/3.2 http://schemas.opengis.net/gml/3.2.1/gml.xsd http://www.opengis.net/samplingSpatial/2.0 http://schemas.opengis.net/samplingSpatial/2.0/spatialSamplingFeature.xsd http://www.opengis.net/sampling/2.0 http://schemas.opengis.net/sampling/2.0/samplingFeature.xsd\">
          <ifoi:featureMember>
          <sams:SF_SpatialSamplingFeature gml:id=\"", gmlId, "\">
          <gml:identifier codeSpace=\"\">", gmlId, "</gml:identifier>
          <gml:name>", name, "</gml:name>
          <sf:type xlink:href=\"http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint\"/>
          <sf:sampledFeature xlink:href=\"", super_FoI, "\"/>
          <sams:shape>
          <gml:Point gml:id=\"", gmlId,"_point\">
          <gml:pos srsName=\"http://www.opengis.net/def/crs/EPSG/0/4326\">", lat, " ", lon, "</gml:pos>
          </gml:Point>
          </sams:shape>
          </sams:SF_SpatialSamplingFeature>
          </ifoi:featureMember>
          </ifoi:InsertFeatureOfInterest>")
}

SOSreqFoI <- function(gmlId) {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
          <sos:GetFeatureOfInterest
          xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
          xmlns:sos=\"http://www.opengis.net/sos/2.0\"
          service=\"SOS\" version=\"2.0.0\" xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd\">
          <sos:featureOfInterest>", gmlId, "</sos:featureOfInterest></sos:GetFeatureOfInterest>")
}
## /tools

server <- function(input, output) {

  source("server_FoI.R", local = TRUE)$value
  
  source("server_data.R", local = TRUE)$value
  
}