# server DaKaMon Import
library(shiny)
library(DT)
library(httr)
library(rjson)


SOSWebApp <- "http://localhost:8080/52n-sos-webapp/"
verbose <- TRUE
BGencode <- 0
BGchar <- "< BG"
BGlabel <- "Bestimmungsgrenze" # label in DB
feederPath <- "~/GitRepos/sos-importer/feeder/target/52n-sos-importer-feeder-bin.jar"
stndTime <- "T12:00:00+00:00"
adminConf <- authenticate("a","a")
reqColFoI <- list(id="ID", # also checks whether it is unique
                  name="Name",
                  super_FoI="Stammanlage",
                  lat="lat",
                  lon="lon")
reqColData <- list(id="ID", 
                   probeId = "Proben-Nr",
                   date="Datum")

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

SOSreqObs <- function(FoI="http://www.52north.org/test/featureOfInterest/9",
                      obsProp="http://www.52north.org/test/observableProperty/9_3",
                      phenTime="2012-07-31T17:45:15+00:00") {
  options(digits.secs = 3)
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
          <sos:GetObservation
              xmlns:sos=\"http://www.opengis.net/sos/2.0\"
              xmlns:fes=\"http://www.opengis.net/fes/2.0\"
              xmlns:gml=\"http://www.opengis.net/gml/3.2\"
              xmlns:swe=\"http://www.opengis.net/swe/2.0\"
              xmlns:xlink=\"http://www.w3.org/1999/xlink\"
              xmlns:swes=\"http://www.opengis.net/swes/2.0\"
              xmlns:sosrf=\"http://www.opengis.net/sosrf/1.0\"
              xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" service=\"SOS\" version=\"2.0.0\" xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd\">
              <sos:observedProperty>", obsProp, "</sos:observedProperty>
              <sos:temporalFilter>
                <fes:TEquals><fes:ValueReference>phenomenonTime</fes:ValueReference>
                  <gml:TimeInstant gml:id=\"ti_1\">
                    <gml:timePosition>", phenTime, "</gml:timePosition>
                  </gml:TimeInstant>
                </fes:TEquals>
              </sos:temporalFilter>
              <sos:featureOfInterest>", FoI, "</sos:featureOfInterest>    
          </sos:GetObservation>")
}

SOSdelObsByID <- function(obsId) {
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
           <sosdo:DeleteObservation
         xmlns:sosdo=\"http://www.opengis.net/sosdo/1.0\" version=\"2.0.0\" service=\"SOS\">
           <sosdo:observation>", obsId, "</sosdo:observation>
           </sosdo:DeleteObservation>")
}

SOScacheUpdate <- function(gmlId=NULL, wait=0.5, conf=adminConf, verbose=FALSE) {
  POST(url = paste0(SOSWebApp, "admin/cache/reload"), 
       config=conf, body="a")
  
  if (!is.null(gmlId)) {
    reqMsg <- rawToChar(POST(paste0(SOSWebApp, "service"), 
                                   body = SOSreqFoI(gmlId),
                                   content_type_xml(), accept_json())$content)
    
    while (is.null(fromJSON(reqMsg)$exceptions)) {
      if (verbose)
        message(foi_data[sfoi,]$ID)
      
      Sys.sleep(wait)
      reqMsg <- rawToChar(POST(paste0(SOSWebApp, "service"), 
                                     body = SOSreqFoI(gmlId), # foi_data[sfoi,]$ID
                                     content_type_xml(), accept_json())$content)
    }
  } else {
    Sys.sleep(wait)
  }
}
  
## /tools

server <- function(input, output) {

  source("server_FoI.R", local = TRUE)$value
  
  source("server_data.R", local = TRUE)$value
  
}