## connect with SOS DB:

# install.packages("rpostgis")
library("rpostgis")
library("httr")

db <- dbConnect("PostgreSQL", host="localhost", dbname="sos", user="postgres", password="postgres", port="5432")
db

ls(db)

# overwriteDB <- function() FALSE

############################################
##############                ##############
############## Initial set-up ##############
##############                ##############
############################################

## use separate table to store FoI attributes, as well as a separate table to store column headers and UoM
# dbSendQuery(db, "DROP TABLE foidata;")
dbSendQuery(db, 
  "CREATE TABLE foidata (featureofinterestid bigint PRIMARY KEY,
   ID character varying(255) NOT NULL UNIQUE,
   Name character varying(255) NOT NULL UNIQUE,
   CONSTRAINT foidata_featureofinterestid_fkey FOREIGN KEY (featureofinterestid)
   REFERENCES featureofinterest (featureofinterestid) MATCH SIMPLE
   ON UPDATE CASCADE ON DELETE CASCADE);")

# dbSendQuery(db, "DROP TABLE foidatametadata;")
dbSendQuery(db, 
            "CREATE TABLE foidatametadata (columnid character varying(255) PRIMARY KEY,
   dede character varying(255) NOT NULL,
   uom bigint);")

dbSendQuery(db, "INSERT INTO foidatametadata VALUES ('ID', 'ID')")
dbSendQuery(db, "INSERT INTO foidatametadata VALUES ('Name', 'Name')")

# insert table 'observablepropertyrelation' - analogously table 'featurerelation'
# dbSendQuery(db, "DROP TABLE observablepropertyrelation")
dbSendQuery(db, "CREATE TABLE observablepropertyrelation (
                 parentobservablepropertyid bigint NOT NULL,
                 childobservablepropertyid bigint NOT NULL,
                 CONSTRAINT observablepropertyrealtion_pkey PRIMARY KEY (childobservablepropertyid, parentobservablepropertyid),
                 CONSTRAINT observablepropertychildfk FOREIGN KEY (childobservablepropertyid)
                    REFERENCES observableproperty (observablepropertyid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE NO ACTION,
                 CONSTRAINT observablepropertyparentfk FOREIGN KEY (parentobservablepropertyid)
                    REFERENCES observableproperty (observablepropertyid) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE NO ACTION)")

##########################################
##############              ##############
##############  dummy data  ##############
##############              ##############
##########################################

foi_df <- read.csv("Daten/FoI_sample.csv", sep = ";", header = FALSE, stringsAsFactors = FALSE)
foi_header <- foi_df[1,]
if (!("Name" %in% foi_header))
  stop("A name is mandatory for each feature of interest; please supply a column 'Name'.")

foi_uom <- foi_df[2,]
foi_data <- read.csv("Daten/FoI_sample.csv", sep = ";", header = FALSE, skip = 2, stringsAsFactors = FALSE)

foi_empty_cols <- apply(foi_data, 2, function(x) all(is.na(x)))
foi_header <- as.character(foi_header[,!foi_empty_cols])

comp_header <- outer(foi_header, foi_header, "==")
if(any(comp_header[upper.tri(comp_header)]))
  stop("Column names need to be unique.")

foi_uom <- as.character(foi_uom[,!foi_empty_cols])
foi_data <- foi_data[,!foi_empty_cols]

colnames(foi_data) <- foi_header

###############################################
##############                   ##############
##############  Insert Feautres  ##############
##############                   ##############
###############################################

# any parent features that need to be inserted before? Those with empty or missing super_FoI column
par_foi <- is.na(foi_data$super_FoI) | nchar(foi_data$super_FoI) == 0

if (any(par_foi)) {
  for (sfoi in which(par_foi)) {# sfoi <- 5
    # check whether the feature is already in the DB
    if (nrow(dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                   foi_data[sfoi,]$ID,"'"))) > 0 & !overwriteDB()) {
      next;
    } else {
      # update FoI (keeping the DB_identifier)
    }
    # insert a new feature
    
    insReq <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
                     <ifoi:InsertFeatureOfInterest service=\"SOS\" version=\"2.0.0\"
                     xmlns:ifoi=\"http://www.opengis.net/ifoi/1.0\"
                     xmlns:gml=\"http://www.opengis.net/gml/3.2\"
                     xmlns:xlink=\"http://www.w3.org/1999/xlink\"
                     xmlns:sams=\"http://www.opengis.net/samplingSpatial/2.0\"
                     xmlns:sf=\"http://www.opengis.net/sampling/2.0\"
                     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.opengis.net/ifoi/1.0 http://52north.org/schema/ifoi/1.0/InsertFeatureOfInterest.xsd http://www.opengis.net/gml/3.2 http://schemas.opengis.net/gml/3.2.1/gml.xsd http://www.opengis.net/samplingSpatial/2.0 http://schemas.opengis.net/samplingSpatial/2.0/spatialSamplingFeature.xsd http://www.opengis.net/sampling/2.0 http://schemas.opengis.net/sampling/2.0/samplingFeature.xsd\">
                     <ifoi:featureMember>
                     <sams:SF_SpatialSamplingFeature gml:id=\"", foi_data[sfoi,]$ID, "\">
                     <gml:identifier codeSpace=\"\">", foi_data[sfoi,]$ID, "</gml:identifier>
                     <gml:name>", foi_data[sfoi,]$Name, "</gml:name>
                     <sf:type xlink:href=\"http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint\"/>
                     <sf:sampledFeature xlink:href=\"unknown\"/>
                     <sams:shape>
                     <gml:Point gml:id=\"", foi_data[sfoi,]$ID,"_point\">
                                          <gml:pos srsName=\"http://www.opengis.net/def/crs/EPSG/0/4326\">", foi_data[sfoi,]$lat, " ", foi_data[sfoi,]$lat, "</gml:pos>
                                      </gml:Point>
                                  </sams:shape>
                              </sams:SF_SpatialSamplingFeature>
                          </ifoi:featureMember>
                      </ifoi:InsertFeatureOfInterest>")
    
    insMsg <- rawToChar(httr::POST("http://localhost:8080/52n-sos-webapp/service", 
                                   body = insReq, verbose(), content_type_xml(), accept_xml())$content)
    message(insMsg)
  }
}

# insert remaining FoI
for (foi in which(!par_foi)) {# foi <- 6
  # check whether the feature is already in the DB
  if (nrow(dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier = '", 
                                 foi_data[foi,]$ID,"'"))) > 0 & !overwriteDB()) {
    next;
  } else {
    # update FoI (keeping the DB_identifier)
  }
  # insert a new feature
  
  insReq <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>
                      <ifoi:InsertFeatureOfInterest service=\"SOS\" version=\"2.0.0\"
                          xmlns:ifoi=\"http://www.opengis.net/ifoi/1.0\"
                          xmlns:gml=\"http://www.opengis.net/gml/3.2\"
                          xmlns:xlink=\"http://www.w3.org/1999/xlink\"
                          xmlns:sams=\"http://www.opengis.net/samplingSpatial/2.0\"
                          xmlns:sf=\"http://www.opengis.net/sampling/2.0\"
                          xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.opengis.net/ifoi/1.0 http://52north.org/schema/ifoi/1.0/InsertFeatureOfInterest.xsd http://www.opengis.net/gml/3.2 http://schemas.opengis.net/gml/3.2.1/gml.xsd http://www.opengis.net/samplingSpatial/2.0 http://schemas.opengis.net/samplingSpatial/2.0/spatialSamplingFeature.xsd http://www.opengis.net/sampling/2.0 http://schemas.opengis.net/sampling/2.0/samplingFeature.xsd\">
                          <ifoi:featureMember>
                              <sams:SF_SpatialSamplingFeature gml:id=\"", foi_data[foi,]$ID, "\">
                                  <gml:identifier codeSpace=\"\">", foi_data[foi,]$ID, "</gml:identifier>
                                  <gml:name>", foi_data[foi,]$Name, "</gml:name>
                                  <sf:type xlink:href=\"http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint\"/>
                                  <sf:sampledFeature xlink:href=\"", foi_data[foi,]$super_FoI, "\"/>
                                  <sams:shape>
                                      <gml:Point gml:id=\"", foi_data[foi,]$ID,"_point\">
                                          <gml:pos srsName=\"http://www.opengis.net/def/crs/EPSG/0/4326\">", foi_data[foi,]$lat, " ", foi_data[foi,]$lat, "</gml:pos>
                                      </gml:Point>
                                  </sams:shape>
                              </sams:SF_SpatialSamplingFeature>
                          </ifoi:featureMember>
                      </ifoi:InsertFeatureOfInterest>")
  
  insMsg <- rawToChar(httr::POST("http://localhost:8080/52n-sos-webapp/service", 
                                 body = insReq, verbose(), content_type_xml(), accept_xml())$content)
  message(insMsg)
}


# pre-process: add new units
regUoMs <- dbGetQuery(db, paste0("SELECT unit FROM unit"))
unqUoMs <- unique(foi_uom[-1])
if (nrow(regUoMs) > 0) {
  regUoMs <- regUoMs[,1]
  misUoMs <- unqUoMs[which(sapply(unqUoMs, function(x) is.na(match(x, regUoMs))) & nchar(unqUoMs) > 0)]
} else {
  misUoMs <- unqUoMs
}

for (uom in misUoMs) {
  dbSendQuery(db, paste0("INSERT INTO unit (unitid, unit) VALUES (nextval('unitid_seq'), '", uom, "');"))
}

### add new columns
regCols <- dbGetQuery(db, paste0("SELECT dede FROM foidatametadata"))[,1]
misCols <- which(sapply(foi_header, function(x) is.na(match(x, regCols))))

if (length(misCols > 0)) { 
  for (i in 1:length(misCols)) {
    colId <- sprintf("col%03d", i + length(regCols) )
    dbColumn(db, "foidata", colId, "add", 
             coltype = switch(class(foi_data[,misCols[i]]),
                              integer = "numeric",
                              numeric = "numeric",
                              character = "character varying(255)"))
    
    # look-up UoM id
    unitId <- NULL
    if (nchar(foi_uom[misCols[i]]) > 0) {
      unitId <- dbGetQuery(db, paste0("SELECT unitid FROM unit WHERE unit = '", foi_uom[misCols[i]], "'"))[1,1]
    }
    
    if (is.null(unitId)) {
      dbSendQuery(db, paste0("INSERT INTO foidatametadata (columnid, dede)
                  VALUES ('", paste(colId, foi_header[misCols[i]], sep="', '"),"')"))
    } else {
      dbSendQuery(db, paste0("INSERT INTO foidatametadata (columnid, dede, uom)
                  VALUES ('", paste(colId, foi_header[misCols[i]], unitId, sep="', '"),"')"))
    }
  }
}

# feed data row-wise
for (i in 1:nrow(foi_data)) {
  nonEmpty <- which(!is.na(foi_data[i,]))
  if (all(!nonEmpty)) next;

  # map csv-header to DB header via foidatametadata
  foi_db_col_ids <- dbGetQuery(db, paste0("SELECT columnid, dede FROM foidatametadata WHERE dede IN ('", 
                                          paste(foi_header[nonEmpty], collapse="', '"),"')"))
  
  # mind the ordering
  foi_db_col_ids <- foi_db_col_ids[match(foi_header[nonEmpty], foi_db_col_ids$dede), "columnid"]
  
  # find the FoI idntifier
  foi_db_id <- dbGetQuery(db, paste0("SELECT featureofinterestid FROM featureofinterest WHERE identifier ='", 
                                     foi_data$ID[i],"'"))
  
  # check whether the FoI has already some data
  if (nrow(dbGetQuery(db, paste0("SELECT id FROM foidata WHERE featureofinterestid = ", foi_db_id))) > 0) {
    if(overwriteDB()) {
      dbSendQuery(db, paste0("UPDATE foidata SET ", 
                             paste(foi_db_col_ids, foi_data[i, nonEmpty], sep = " = '", collapse = "', "),
                             "' WHERE featureofinterestid = ", foi_db_id, ";"))
    } else {
      next()
    }
  } else {
    dbSendQuery(db, paste0("INSERT INTO foidata ( featureofinterestid, ", paste(foi_db_col_ids, collapse=", "), ") ",
                           "VALUES ('", foi_db_id, "', '", paste(foi_data[i, nonEmpty], collapse="', '"), "')"))
  }
}

# restrictions on CSV:
# Must contain columns ID, Name, lat, lon, with unique values!
# coordiantes must be in WGS84, i.e. epsg:4326