packages <- c('shiny','DT','shinyjs','httr','rjson','RPostgreSQL','pool','readr', 'stringi')

for (package in packages) {
  info <- paste0("| ", packageDescription(package, fields = 'Package'), " | ", packageDescription(package, fields = 'Version'), " | ", packageDescription(package, fields = 'License'), " |")
  print(info)
}

file.show(system.file("LICENSE",package="stringi"))