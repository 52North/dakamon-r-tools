# DaKaMon - r-tools

## Introduction

DaKaMon r-tools is software supporting a system for storing hydrological measurements in a structured database and was developed within the ["Monitoringprogramm für prioritäre Stoffe zur Ableitung deutschlandweiter differenzierter Emissionsfaktoren zur Bilanzierung der Stoffeinträge aus kommunalen Kläranlagen"](https://isww.iwg.kit.edu/607_2201.php).

See https://github.com/52north/dakamon for more details.

## License

- GPLv2 (see LICENSE.md)
- see NOTICE.md for used libraries

## Version - changelog

- 2.3 - 23.12.2020

  - Fix decimal separator bug
  - Clean-up configuration files
  - Fix failing measurement overwriting
  - Minor comment updates
  - Improve feedback on already present measurements
  - Fix typo in comment in conf.R
  - Add workaround for problems with unit "%"
  - Fix error when adding values for parameter without BG and NG
  - Fix not matching identifier
  - [Complete diff](../../compare/v2.2...v2.3)

- 2.2 - 08.12.2020

  - Improve identifier validation
  - Check required columns for empty cells
  - Check probe$labName for identifier validity
  - [Complete diff](../../compare/v2.1...v2.2)

- 2.1 - 26.11.2020

  - Identifier validation
  - Code and code layout cleaning
  - Remove ")" when updating feature of interests during location upload
  - Improve stability when uploading data
  - Fix problem with missing temporal folder for data upload
  - Fix problem with identifying data upload result
  - Add license header
  - Improve stability of getLiteraturReferences()
  - Use identifier and not id when requesting literature data
  - Add [NOTICE.md](NOTICE.md)
  - Add [LICENSE.md](LICENSE.md)
  - Publish to 52North github organisation

- 2.0 - 19.10.2018

  - Refactoring
  - All required features implemented

- 1.0 - 16.06.2018

## Contact

- [Gräler, Benedikt](mailto:b.graeler@52north.org)


## Developer

- [Bredel, Henning](mailto:h.bredel@52north.org)
- [Hollmann, Carsten](mailto:c.hollmann@52north.org)
- [Jürrens, Eike Hinderk](mailto:e.h.juerrens@52north.org)
