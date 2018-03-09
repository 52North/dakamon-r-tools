#
#  build via
#
#    docker build -f docker/Dockerfile -t shiny-server:dakamon .
#
#  from repository root
FROM rocker/shiny:latest
MAINTAINER <a href="mailto:e.h.juerrens@52north.org">J&uuml;rrens, Eike Hinderk</a>

ENV JAVA_HOME /usr/lib/jvm/default-java

EXPOSE 3838

ADD docker/shiny-server.sh /usr/bin/shiny-server.sh

WORKDIR /tmp

# install sos-importer
RUN chmod +x /usr/bin/shiny-server.sh &&\
    apt-get update &&\
    apt-get --assume-yes upgrade &&\
    apt-get dist-upgrade --assume-yes &&\
    apt-get install --assume-yes --no-install-recommends openjdk-8-jdk maven git
RUN git clone https://github.com/EHJ-52n/arctic-sea.git &&\
    cd arctic-sea &&\
    git fetch --all &&\
    mvn install &&\
    cd .. &&\
    git clone https://github.com/EHJ-52n/sos-importer.git &&\
    cd sos-importer &&\
    mvn package -e -Pcheck
RUN mkdir -pv /usr/local/52n &&\
    mv /tmp/sos-importer/feeder/target/52n-sos-importer-feeder-bin.jar /usr/local/52n/ &&\
    rm -rf /tmp/sos-importer &&\
    apt-get purge git maven openjdk-8-jdk --assume-yes &&\
    apt-get install openjdk-8-jre --assume-yes --no-install-recommends &&\
    apt-get autoremove --purge --assume-yes && apt-get clean --assume-yes &&\
    rm -rf /var/cache/apt/archives &&\
    rm -rf /root/.m2 &&\
    rm -rf /var/lib/apt/lists/* &&\
    rm -rf /opt/shiny-server/samples &&\
    rm -rfv /srv/shiny-server/*
# install R packages as sudo or user shiny
RUN apt-get update &&\
    apt-get install --assume-yes --no-install-recommends libssl-dev # libgeos-dev libpq-dev
# RUN R -e "install.packages(c('rpostgis'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('DT', 'httr', 'rjson', 'dplyr'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('shinyjs'), repos='https://cran.rstudio.com/')"

RUN java -jar /usr/local/52n/52n-sos-importer-feeder-bin.jar

ADD DaKaMon_importer/*.* /srv/shiny-server/DaKaMon_importer/

ADD DaKaMon_viewer/*.* /srv/shiny-server/DaKaMon_viewer/

ADD docker/shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN chown shiny:shiny /var/lib/shiny-server

USER shiny

WORKDIR /opt/shiny-server

CMD ["/usr/bin/shiny-server.sh"]
