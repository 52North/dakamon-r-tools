#
#  build via
#
#    docker build -f docker/Dockerfile -t shiny-server:dakamon-$(date +%Y-%m) --no-cache .
#
#  from repository root
FROM rocker/shiny:3.5.1
MAINTAINER <a href="mailto:e.h.juerrens@52north.org">J&uuml;rrens, Eike Hinderk</a>

ENV JAVA_HOME /usr/lib/jvm/default-java

EXPOSE 3838

WORKDIR /tmp

# install sos-importer
RUN apt-get update &&\
    apt-get --assume-yes upgrade &&\
    apt-get install --assume-yes --no-install-recommends openjdk-8-jdk maven git &&\
    ln -sv /usr/lib/jvm/java-8-openjdk-amd64 /usr/lib/jvm/default-java
RUN git clone https://github.com/EHJ-52n/sos-importer.git &&\
    cd sos-importer &&\
    mvn package -e -Pcheck
RUN mkdir -pv /usr/local/52n &&\
    mv /tmp/sos-importer/feeder/target/52n-sos-importer-feeder-bin.jar /usr/local/52n/ &&\
    rm -rf /tmp/sos-importer &&\
    apt-get purge git maven openjdk-8-jdk --assume-yes &&\
    apt-get install openjdk-8-jre --assume-yes --no-install-recommends &&\
    apt-get autoremove --purge --assume-yes
RUN chown shiny:shiny /var/lib/shiny-server && apt-get clean --assume-yes &&\
    rm -rf /var/cache/apt/archives &&\
    rm -rf /root/.m2 &&\
    rm -rf /var/lib/apt/lists/* &&\
    rm -rf /opt/shiny-server/samples &&\
    rm -rf /srv/shiny-server/*

# install R packages and dependencies
RUN apt-get update &&\
    apt-get install --assume-yes --no-install-recommends libssl-dev libpq-dev

RUN R -e "install.packages(c('DT', 'shinyjs', 'httr', 'rjson', 'dplyr', 'RPostgreSQL', 'readr', 'devtools', 'later', 'pool'), repos='https://cran.rstudio.com/')"

ADD DaKaMon_importer/*.* /srv/shiny-server/DaKaMon_importer/

ADD DaKaMon_viewer/*.* /srv/shiny-server/DaKaMon_viewer/

ADD docker/shiny-server.conf /etc/shiny-server/shiny-server.conf

ADD docker/shiny-server.sh /usr/bin/shiny-server.sh

RUN java -jar /usr/local/52n/52n-sos-importer-feeder-bin.jar &&\
    echo "$(date): chown 1/4 (/srv/shiny-server)" &&\
    chown shiny:shiny -R /srv/shiny-server &&\
    echo "$(date): chown 2/4 (/opt/shiny-server)" &&\
    chown shiny:shiny -R /opt/shiny-server &&\
    echo "$(date): chown 3/4 (/var/lib/shiny-server)" &&\
    chown shiny:shiny -R /var/lib/shiny-server &&\
    echo "$(date): chown 4/4 (/usr/local/lib/R)" &&\
    chown shiny:shiny -R /usr/local/lib/R &&\
    echo "$(date): chown done" &&\
    chmod +x /usr/bin/shiny-server.sh &&\
    R --version | head -n1 &&\
    java -version

USER shiny

WORKDIR /opt/shiny-server


CMD ["/usr/bin/shiny-server.sh"]
