FROM rocker/shiny:latest
MAINTAINER <a href="mailto:e.h.juerrens@52north.org">J&uuml;rrens, Eike Hinderk</a>

WORKDIR /tmp

# install sos-importer
RUN apt-get update &&\
    apt-get --assume-yes upgrade &&\
    apt-get dist-upgrade --assume-yes &&\
    apt-get install --assume-yes --no-install-recommends openjdk-8-jdk maven git &&\
    git clone https://github.com/EHJ-52n/arctic-sea.git

RUN cd arctic-sea &&\
    git fetch --all &&\
    mvn install

RUN git clone https://github.com/EHJ-52n/sos-importer.git

ENV JAVA_HOME /usr/lib/jvm/default-java

RUN echo "JAVA_HOME: $JAVA_HOME"

RUN cd sos-importer &&\
    mvn package -e -Pcheck &&\
    mkdir -pv /usr/local/52n
RUN mv /tmp/sos-importer/feeder/target/52n-sos-importer-feeder-bin.jar /usr/local/52n/ &&\
    rm -rf /tmp/sos-importer &&\
    apt-get purge git maven openjdk-8-jdk --assume-yes &&\
    apt-get install openjdk-8-jre --assume-yes --no-install-recommends &&\
    apt-get autoremove --purge --assume-yes && apt-get clean --assume-yes &&\
    rm -rf /var/cache/apt/archives &&\
    rm -rf /root/.m2 &&\
    rm -rf /var/lib/apt/lists/* &&\
    rm -rf /opt/shiny-server/samples &&\
    rm -rfv /srv/shiny-server/*
RUN java -jar /usr/local/52n/52n-sos-importer-feeder-bin.jar

EXPOSE 3838

ADD shiny-server.sh /usr/bin/shiny-server.sh

RUN chmod +x /usr/bin/shiny-server.sh

USER shiny

WORKDIR /opt/shiny-server

CMD ["/usr/bin/shiny-server.sh"]