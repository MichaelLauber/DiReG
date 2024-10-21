FROM rocker/r-ver:4.2.0

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    wget \
    libssl-dev \
    libxml2-dev \
    build-essential\
    libv8-dev \
    vim \
    bzip2 \
    libbz2-dev \
    liblzma-dev \
    libgsl-dev \
    cmake
    
    
# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown'), repos='$MRAN')" && \
    chown shiny:shiny /var/lib/shiny-server


# setup renv to handle R packages
RUN Rscript -e 'install.packages("renv")'


COPY renv.lock renv.lock
RUN R -e "renv::restore()"

COPY css /srv/shiny-server/css
COPY data /srv/shiny-server/data
COPY doc /srv/shiny-server/doc
COPY sbs /srv/shiny-server/sbs
COPY www /srv/shiny-server/www
COPY utils /srv/shiny-server/utils
COPY server /srv/shiny-server/server
COPY ui /srv/shiny-server/ui
COPY ui.R /srv/shiny-server/ui.R
COPY server.R /srv/shiny-server/server.R 

RUN chown -R shiny:shiny /srv/shiny-server

RUN cd /srv/shiny-server

EXPOSE 3636

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3636)"]