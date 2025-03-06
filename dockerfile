# dockerfile

FROM rocker/r-ver:4.2.1

# Update apt-get and install system libraries
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libxml2-dev \
    build-essential \
    libv8-dev \
    libgsl-dev \
    libsodium-dev \
    cmake \
    libbz2-dev \
    liblzma-dev \
    bzip2 \
    wget \
    vim \
    perl \
    unzip \
    && rm -rf /var/lib/apt/lists/*
    

# Step 2: Install Shiny Server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" \
    && VERSION=$(cat version.txt)  \
    && wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb \
    && gdebi -n ss-latest.deb \
    && rm -f version.txt ss-latest.deb \
    && . /etc/environment \
    && R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org')" \
    && chown shiny:shiny /var/lib/shiny-server

# Step 4: Setup renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

    
COPY renv.lock renv.lock
RUN R -e "renv::restore()"

# Step 5: Copy only the needed Shiny app code
COPY css /srv/shiny-server/css
COPY doc /srv/shiny-server/doc
COPY sbs /srv/shiny-server/sbs
COPY www /srv/shiny-server/www
COPY utils /srv/shiny-server/utils
COPY server /srv/shiny-server/server
COPY ui /srv/shiny-server/ui
COPY ui.R /srv/shiny-server/ui.R
COPY server.R /srv/shiny-server/server.R
COPY global.R /srv/shiny-server/global.R


RUN chown -R shiny:shiny /srv/shiny-server

# Step 6: Expose port & specify the run command
EXPOSE 3232
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 3232)"]
