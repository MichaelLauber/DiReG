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
    bedtools \
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

# Install HOMER in /opt/homer
RUN mkdir -p /opt/homer \
    && cd /opt/homer \
    && wget --no-verbose -O configureHomer.pl http://homer.ucsd.edu/homer/configureHomer.pl \
    && perl configureHomer.pl -install \
    && chmod -R a+rx /opt/homer

# Update PATH to include MEME Suite and HOMER binaries
ENV PATH="/opt/homer/bin:${PATH}"


# Install MEME Suite from source (command-line tools only)
RUN wget --no-verbose -O meme.tar.gz https://meme-suite.org/meme-software/5.5.7/meme-5.5.7.tar.gz \
    && tar zxf meme.tar.gz \
    && cd meme-5.5.7 \
    && ./configure --prefix=/opt/meme --enable-build-libxml2 --enable-build-libxslt \
    && make \
    && make install \
    && cd .. \
    && rm -rf meme-5.5.7 meme.tar.gz

ENV PATH="/opt/meme/bin:${PATH}"

RUN R -e "install.packages('reshape2', repos='https://cloud.r-project.org')"

# Step 5: Copy only the needed Shiny app code
COPY refGenome /srv/shiny-server/refGenome
COPY data /srv/shiny-server/data
COPY css /srv/shiny-server/css
COPY doc /srv/shiny-server/doc
COPY sbs /srv/shiny-server/sbs
COPY www /srv/shiny-server/www
COPY utils /srv/shiny-server/utils
COPY ui.R /srv/shiny-server/ui.R
COPY ui /srv/shiny-server/ui
COPY login /srv/shiny-server/login


COPY scripts /srv/shiny-server/scripts
COPY server /srv/shiny-server/server
COPY server.R /srv/shiny-server/server.R

RUN chown -R shiny:shiny /srv/shiny-server


# Step 6: Expose port & specify the run command
EXPOSE 3232
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 3232)"]
