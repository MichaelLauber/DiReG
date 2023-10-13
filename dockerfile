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
ENV RENV_VERSION 1.0.3
RUN R -e "install.packages(c('remotes','huge'), repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/foreign/foreign_0.8-76.tar.gz')"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

