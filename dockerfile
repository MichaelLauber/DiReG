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

    
# Step 3: Install HOMER
RUN cd /opt \
    && wget http://homer.ucsd.edu/homer/configureHomer.pl \
    && perl configureHomer.pl -install core \
    && perl configureHomer.pl -install promoter \
    && perl configureHomer.pl -install factorbins \
    # Add extra HOMER installations if needed:
    # perl configureHomer.pl -install <something>
    && ln -s /opt/homer/bin/* /usr/local/bin/ \
    && echo "export PATH=$PATH:/opt/homer/bin" >> /etc/profile
