# Dockerfile for ChemotaxisDashboard

FROM oraclelinux:8.8

# system dependencies
RUN yum install \
  wget \
  which \
  gcc \
  gcc-gfortran \
  gcc-c++ \
  zlib-devel \
  bzip2-devel \
  xz-devel \
  pcre-devel \
  libcurl-devel \
  make \
  java \
  perl \
  git \
  cmake \
  python3.11 \
  openssl-devel \
  fontconfig-devel \
  libxml2-devel \
  harfbuzz-devel \
  fribidi-devel \
  libtiff-devel \
  libjpeg-devel

# libsodium
RUN wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.19.tar.gz
RUN tar -zxf libsodium-1.0.19.tar.gz
WORKDIR "/libsodium-stable"
RUN ./configure
RUN make
RUN make install
WORKDIR "/"
RUN rm -f libsodium-1.0.19.tar.gz
RUN rm -rf libsodium-stable

#############
# install R #
#############

RUN wget https://cran.r-project.org/src/base/R-4/R-4.3.1.tar.gz
RUN tar -zxf R-4.3.1.tar.gz
RUN rm -f R-4.3.1.tar.gz
WORKDIR "/R-4.3.1"

RUN ./configure --with-readline=no --with-x=no --with-pcre1
RUN make
RUN make install
WORKDIR "/"
RUN rm -rf R-4.3.1


#################
# install Shiny #
#################

# see https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source

RUN R -e "install.packages(c('devtools', 'shiny'), repos='https://cran.rstudio.com', dependencies=TRUE)"
RUN # Clone the repository from GitHub
RUN git clone https://github.com/rstudio/shiny-server.git

# Get into a temporary directory in which we'll build the project
RUN mkdir shiny-server/tmp
WORKDIR "/shiny-server/tmp"

# Install our private copy of Node.js
RUN ../external/node/install-node.sh

# Add the bin directory to the path so we can reference node
ENV PATH=/shiny-server/bin:$PATH

# Use cmake to prepare the make step. Modify the "--DCMAKE_INSTALL_PREFIX"
# if you wish the install the software at a different location.
RUN cmake -DCMAKE_INSTALL_PREFIX=/usr/local ../
# Get an error here? Check the "How do I set the cmake Python version?" question below

# Compile native code and install npm dependencies
RUN make
RUN mkdir ../build
RUN (cd .. && ./bin/npm install)

# Install the software at the predefined location
RUN make install

# Install default config file
RUN mkdir -p /etc/shiny-server
RUN cp ../config/default.config /etc/shiny-server/shiny-server.conf


###############################
# install ChemotaxisDashboard #
###############################

#RUN R -e "devtools::install_github('abcsFrederick/Chemotaxis-Dashboard')"


#########################
# Start up Shiny Server #
#########################

# run app
CMD ["/usr/local/shiny-server/bin/shiny-server"]
