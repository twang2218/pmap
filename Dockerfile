ARG R_BASE_VERSION=3.6.3
FROM r-base:${R_BASE_VERSION}

# Dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libv8-dev \
    librsvg2-dev \
    graphviz \
    git \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages('devtools')" && \
    Rscript -e "install.packages(c('dplyr', 'lubridate', 'data.table', 'DiagrammeR', 'shiny'))" && \
    Rscript -e "install.packages(c('covr', 'DiagrammeRsvg', 'rsvg'))"


COPY . /opt/src
WORKDIR /opt/src

# # Build and Check
# RUN Rscript -e "devtools::check()"
