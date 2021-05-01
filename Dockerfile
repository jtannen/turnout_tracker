FROM rocker/geospatial

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  libxml2 \
  git

# curl

# RUN useradd --system -s /sbin/nologin someuser
# RUN usermod -aG sudo someuser
# RUN chown -R someuser /usr/local

# USER someuser

# RUN /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# RUN ls /usr/local/bin
# RUN brew install pkg-config
# RUN brew install gdal proj geos

RUN install2.r --error \
  --deps TRUE \
    colorspace \
    # dplyr \
    ggplot2 \
    ggthemes \
    gsheet \
    leaflet \
    lubridate \
    magrittr \
    Matrix \
    MASS \
    readr \
    rjson \
    # rgeos \
    # rgdal \
    rmarkdown \
    rtweet \
    # sf \
    stargazer \
    tidyr


COPY R R/
COPY images images/
COPY wordpress_config.json .

RUN mkdir election_configs
COPY election_configs/phila_202105/ election_configs/phila_202105/

CMD R -e "source('R/run_all.R')"
