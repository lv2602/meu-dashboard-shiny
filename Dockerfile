FROM rocker/shiny:4.4.1

# Install system dependencies and R packages
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libudunits2-dev \
    && R -e "install.packages(c('sf', 'dplyr', 'rlang', 'tidyr', 'magrittr', 'ggplot2', 'scales', 'reactable', 'plotly', 'readxl', 'stringi', 'htmlwidgets', 'tibble'), repos='https://cran.rstudio.com/')"

# Copy app and data files to the root
COPY app.R /srv/shiny-server/
COPY indicadores_rj_certo.xlsx /srv/shiny-server/
COPY Limite_de_Bairros.shp /srv/shiny-server/
COPY Limite_de_Bairros.dbf /srv/shiny-server/
COPY Limite_de_Bairros.shx /srv/shiny-server/
# Add more COPY lines if you have additional shapefile files, e.g.:
# COPY Limite_de_Bairros.prj /srv/shiny-server/
# COPY Limite_de_Bairros.cpg /srv/shiny-server/

# Expose port
EXPOSE 3838

# Run Shiny Server
USER shiny
CMD ["/usr/bin/shiny-server"]
