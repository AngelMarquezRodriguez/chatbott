FROM rocker/r-ver:4.3.0

# Instalar dependencias del sistema necesarias para RPostgres, XML y Curl
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Establecer el directorio de trabajo
WORKDIR /app

# Copiar todos los archivos del proyecto al contenedor
COPY . /app

# Instalar explícitamente todos los paquetes de R requeridos por el proyecto
RUN R -e "install.packages(c('plumber', 'DBI', 'RPostgres', 'rvest', 'httr', 'xml2', 'stringr', 'stringi', 'jsonlite', 'igraph', 'tidygraph', 'dplyr', 'ggraph', 'ggplot2', 'tm', 'gtrendsR'), repos='https://cloud.r-project.org')"

# Exponer el puerto del servidor Plumber
EXPOSE 8000

# Comando para iniciar la aplicación
CMD ["Rscript", "main.R"]
