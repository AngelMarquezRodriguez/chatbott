FROM rocker/r-ver:4.3.0

# Instalar librerías del sistema necesarias
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copiar archivos del proyecto
COPY . /app
WORKDIR /app

# Instalar paquetes de R
RUN Rscript -e "install.packages(c('plumber', 'DBI', 'RPostgres', 'stringr', 'stringi', 'httr', 'jsonlite', 'dplyr', 'tm'), repos='https://cloud.r-project.org')"

# Exponer el puerto
EXPOSE 8000

# Arrancar el servidor
CMD ["Rscript", "main.R"]