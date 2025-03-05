FROM rocker/shiny:4.4

COPY . /tmp/package/

RUN Rscript -e 'install.packages("pak")' && \
    Rscript -e 'pak::local_install("/tmp/package/")'

COPY docker/app.R /srv/shiny-server/ubigen/app.R
COPY docker/shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3464
CMD ["/usr/bin/shiny-server"]
