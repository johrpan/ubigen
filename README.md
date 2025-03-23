# Ubigen

Ubigen is an interactive web application for analyzing ubiquitously expressed
genes. It is publicly available at
[ubigen.uni-rostock.de](https://ubigen.uni-rostock.de).
This repository contains an R package including the input data, the methods and
the code for the interactive web interface.

## Installation and Usage

### Local

You can install the development version of `ubigen` using:

```r
# install.packages("pak")
pak::pkg_install("johrpan/ubigen")
```

You only need one function to get started. It will run the application and
serve the graphical user interface which can be viewed using your preferred
web browser:

```r
ubigen::run_app()
```

For additional information on using the R package, take a look at the built-in
documentation (`?ubigen::run_app`).

### Server

The [`Dockerfile`](Dockerfile) included in this repository can be used to
deploy Ubigen on a server. It exposes the web application at port 3464. We
recommend setting up a reverse proxy to enable TLS. Prebuilt Docker images are
available on [Docker Hub](https://hub.docker.com/r/johrpan/ubigen). You can
get the latest release using:

```bash
docker pull johrpan/ubigen
```

Running the image like this will expose an _unencrypted_ HTTP server at port
80:

```bash
docker run -p 80:3464 johrpan/ubigen
```

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the
[GNU Affero General Public License](https://www.gnu.org/licenses/agpl-3.0.html)
for more details.
