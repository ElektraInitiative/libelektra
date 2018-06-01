FROM ubuntu:16.04

# elektra deps
RUN apt-get update -y
RUN apt-get install -y cmake git build-essential

# elektra web deps
RUN apt-get install -y libyajl-dev curl nodejs-legacy npm
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get install -y nodejs

# pull latest libelektra
RUN mkdir /home/elektra
WORKDIR /home/elektra
RUN git clone https://github.com/ElektraInitiative/libelektra.git

# build & install libelektra
RUN mkdir /home/elektra/libelektra/build
WORKDIR /home/elektra/libelektra/build
RUN cmake .. -DTOOLS="kdb;web"
RUN make
RUN make install

# create start script
WORKDIR /home/elektra
RUN printf "#!/bin/bash\nkdb run-elektrad &\nkdb run-web" > start
RUN chmod +x start

# run elektrad and webd on container start
RUN groupadd -r elektra && useradd --no-log-init -r -g elektra elektra
RUN chown -R elektra:elektra /home/elektra
USER elektra
CMD ["/home/elektra/start"]

EXPOSE 33333
EXPOSE 33334

