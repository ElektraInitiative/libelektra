FROM ubuntu:16.04

# elektra deps
RUN apt-get update && apt-get install -y --no-install-recommends cmake git build-essential curl

# elektra web deps
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get install -y --no-install-recommends nodejs libyajl-dev nodejs-legacy npm

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

# clean up
RUN apt-get remove -y curl libyajl-dev cmake git build-essential

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

