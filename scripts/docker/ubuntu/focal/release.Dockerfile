FROM ubuntu:focal

ARG DEBIAN_FRONTEND=noninteractive

ARG USERID=1000
RUN adduser elektra --uid ${USERID} \
    && adduser elektra sudo

ENV ELEKTRA_ROOT=/opt/elektra/
RUN mkdir -p ${ELEKTRA_ROOT}
COPY ./*.deb ${ELEKTRA_ROOT}
COPY ./*.ddeb ${ELEKTRA_ROOT}

RUN apt-get update \
    && apt-get -y install \
        sudo \
        vim \
    && apt-get -y install ${ELEKTRA_ROOT}/* \
    && rm -rf /var/lib/apt/lists/*

RUN rm -rf ${ELEKTRA_ROOT}

RUN kdb mount-info \
    && mkdir -p `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` || true \
    && chown -R ${USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` \
    && chown -R ${USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SYSTEM .` \
    && chown -R ${USERID} `kdb sget system:/info/elektra/constants/cmake/BUILTIN_DATA_FOLDER .`

RUN kdb run_all

RUN echo "%sudo ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

USER ${USERID}
WORKDIR /home/elektra

CMD ["/bin/bash","-l"]
