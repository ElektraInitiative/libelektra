# syntax = docker/dockerfile:1.2
FROM fedora:37

RUN dnf upgrade --refresh -y \
    && dnf install -y \
        sudo \
        vim \
        diffutils \
    && dnf clean all -y

ARG USERID=1000
RUN adduser elektra --uid ${USERID}
RUN usermod -aG wheel elektra

ENV ELEKTRA_ROOT=/opt/elektra/
RUN mkdir -p ${ELEKTRA_ROOT}
COPY ./*.rpm ${ELEKTRA_ROOT}

RUN yum localinstall -y ${ELEKTRA_ROOT}/* \
    && dnf clean all -y

RUN rm -rf ${ELEKTRA_ROOT}

RUN kdb mount-info \
    && mkdir -p `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` || true \
    && chown -R ${USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` \
    && chown -R ${USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SYSTEM .` \
    && chown -R ${USERID} `kdb sget system:/info/elektra/constants/cmake/BUILTIN_DATA_FOLDER .`

RUN --mount=type=tmpfs,target=/tmp \
    --mount=type=tmpfs,target=/etc/kdb \
    --mount=type=tmpfs,target=/root/.cache/elektra \
    --mount=type=tmpfs,target=/root/.config \
    kdb run_all

RUN echo "%wheel ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

USER ${USERID}
WORKDIR /home/elektra

CMD ["/bin/bash","-l"]
