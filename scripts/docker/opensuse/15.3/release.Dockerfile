FROM opensuse/leap:15.3

RUN zypper update -y \
    && zypper install -y \
        sudo \
        system-group-wheel \
        system-user-mail \
        vim \
        diffutils \
    && zypper clean --all

ARG USERID=1000
RUN useradd elektra --uid ${USERID}
RUN usermod -aG wheel elektra

ENV ELEKTRA_ROOT=/opt/elektra/
RUN mkdir -p ${ELEKTRA_ROOT}
COPY ./*.rpm ${ELEKTRA_ROOT}

RUN zypper install -y --allow-unsigned-rpm ${ELEKTRA_ROOT}/* \
    && zypper clean --all

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
    export $(dbus-launch); \
    systemd-machine-id-setup || true # xfconf also requires a machine-id; \
    kdb run_all

RUN echo "%wheel ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

USER ${USERID}
WORKDIR /home/elektra

CMD ["/bin/bash","-l"]
