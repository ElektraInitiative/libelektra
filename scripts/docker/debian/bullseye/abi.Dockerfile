FROM debian:bullseye

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get -y install gnupg2 software-properties-common apt-transport-https

# Create User:Group
# The id is important as jenkins docker agents use the same id that is running
# on the slaves to execute containers
ARG JENKINS_GROUPID
RUN groupadd \
    -g ${JENKINS_GROUPID} \
    -f \
    jenkins

ARG JENKINS_USERID
RUN useradd \
    --create-home \
    --uid ${JENKINS_USERID} \
    --gid ${JENKINS_GROUPID} \
    --shell "/bin/bash" \
    jenkins

ENV ELEKTRA_ROOT=/opt/elektra
RUN mkdir -p ${ELEKTRA_ROOT}
COPY ./*.deb ${ELEKTRA_ROOT}
COPY ./*.ddeb ${ELEKTRA_ROOT}
RUN rm -rf ${ELEKTRA_ROOT}/elektra-tests* ${ELEKTRA_ROOT}/elektra-dbg*

RUN apt-get update \
    && apt-get -y install ${ELEKTRA_ROOT}/*

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F26BBE02F3C315A19BF1F791A9A25CC1CC83E839 \
    && add-apt-repository "deb https://debs.libelektra.org/bullseye bullseye main" \
    && apt-get update \
    && apt download elektra-tests \
    && dpkg --force-all -i ./elektra-tests* \
    && rm -rf /var/lib/apt/lists/*

RUN rm -rf ${ELEKTRA_ROOT} ./elektra-tests*

RUN kdb mount-info \
    && mkdir -p `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` || true \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SYSTEM .` \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/BUILTIN_DATA_FOLDER .`

USER ${JENKINS_USERID}
