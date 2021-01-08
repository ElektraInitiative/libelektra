FROM ubuntu:focal

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get upgrade -y && apt-get install -y netbase

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


ENV ELEKTRA_ROOT=/opt/elektra/
RUN mkdir -p ${ELEKTRA_ROOT}
COPY ./*.deb ${ELEKTRA_ROOT}
COPY ./*.ddeb ${ELEKTRA_ROOT}

RUN apt-get update
RUN dpkg -i ${ELEKTRA_ROOT}/* || apt-get -f install -y

RUN kdb mount-info \
    && mkdir -p `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` || true \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SYSTEM .` \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/BUILTIN_DATA_FOLDER .`

USER ${JENKINS_USERID}
