FROM fedora:37

RUN dnf upgrade --refresh -y \
    && dnf install -y wget

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
COPY ./*.rpm ${ELEKTRA_ROOT}
RUN rm -rf ${ELEKTRA_ROOT}/elektra-tests* ${ELEKTRA_ROOT}/elektra-dbg*

RUN yum localinstall -y ${ELEKTRA_ROOT}/*

RUN wget https://rpms.libelektra.org/fedora-37/libelektra.repo -O libelektra.repo \
    && mv libelektra.repo /etc/yum.repos.d/ \
    && yum update -y

RUN yum -y install --downloadonly --downloaddir=./ elektra-tests \
    && rpm -i --nodeps ./elektra-tests* ./elektra-tests* \
    && dnf clean all -y

RUN rm -rf ${ELEKTRA_ROOT} ./elektra-tests*

RUN kdb mount-info \
    && mkdir -p `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` || true \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SYSTEM .` \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/BUILTIN_DATA_FOLDER .`

USER ${JENKINS_USERID}
