FROM opensuse/leap:15.3

RUN zypper update -y \
    && zypper install -y \
        strace \
        tar \
        gzip \
    && zypper clean --all

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

ENV ELEKTRA_ROOT=/opt/elektra/
RUN mkdir -p ${ELEKTRA_ROOT}
COPY ./*.rpm ${ELEKTRA_ROOT}

RUN zypper install -y --allow-unsigned-rpm ${ELEKTRA_ROOT}/* \
    && zypper clean --all

RUN kdb mount-info \
    && mkdir -p `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` || true \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SPEC .` \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/KDB_DB_SYSTEM .` \
    && chown -R ${JENKINS_USERID} `kdb sget system:/info/elektra/constants/cmake/BUILTIN_DATA_FOLDER .`

USER ${JENKINS_USERID}
