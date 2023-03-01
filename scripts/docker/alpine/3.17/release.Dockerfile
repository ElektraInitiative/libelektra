# syntax = docker/dockerfile:1.2
FROM alpine:3.17.2

RUN apk update \
    && apk add --no-cache --upgrade\
        augeas \
        augeas-dev \
        bash \
        bison \
        build-base \
        cmake \
        curl \
        git \
        libgit2 \
        libgit2-dev \
        ninja \
        sudo \
        yajl \
        yajl-dev \
        yaml-cpp \
        yaml-cpp-dev

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.13.0
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

ENV ELEKTRA_ROOT=/opt/elektra
ENV ELEKTRA_RELEASE=0.9.11
RUN mkdir -p ${ELEKTRA_ROOT} \
    && cd /tmp \
    && curl -o elektra.tar.gz \
      -L https://www.libelektra.org/ftp/elektra/releases/elektra-${ELEKTRA_RELEASE}.tar.gz \
    && tar -zxvf elektra.tar.gz --strip-components=1 -C ${ELEKTRA_ROOT} \
    && rm elektra.tar.gz

ARG USERID=1000
RUN adduser -u ${USERID} -G wheel -D elektra

ARG PARALLEL=8
WORKDIR ${ELEKTRA_ROOT}
RUN --mount=type=tmpfs,target=/tmp \
    --mount=type=tmpfs,target=/etc/kdb \
    --mount=type=tmpfs,target=/root/.cache/elektra \
    --mount=type=tmpfs,target=/root/.config \
    mkdir build \
    && cd build \
    && cmake -DPLUGINS="ALL;-date" \
             -DTOOLS="ALL" \
             -DENABLE_DEBUG="OFF" \
             -DENABLE_LOGGER="OFF" \
             -DCMAKE_BUILD_TYPE="Release" \
             -DKDB_DB_SYSTEM='/home/elektra/.config/kdb/system' \
             -DKDB_DB_SPEC='/home/elektra/.config/kdb/spec' \
             -DKDB_DB_HOME='/home/elektra/.config/kdb/home' \
             .. \
    && make -j ${PARALLEL} \
    && ctest -T Test --output-on-failure -j ${PARALLEL} \
    && cmake -DBUILD_TESTING=OFF -UKDB_DB_SYSTEM -UKDB_DB_SPEC -UKDB_DB_HOME . \
    && make -j ${PARALLEL} \
    && rm -Rf ${GTEST_ROOT}


FROM alpine:3.17.2
COPY --from=0 ${ELEKTRA_ROOT} \
              ${ELEKTRA_ROOT}
ARG USERID=1000

ENV ELEKTRA_ROOT=/opt/elektra
WORKDIR ${ELEKTRA_ROOT}
RUN cd build \
    && make install \
    && ldconfig /usr/local/lib/elektra/ \
    && rm -Rf ${ELEKTRA_ROOT} \
    && rm -Rf ${GTEST_ROOT}

RUN apk del \
        augeas-dev \
        bison \
        build-base \
        cmake \
        git \
        libgit2-dev \
        yajl-dev \
        yaml-cpp-dev \
        && rm -rf /var/cache/apk/* && rm -rf /home/elektra/.config && rm -rf /home/elektra/.cache

RUN echo "%wheel ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

RUN echo "alias sudo='sudo -i' # in this image we do not need to be root" >> /etc/profile
RUN echo "export PS1='\u $ '" >> /etc/profile
RUN echo "export LD_LIBRARY_PATH=/usr/local/lib/elektra/" >> /etc/profile
RUN echo "export ALLUSERSPROFILE=''" >> /etc/profile

# Workaround for "sudo: setrlimit(RLIMIT_CORE): Operation not permitted" problem
RUN echo "Set disable_coredump false" >> /etc/sudo.conf

USER ${USERID}
WORKDIR /home/elektra

CMD ["/bin/ash","-l"]
