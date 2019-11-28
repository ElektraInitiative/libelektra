FROM alpine:3.10

RUN apk update \
    && apk add --no-cache --upgrade\
        bison \
        build-base \
        cmake \
        curl \
        diffutils \
        file \
        git \
        ninja \
        ronn \
        yaml-cpp-dev

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.8.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

ENV ELEKTRA_ROOT=/opt/elektra
ENV ELEKTRA_RELEASE=0.9.1
RUN mkdir -p ${ELEKTRA_ROOT} \
    && cd /tmp \
    && curl -o elektra.tar.gz \
      -L https://www.libelektra.org/ftp/elektra/releases/elektra-${ELEKTRA_RELEASE}.tar.gz \
    && tar -zxvf elektra.tar.gz --strip-components=1 -C ${ELEKTRA_ROOT} \
    && rm elektra.tar.gz

ARG PARALLEL=8
WORKDIR ${ELEKTRA_ROOT}
RUN mkdir build \
    && cd build \
    && cmake -DPLUGINS="ALL;-date" \
             -DTOOLS="ALL" \
             -DENABLE_DEBUG="OFF" \
             -DENABLE_LOGGER="OFF" \
             .. \
    && make -j ${PARALLEL} \
    && ctest -T Test --output-on-failure -j ${PARALLEL} -LE kdbtests


FROM alpine:3.10
COPY --from=0 ${ELEKTRA_ROOT} \
              ${ELEKTRA_ROOT}

RUN apk update \
    && apk add --no-cache --upgrade\
        bash

ENV ELEKTRA_ROOT=/opt/elektra
WORKDIR ${ELEKTRA_ROOT}
RUN cd build \
    && make install \
    && ldconfig /usr/local/lib/elektra/ \
    && rm -Rf ${ELEKTRA_ROOT}

ENV LD_LIBRARY_PATH=/usr/local/lib/elektra/

# Create User:Group
# The id is important as jenkins docker agents use the same id that is running
# on the slaves to execute containers
ARG USERID=1000
RUN adduser \
    -u ${USERID} \
    -D \
    -s /bin/bash \
    elektra
USER ${USERID}
WORKDIR /home/elektra