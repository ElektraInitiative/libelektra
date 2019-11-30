FROM alpine:3.10

RUN apk update \
    && apk add --no-cache --upgrade\
        bison \
        build-base \
        cmake \
        curl \
        libgit2 \
        git \
        ninja \
        tcl \
        yaml-cpp-dev

# Google Test (TODO: update before 0.9.2 to gtest 1.10.0, but does not work with elektra 0.9.1)
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

ARG USERID=1000
RUN adduser \
    -u ${USERID} \
    -D \
    elektra

ARG PARALLEL=8
WORKDIR ${ELEKTRA_ROOT}
RUN mkdir build \
    && cd build \
    && cmake -DPLUGINS="ALL;-date" \
             -DTOOLS="ALL" \
             -DENABLE_DEBUG="OFF" \
             -DENABLE_LOGGER="OFF" \
             -DKDB_DB_SYSTEM='/home/elektra/.config/kdb/system' \
             -DKDB_DB_SPEC='/home/elektra/.config/kdb/spec' \
             -DKDB_DB_HOME='/home/elektra/.config/kdb/home' \
             .. \
    && make -j ${PARALLEL} \
    && ctest -T Test --output-on-failure -j ${PARALLEL}


FROM alpine:3.10
COPY --from=0 ${ELEKTRA_ROOT} \
              ${ELEKTRA_ROOT}

ENV ELEKTRA_ROOT=/opt/elektra
WORKDIR ${ELEKTRA_ROOT}
RUN cd build \
    && make install \
    && ldconfig /usr/local/lib/elektra/ \
    && rm -Rf ${ELEKTRA_ROOT}

RUN echo "alias sudo='' # in this image we do not need to be root" >> /etc/profile
RUN echo "export PS1='\u $ '" >> /etc/profile
ENV LD_LIBRARY_PATH=/usr/local/lib/elektra/
ENV ENV="/etc/profile"

ARG USERID=1000
USER ${USERID}
WORKDIR /home/elektra
