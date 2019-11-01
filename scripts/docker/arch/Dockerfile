FROM archlinux/base

RUN pacman -Sy --noconfirm \
    awk \
    cmake \
    diffutils \
    gcc \
    lua \
    make \
    ninja \
    ruby-ronn \
    swig \
    tar

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.8.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz
