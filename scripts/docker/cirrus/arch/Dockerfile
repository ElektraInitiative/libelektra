FROM archlinux:base-devel

RUN pacman -Sy --noconfirm \
    cmake \
    diffutils \
    discount \
    doxygen \
    git \
    go \
    graphviz \
    ninja \
    npm \
    qt5-base \
    qt5-declarative \
    rubygems \
    texlive-most \
    wget \
    xfconf \
    yajl \
    python-pip

# Build dependency for libelektra-fuse
RUN pip3 install wheel

# see https://github.com/docker-library/golang/issues/225
ENV XDG_CACHE_HOME=/tmp/.cache

ENV CGO_CFLAGS_ALLOW="directory.*"
ENV CGO_LDFLAGS_ALLOW="directory.*"

# The root user is not allowed to use `makepkg`. We therefore install software from Arch’s User Software repository as user `makepkg`.
ARG ELEKTRA_USER=makepkg
RUN useradd -m -d /home/${ELEKTRA_USER} ${ELEKTRA_USER} \
    && chown -R ${ELEKTRA_USER} /home/${ELEKTRA_USER} \
    && printf '%s ALL=(ALL) NOPASSWD:ALL\n' ${ELEKTRA_USER} >> /etc/sudoers

USER ${ELEKTRA_USER}

# Ronn-NG
ENV PATH="$PATH:/home/${ELEKTRA_USER}/.local/share/gem/ruby/3.0.0/bin"
RUN gem install ronn-ng -v 0.10.1.pre3 && ronn --version
