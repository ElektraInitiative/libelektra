FROM archlinux/base

RUN pacman -Sy --noconfirm \
    base-devel \
    cmake \
    diffutils \
    discount \
    doxygen \
    git \
    go \
    graphviz \
    ninja \
    npm \
    openssl \
    python2 \
    qt5-base \
    qt5-declarative \
    ruby-ronn \
    texlive-most \
    wget \
    yajl

# The root user is not allowed to use `makepkg`. We therefore install software from Arch’s User Software repository as user `makepkg`.
ARG MAKEPKG_USER=makepkg
RUN useradd -m -d /home/${MAKEPKG_USER} ${MAKEPKG_USER} \
    && chown -R ${MAKEPKG_USER} /home/${MAKEPKG_USER} \
    && printf '%s ALL=(ALL) NOPASSWD:ALL\n' ${MAKEPKG_USER} >> /etc/sudoers

USER ${MAKEPKG_USER}

# CppCMS
WORKDIR /home/${MAKEPKG_USER}
RUN git clone https://aur.archlinux.org/cppcms.git

WORKDIR /home/${MAKEPKG_USER}/cppcms
RUN sudo -u ${MAKEPKG_USER} makepkg -si --noconfirm

# JWT C Library
WORKDIR /home/${MAKEPKG_USER}
RUN git clone https://aur.archlinux.org/libjwt.git

WORKDIR /home/${MAKEPKG_USER}/libjwt
RUN sudo -u ${MAKEPKG_USER} makepkg -si --noconfirm
