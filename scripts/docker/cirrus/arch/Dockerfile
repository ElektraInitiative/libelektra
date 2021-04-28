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
    ruby-ronn-ng \
    texlive-most \
    wget \
    yajl

# The root user is not allowed to use `makepkg`. We therefore install software from Arch’s User Software repository as user `makepkg`.
ARG ELEKTRA_USER=makepkg
RUN useradd -m -d /home/${ELEKTRA_USER} ${ELEKTRA_USER} \
    && chown -R ${ELEKTRA_USER} /home/${ELEKTRA_USER} \
    && printf '%s ALL=(ALL) NOPASSWD:ALL\n' ${ELEKTRA_USER} >> /etc/sudoers

USER ${ELEKTRA_USER}

