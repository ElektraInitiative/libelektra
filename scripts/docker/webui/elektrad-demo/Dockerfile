# same as `elektrad`, but with a KDB config set up (for demo)

ARG BASE_IMG
FROM $BASE_IMG

WORKDIR /home/elektra

# prepare demo environment

# mount copy of /etc/hosts to user/hosts

# mount as root user
RUN kdb mount --with-recommends hosts user/hosts hosts
RUN kdb mount /etc/networks system/networks hosts
RUN kdb mount /etc/ssh/ssh_config system/ssh line
RUN kdb mount /etc/ldap/ldap.conf system/ldap line
RUN kdb mount /var/lib/dpkg/available system/dpkg/available dpkg
RUN kdb mount /var/lib/dpkg/status system/dpkg/status dpkg

# then switch to elektra user
USER elektra
RUN mkdir /home/elektra/.config
RUN cp /etc/hosts /home/elektra/.config/hosts

# create user/app structure
COPY --chown=elektra:elektra demo.kdb /home/elektra/
RUN kdb import user/app < /home/elektra/demo.kdb

# create user/realworld structure
COPY --chown=elektra:elektra realworld.ini /home/elektra/
RUN kdb import user/realworld ini < /home/elektra/realworld.ini

# run elektrad
EXPOSE 33333
CMD ["kdb","run-elektrad"]
