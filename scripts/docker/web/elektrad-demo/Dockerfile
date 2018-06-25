# same as `elektrad`, but with a KDB config set up (for demo)

FROM elektra/web-base:latest

WORKDIR /home/elektra

# prepare demo environment

# mount copy of /etc/hosts to user/hosts

# mount as root user
RUN kdb mount --with-recommends hosts user/hosts hosts

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
