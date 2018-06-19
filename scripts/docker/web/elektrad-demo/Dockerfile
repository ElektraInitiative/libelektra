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
RUN kdb set user/app

RUN kdb set user/app/peers
RUN kdb set user/app/peers/#0 "192.168.0.43"
RUN kdb setmeta user/app/peers/#0 check/validation "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
RUN kdb setmeta user/app/peers/#0 check/validation/message "invalid ip address"
RUN kdb setmeta user/app/peers/#1 check/validation "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
RUN kdb setmeta user/app/peers/#1 check/validation/message "invalid ip address"
RUN kdb set user/app/peers/#1 "192.168.0.44"
RUN kdb setmeta user/app/peers array 2
RUN kdb setmeta user/app/peers visibility "advanced"

RUN kdb set user/app/database "production"
RUN kdb setmeta user/app/database check/type "enum"
RUN kdb setmeta user/app/database check/enum/#0 "production"
RUN kdb setmeta user/app/database check/enum/#1 "staging"
RUN kdb setmeta user/app/database check/enum/#2 "development"

RUN kdb set user/app/host "192.168.0.42"
RUN kdb setmeta user/app/host check/validation "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
RUN kdb setmeta user/app/host check/validation/message "invalid ip address"
RUN kdb setmeta user/app/host visibility "advanced"

RUN kdb set user/app/maxConnections "1"
RUN kdb setmeta user/app/maxConnections check/type "unsigned_long"
RUN kdb setmeta user/app/maxConnections check/range "1-999"

RUN kdb set user/app/running "1"
RUN kdb setmeta user/app/running check/type "boolean"
RUN kdb setmeta user/app/running restrict/write "1"

# run elektrad
EXPOSE 33333
CMD ["kdb","run-elektrad"]

