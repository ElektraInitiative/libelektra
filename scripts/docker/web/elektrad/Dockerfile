# image that only starts elektrad

FROM elektra/web-base:latest

WORKDIR /home/elektra
USER elektra

# run elektrad
EXPOSE 33333
CMD ["kdb","run-elektrad"]
