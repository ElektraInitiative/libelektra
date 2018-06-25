# same as `webd`, but with 2 instances already created, they both connect to http://elektrad-demo.libelektra.org with different visibility levels (for demo)

FROM web-base:latest

WORKDIR /home/elektra
USER elektra

# prepare demo environment
COPY --chown=elektra:elektra demo.kdb /home/elektra/
RUN kdb import user/sw < /home/elektra/demo.kdb

# run webd (serves client)
EXPOSE 33334
CMD ["kdb","run-web"]
