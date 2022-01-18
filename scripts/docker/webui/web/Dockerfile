# image that starts elektrad & webd (for quickstart with elektra web)

FROM elektra/web-base:latest

WORKDIR /home/elektra
USER elektra

# create start script
RUN printf "#!/bin/bash\nkdb run-elektrad &\nkdb run-webd" > start
RUN chmod +x start

# run elektrad and webd in one container
EXPOSE 33333
EXPOSE 33334
CMD ["/home/elektra/start"]
