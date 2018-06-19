FROM elektra/web-base:latest

WORKDIR /home/elektra
USER elektra

# create start script
RUN printf "#!/bin/bash\nkdb run-elektrad &\nkdb run-web" > start
RUN chmod +x start

# run elektrad and webd in one container
CMD ["/home/elektra/start"]

