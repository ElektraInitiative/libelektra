FROM debian:bullseye
RUN apt-get update
RUN apt-get install cmake git build-essential -y
RUN git clone https://github.com/ElektraInitiative/libelektra.git
WORKDIR /libelektra
RUN mkdir build
WORKDIR /libelektra/build
RUN cmake ..
RUN make install
