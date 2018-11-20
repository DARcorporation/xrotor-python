FROM buildpack-deps

ADD . /xrotor

WORKDIR /xrotor/bin

# Install required packages available from apt-get
RUN apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install -y gfortran

RUN make && make install BINDIR=/usr/bin
