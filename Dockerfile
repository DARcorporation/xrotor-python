FROM buildpack-deps

# Add source to container and set working directory to bin
ADD . /xrotor
WORKDIR /xrotor/bin

# Install required packages available from apt-get
RUN apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install -y gfortran

# Build and install XROTOR
RUN make && make install BINDIR=/usr/bin

# Add user, switch to it, and set working directory to its home directory
RUN useradd -ms /bin/bash xrotor-user
USER xrotor-user
WORKDIR /home/xrotor-user
