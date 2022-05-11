FROM ubuntu:18.04

LABEL maintainer="informarte@freenet.de"
LABEL description="Yuck is a local-search constraint solver with FlatZinc interface"
LABEL homepage="https://github.com/informarte/yuck"

RUN apt-get update

# Install Java runtime
RUN apt-get install -y default-jre-headless && \
    apt-get clean && \
    java -version

# Install MiniZinc compiler
ARG minizinc_version=2.6.3
LABEL minizinc_version=${minizinc_version}
RUN apt-get install -y bison clang cmake flex git && \
    git clone https://github.com/MiniZinc/libminizinc.git && \
    cd libminizinc && \
    git checkout ${minizinc_version} && \
    mkdir build && \
    cd build && \
    cmake -DCMAKE_INSTALL_PREFIX:PATH=/usr/local .. && \
    cmake --build . --target install && \
    apt-get remove -y --purge --autoremove bison clang cmake flex git && \
    apt-get clean && \
    cd ../.. && \
    rm -fr libminizinc

# Install Yuck
ARG yuck_deb_url
LABEL yuck_deb_url=${yuck_deb_url}
RUN apt-get install -y wget && \
    wget -O yuck.deb ${yuck_deb_url} && \
    dpkg -i yuck.deb && \
    yuck --version && \
    apt-get remove -y --purge --autoremove wget && \
    apt-get clean && \
    rm -f yuck.deb
