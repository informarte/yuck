FROM minizinc/mznc2020:latest

LABEL maintainer="informarte@freenet.de"
LABEL description="Yuck is a local-search constraint solver with FlatZinc interface"
LABEL homepage="https://github.com/informarte/yuck"

RUN apt-get update

# Install Java runtime
RUN apt-get install -y default-jre-headless && \
    java -version

# Install Yuck
ARG yuck_deb_url
LABEL yuck_deb_url=${yuck_deb_url}
RUN apt-get install -y wget && \
    wget -O yuck.deb ${yuck_deb_url} && \
    dpkg -i yuck.deb && \
    yuck --version && \
    apt-get remove -y --purge --autoremove wget && \
    rm -f yuck.deb

# Register Yuck as solver to test
RUN cd /entry_data && \
    ln -s /usr/bin/yuck fzn-exec && \
    rm -fr mzn-lib && \
    ln -s /usr/share/yuck/mzn/lib mzn-lib

# Set JAVA_OPTS
ARG java_opts
LABEL java_opts=${java_opts}
ENV JAVA_OPTS ${java_opts}
