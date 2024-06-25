FROM minizinc/mznc2024:latest

LABEL maintainer="informarte@freenet.de"
LABEL description="Yuck is a local-search constraint solver with FlatZinc interface"
LABEL homepage="https://github.com/informarte/yuck"

RUN apt update

# Install Java runtime
RUN apt install -y openjdk-17-jre-headless && \
    java -version

# Tell the JVM to use UTF-8
ENV LC_ALL=C.UTF-8

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
RUN echo '{"tagDefaults": [["", "yuck"]]}' > $HOME/.minizinc/Preferences.json

# Define entrypoint
RUN echo '#!/bin/bash' > /entrypoint.sh && \
    echo 'TOTAL_MEMORY=${MEMORY_LIMIT:-16384}' >> /entrypoint.sh && \
    echo 'HEAP_SIZE=$((TOTAL_MEMORY < 16384 ? TOTAL_MEMORY * 3 / 4 : TOTAL_MEMORY - 4096))' >> /entrypoint.sh && \
    echo 'export JAVA_OPTS="-server -Xmx${HEAP_SIZE}m -XX:+AggressiveHeap"' >> /entrypoint.sh && \
    echo 'exec "$@"' >> /entrypoint.sh && \
    chmod +x /entrypoint.sh
ENTRYPOINT [ "/entrypoint.sh" ]
