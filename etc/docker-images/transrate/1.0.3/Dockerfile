FROM pveber/bistro-base:stretch

RUN apt-get update
RUN apt-get install -y ruby ruby-dev cmake zlib1g-dev g++

RUN wget https://bintray.com/artifact/download/blahah/generic/transrate-1.0.3-linux-x86_64.tar.gz && \
    tar xfz transrate-1.0.3-linux-x86_64.tar.gz && \
    cd transrate-1.0.3-linux-x86_64 && \
    chmod a+x transrate && \
    echo '#!/bin/bash\ncd /transrate-1.0.3-linux-x86_64 && ./transrate $@\n' > /usr/bin/transrate && \
    chmod a+x /usr/bin/transrate
RUN transrate --install-deps ref
RUN rm transrate-1.0.3-linux-x86_64/bin/librt.so.1
