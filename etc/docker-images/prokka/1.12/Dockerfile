FROM pveber/bistro-base:jessie


RUN apt-get update && apt-get install -y libdatetime-perl libxml-simple-perl \
    libdigest-md5-perl default-jdk bioperl

RUN wget https://github.com/tseemann/prokka/archive/v1.12.tar.gz

RUN tar xzf v1.12.tar.gz && \
    rm v1.12.tar.gz && \
    cd /usr/bin && \
    ln -s /prokka-1.12/bin/prokka

RUN prokka --setupdb
