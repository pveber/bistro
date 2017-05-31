FROM pveber/bistro-base:jessie

RUN wget https://cb.utdallas.edu/BCP/BCP_v1.1.tar.gz

Run tar -xzf BCP_v1.1.tar.gz && \
    cd BCP_v1.1 && ./make && \
    cd .. && rm BCP_v1.1.tar.gz && \
    ln -s /BCP_v1.1/BCP_HM /usr/bin/BCP_HM && \
    ln -s /BCP_v1.1/BCP_TF /usr/bin/BCP_TF
