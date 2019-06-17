FROM pveber/bistro-base:stretch

RUN cd /usr/local/src && \
    git clone https://github.com/califano-lab/ARACNe-AP.git aracne-ap
RUN apt-get update && apt-get install -y ant openjdk-8-jdk-headless
RUN cd /usr/local/src/aracne-ap && ant main
RUN mv /usr/local/src/aracne-ap/dist/aracne.jar /usr/share/java && rm -rf /usr/local/src/aracne-ap
