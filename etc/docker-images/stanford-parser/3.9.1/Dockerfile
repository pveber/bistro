FROM pveber/bistro-base:stretch

RUN wget https://nlp.stanford.edu/software/stanford-parser-full-2018-02-27.zip
RUN unzip stanford-parser-full-2018-02-27.zip
RUN cp -r stanford-parser-full-2018-02-27/* /usr/bin
RUN cd /usr/bin && wget "http://chaoticity.com/software/DependenSee.2.0.5.jar"
RUN sed -i 's/penn,//g' /usr/bin/lexparser.sh
RUN apt-get update && apt-get -y install openjdk-8-jre
