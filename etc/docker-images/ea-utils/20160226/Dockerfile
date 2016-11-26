FROM pveber/bistro-base:jessie

RUN apt-get update && apt-get install -y git libgsl0-dev zlib1g-dev
RUN git clone https://github.com/ExpressionAnalysis/ea-utils.git
RUN cd ea-utils && git checkout 27a480914f52b016d25567e639c55ce8f7557daa
RUN cd ea-utils/clipper && make && make install
