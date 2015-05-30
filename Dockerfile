FROM textlab/ubuntu-essential

RUN \
	apt-get update && \
	apt-get install -y software-properties-common && \
	apt-add-repository ppa:swi-prolog/stable && \
	apt-get update && \
	apt-get install -y swi-prolog

ENV install_dir /abstat-akp-inference
COPY . $install_dir
WORKDIR $install_dir

ENTRYPOINT ["/bin/bash"]

