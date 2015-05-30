FROM debian:latest

ENV install_dir /abstat-akp-inference
COPY . $install_dir
WORKDIR $install_dir

CMD ["/bin/bash"]

