FROM mndrix/swipl

ENV install_dir /abstat-akp-inference
COPY . $install_dir
WORKDIR $install_dir

ENTRYPOINT ["/bin/bash"]

CMD ["-c", "swipl"]

