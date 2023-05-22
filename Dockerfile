FROM ocaml/opam:ubuntu-ocaml-4.14


RUN sudo apt update
RUN sudo apt install -y libgmp-dev openssh-server pkg-config libssl-dev
RUN sudo service ssh start
RUN sudo rm /usr/bin/git-upload-pack
RUN sudo rm /usr/bin/git-receive-pack
USER root
RUN echo "#!/bin/sh\n/home/opam/git_md/_build/install/default/bin/git_md upload-pack $@\n" > /usr/bin/git-upload-pack
RUN echo "#!/bin/sh\n/home/opam/git_md/_build/install/default/bin/git_md receive-pack $@\n" > /usr/bin/git-receive-pack
RUN chmod a+x /usr/bin/git-upload-pack
RUN chmod a+x /usr/bin/git-receive-pack
USER opam


COPY --chown=opam *.opam git_md/

WORKDIR git_md/

RUN opam install -y --deps-only --with-test --with-doc .

COPY --chown=opam dune* .
COPY --chown=opam src src

RUN opam exec -- dune build -p git_md
# RUN opam exec -- sudo dune install --prefix=/usr/local

USER root
