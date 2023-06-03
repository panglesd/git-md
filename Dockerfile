FROM ocaml/opam:ubuntu-ocaml-4.14


RUN sudo apt update
RUN sudo apt install -y libgmp-dev openssh-server pkg-config libssl-dev
RUN sudo service ssh start
RUN sudo rm /usr/bin/git-upload-pack
RUN sudo rm /usr/bin/git-receive-pack
USER root
RUN echo "#!/bin/sh\n/home/opam/git_md/_build/install/default/bin/git_md upload-pack \$@\n" > /usr/bin/git-upload-pack
RUN echo "#!/bin/sh\n/home/opam/git_md/_build/install/default/bin/git_md receive-pack \$@\n" > /usr/bin/git-receive-pack
RUN chmod a+x /usr/bin/git-upload-pack
RUN chmod a+x /usr/bin/git-receive-pack
USER opam


COPY --chown=opam *.opam git_md/

WORKDIR git_md/

RUN opam install -y --deps-only --with-test --with-doc .

RUN opam pin hockmd.dev git+https://github.com/panglesd/hockmd#9a7c921a0a922bba80c874c42fee46a52b70eedd

COPY --chown=opam dune* .
COPY --chown=opam src src

RUN opam exec -- dune build @all
# RUN opam exec -- sudo dune install --prefix=/usr/local

USER root
RUN chmod a+rwx /home/opam
RUN chmod -R a+rwx /home/opam/git_md
# RUN mkdir /home/git
# RUN chmod a+rwx /home/git
# RUN cp /home/opam/git_md/_build/default/src/bin/git_shell.exe /usr/bin

RUN printf "opam:a" | sudo chpasswd

RUN echo "PermitEmptyPasswords yes" >> /etc/ssh/sshd_config

RUN set -eux; \
    addgroup git; \
    adduser \
        --gecos "Git User" \
        --ingroup git \
        --home /home/git/ \
        --disabled-password \
        git ; \
    passwd -d git

USER git
WORKDIR /home/git

RUN export HOME=/home/git ; git config --global user.email "you@example.com"
RUN export HOME=/home/git ; git config --global user.name "Your Name"

USER root

RUN service ssh start

RUN ssh-keygen -A

EXPOSE 22

CMD [ "/usr/sbin/sshd", "-D"]
