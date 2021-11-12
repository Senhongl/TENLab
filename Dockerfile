FROM ubuntu:focal
ENV TZ=America/New_York
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -yq && \
    apt-get upgrade -yq && \
    apt-get install -yq --no-install-suggests --no-install-recommends \
        ocaml \
        menhir \
        llvm-10 \
        llvm-10-dev \
        m4 \
        git \
        aspcud \
        ca-certificates \
        python2.7 \
        pkg-config \
        cmake \
        opam && \
    ln -s /usr/bin/lli-10 /usr/bin/lli && \
    ln -s /usr/bin/llc-10 /usr/bin/llc

RUN opam init --auto-setup --yes --disable-sandboxing
RUN opam install --yes \
    llvm.10.0.0 \
    ocamlfind \
    ocamlbuild

WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"]
