FROM mcr.microsoft.com/devcontainers/java:17-bullseye

ARG LEIN_VERSION="2.9.10"
ARG LEIN_PKG_CHECKSUM="dbb84d13d6df5b85bbf7f89a39daeed103133c24a4686d037fe6bd65e38e7f32"

WORKDIR /tmp

RUN set -eux; \
    apt-get update && \
    apt-get install --yes --no-install-recommends \
                    gnupg \
                    make \
                    wget && \
    rm -rf /var/lib/apt/lists/* && \
    mkdir -p /usr/local/bin && \
    wget -q "https://codeberg.org/leiningen/leiningen/raw/tag/${LEIN_VERSION}/bin/lein-pkg" && \
    echo "Comparing lein-pkg checksum ..." && \
    sha256sum lein-pkg && \
    echo "${LEIN_PKG_CHECKSUM} *lein-pkg" | sha256sum -c - && \
    mv lein-pkg "/usr/local/bin/lein" && \
    chmod 0755 "/usr/local/bin/lein" && \
    export GNUPGHOME="$(mktemp -d)" && \
    gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys 6A2D483DB59437EBB97D09B1040193357D0606ED && \
    wget -q "https://codeberg.org/leiningen/leiningen/releases/download/${LEIN_VERSION}/leiningen-${LEIN_VERSION}-standalone.jar" && \
    wget -q "https://codeberg.org/leiningen/leiningen/releases/download/${LEIN_VERSION}/leiningen-${LEIN_VERSION}-standalone.jar.asc" && \
    echo "Verifying file PGP signature..." && \
    gpg --batch --verify "leiningen-${LEIN_VERSION}-standalone.jar.asc" "leiningen-${LEIN_VERSION}-standalone.jar" && \
    gpgconf --kill all && \
    rm -rf "$GNUPGHOME" "leiningen-${LEIN_VERSION}-standalone.jar.asc" && \
    mkdir -p /usr/share/java && \
    mv "leiningen-${LEIN_VERSION}-standalone.jar" "/usr/share/java/leiningen-${LEIN_VERSION}-standalone.jar"

ENV LEIN_ROOT 1

RUN echo '(defproject dummy "" :dependencies [[org.clojure/clojure "1.11.1"]])' > project.clj && \
    lein deps && \
    rm project.clj

RUN mkdir -p /home/vscode/.local/bin && \
    chown -R vscode:vscode /home/vscode/.local/bin
COPY --chown=vscode:vscode workspace-setup.sh /home/vscode/.local/bin/workspace-setup
RUN chmod +x /home/vscode/.local/bin/workspace-setup
