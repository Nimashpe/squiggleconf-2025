FROM ubuntu:24.04

LABEL maintainer="SquiggleConf 2025 <support@squiggleconf.com>"
LABEL description="Development environment for SquiggleConf 2025 projects and demos"

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install basic tools and dependencies
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    git \
    build-essential \
    pkg-config \
    libssl-dev \
    emacs \
    vim \
    zip \
    unzip \
    python3 \
    python3-pip \
    python3-venv \
    python-is-python3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js and npm
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get update \
    && apt-get install -y nodejs \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install TypeScript and other global npm packages
RUN npm install -g typescript@latest @babel/cli @babel/core vite webpack rollup eslint prettier

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Install wasm-pack
RUN curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# Install Go
RUN wget https://go.dev/dl/go1.21.0.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf go1.21.0.linux-amd64.tar.gz \
    && rm go1.21.0.linux-amd64.tar.gz
ENV PATH="/usr/local/go/bin:${PATH}"

# Install Deno
RUN curl -fsSL https://deno.land/install.sh | sh
ENV PATH="/root/.deno/bin:${PATH}"

# Install Bun
RUN curl -fsSL https://bun.sh/install | bash
ENV PATH="/root/.bun/bin:${PATH}"

# Create workspace directory
WORKDIR /app

# Copy package.json and install dependencies
COPY package*.json ./
RUN npm install

# Copy tool version check script
COPY tools/scripts/tool-version-check.js /app/tool-version-check.js
RUN chmod +x /app/tool-version-check.js

# Create sample script to verify tools
RUN echo '#!/bin/bash\n\
echo "SquiggleConf 2025 Docker Environment"\n\
echo "================================"\n\
echo "Node.js: $(node --version)"\n\
echo "npm: $(npm --version)"\n\
echo "TypeScript: $(tsc --version)"\n\
echo "Rust: $(rustc --version)"\n\
echo "Go: $(go version)"\n\
echo "Deno: $(deno --version)"\n\
echo "Bun: $(bun --version)"\n\
echo "Python: $(python --version)"\n\
echo "================================"\n\
' > /usr/local/bin/check-tools && chmod +x /usr/local/bin/check-tools

# Set entrypoint
ENTRYPOINT ["/bin/bash"]