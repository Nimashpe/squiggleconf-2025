# SquiggleConf 2025 Docker Environment

This Docker setup provides a consistent development environment for working with SquiggleConf 2025 demos and projects across different platforms.

## Features

- Ubuntu 24.04 base image
- Pre-installed development tools:
  - Node.js 20.x with npm
  - TypeScript
  - Rust with wasm-pack
  - Go 1.21+
  - Deno
  - Bun
  - Python 3
  - Essential build tools
- Configured for web development tooling demos
- Volume mapping for seamless local development

## Usage

### Prerequisites

- Docker
- Docker Compose

### Starting the Environment

From the repository root:

```bash
cd tools/docker
docker-compose up -d
```

### Entering the Container

```bash
docker exec -it squiggleconf-2025-dev bash
```

### Checking Tool Versions

Inside the container:

```bash
check-tools
```

Or run the comprehensive tool checker:

```bash
npm run check-tools
```

### Working with the Environment

The entire repository is mounted at `/app` in the container, allowing you to:

1. Edit files on your host machine with your preferred editor
2. Run builds, tests, and demos inside the container
3. Access services through mapped ports

## Port Mapping

- 3000: React/frontend development server
- 5173: Vite default port
- 8000: Deno/Node server
- 9000: Additional server port

## Running Demo Projects

For the JSR demo:
```bash
cd /app/tools/demo-projects/jsr-demo
# Run demo code
```

For the version display demo:
```bash
cd /app/tools/demo-projects/version-display
# Run demo code
```

## Customization

You can modify the Dockerfile or docker-compose.yml to add additional tools or adjust configuration as needed for your specific demos.

## Troubleshooting

If you encounter any issues with the Docker environment, try:

1. Rebuilding the image: `docker-compose build --no-cache`
2. Checking for conflicting ports on your host system
3. Ensuring Docker has sufficient resources allocated