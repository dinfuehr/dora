# Create a Development Container

Run these commands from the Dora checkout that should be mounted into the
container.

## Build the image

```sh
podman build -t dora-devbox:latest -f tools/container/Containerfile .
```

When building an arm64 image on another architecture, use binfmt/qemu support
and pass the target platform explicitly:

```sh
podman build --platform linux/arm64 -t dora-devbox:latest -f tools/container/Containerfile .
```

## Create the container

This creates a persistent container named `dora-devbox`, mounts the current working
directory at `/workspace`, and keeps the container alive for later shells.

```sh
podman create \
  --pull=never \
  --name dora-devbox \
  -it \
  -v "$(pwd):/workspace:Z" \
  -w /workspace \
  dora-devbox:latest \
  sleep infinity
```

If `dora-devbox` already exists and should be recreated, remove it first:

```sh
podman rm -f dora-devbox
```

## Use the container

```sh
podman start dora-devbox
podman exec -it dora-devbox /bin/bash
```

On hosts without SELinux labeling, remove the `:Z` suffix from the volume mount.
