#!/usr/bin/env bash
# See description at https://github.com/fpco/devops-helpers#wrappers
set -xe
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
docker build -t commercialhaskell/sig-service-base etc/docker/sig-service-base
stack image container "$@"
