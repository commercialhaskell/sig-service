# Installing

## Build the runtime base Docker image

Take a look at the [Dockerfile](Dockerfile) and then run the following in the
project directory:

    docker build -t commercialhaskell/sig-service-base .

This will produce a new image:

    % docker images | grep sig-service
    commercialhaskell/sig-service-base                  latest              9f8c8fbe329a        52 minutes ago      310.6 MB

You should only need to build your runtime base Docker image
occassionally (as needed for changes to the Dockerfile).

## Build the Yesod web application and create the application images

Take a peek at [stack.yaml](stack.yaml) to see the docker & image options.  Then
run the following:

    stack image container

This will produce 2 new images layered on top of
commercialhaskell/sig-service-base:

    % docker images | grep sig-service
    commercialhaskell/sig-service-base                  latest              9f8c8fbe329a        52 minutes ago      310.6 MB
    commercialhaskell/sig-service-runtime               latest              36bb8a0008b4        3 minutes ago       382 MB
    commercialhaskell/sig-service-runtime-sig-service   latest              133a7eb0ffce        3 minutes ago       382 MB

commercialhaskell/sig-service-runtime is your complete web
application without a default executable.
commercialhaskell/sig-service-runtime-sig-service is an extension
that sets the entrypoint to the /usr/local/bin/sig-service
executable.

# Running

## Run the Yesod web application interactively

    docker run \
           --interactive=true \
           --publish=8080:8080 \
           --rm=true \
           --tty=true \
           commercialhaskell/sig-service-runtime-sig-service \
           Development

## Run the Yesod web application as a background process that survives restarts

    docker run \
           --detach=true \
           --name=sig-service \
           --publish=8080:8080 \
           --restart=always \
           commercialhaskell/sig-service-runtime-sig-service \
           Development
