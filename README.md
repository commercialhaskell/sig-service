# Build

## Build the runtime base Docker image

Take a look at the [Dockerfile](Dockerfile) and then run the following in the
project directory:

    docker build --tag commercialhaskell/sig-service-base .

This will produce a new image:

    % docker images
    commercialhaskell/sig-service-base latest 9f8c8fbe329a 2 minutes ago 310.6 MB

You should only need to build your runtime base Docker image
occassionally (as needed for changes to the Dockerfile).

## Build the Yesod web application and create the application images

Take a peek at [stack.yaml](stack.yaml) to see the docker & image options.  Then
run the following:

    stack image container

This will produce 2 new images layered on top of
commercialhaskell/sig-service-base:

    % docker images
    commercialhaskell/sig-service-base latest 9f8c8fbe329a 2 minutes ago 310.6 MB
    commercialhaskell/sig-service      latest 36bb8a0008b4 3 minutes ago 382 MB

commercialhaskell/sig-service is your complete web application
without a default executable.

# Run

## Run the Yesod web application interactively

    docker run \
           --interactive=true \
           --publish=8080:8080 \
           --rm=true \
           --tty=true \
           commercialhaskell/sig-service-runtime \
           sig-service Development --port 8080

# Kubernetes

## Deploy Dependencies

-   Deploy Consul & Vault
    
        kubectl create -f kube/cluster-vault-svc.yaml
        kubectl create -f kube/cluster-vault-rc.yaml
    
        function with-proxy {
            # FUNCTION ARGUMENTS
            POD_SEARCH=$1 ; shift
            POD_INDEX=$1 ; shift
            # RUN A LOCALHOST PROXY TO THE FIRST CONSUL POD WE CAN FIND IN THE VPC
            kubectl port-forward \
                    -p $(kubectl get po|grep $POD_SEARCH|cut -d' ' -f1|head -n $POD_INDEX) \
                    8200 8300 8301 8302 8400 8500 8600 &
            # TRACK THE PID OF THE PREVIOUS BACKGROUND KUBECTL COMMAND
            PORT_FORWARD_PID=$!
            # RUN THE COMMANDS GIVEN TO THIS FUNCTION
            $@
            # KILL OFF THE BACKGROUND PORT-FORWARD KUBECTL COMMAND
            kill $PORT_FORWARD_PID
        }
    
        # SET THE CONSUL/VAULT HTTP ADDR TO THE LOCALHOST PROXY PORT
        CONSUL_HTTP_ADDR=127.0.0.1:8500
        # SET THE CONSUL MASTER TOKEN (SEE ALSO THE KUBERNETES RC FILE)
        CONSUL_ACL_MASTER_TOKEN=bba227cc-6ef8-11e5-a46b-4437e6b12281

-   Configure Consul
    -   Push SSH Keys for GitHub
        
        You would, of course, need the github ssh keys setup ahead of
        time on github. Use \`ssh-keygen\` & upload to github if you
        don't have any existing keys setup.
        
            # PUSH THE GITHUB SSH KEYS TO CONSUL
            with-proxy consul 1 (\
                curl -v -X PUT \
                    http://127.0.0.1:8500/v1/kv/sig-service/ssh/public\?token\=$CONSUL_ACL_MASTER_TOKEN \
                    --data-binary @id_rsa.pub; \
                curl -v -X PUT \
                    http://127.0.0.1:8500/v1/kv/sig-service/ssh/private\?token\=$CONSUL_ACL_MASTER_TOKEN \
                    --data-binary @id_rsa \
            )
    
    -   Create a new Consul key (for Vault)
        
            # CREATE A NEW CONSUL MANAGEMENT TOKEN (FOR VAULT)
            with-proxy consul 1 \
                       consul-cli acl-create \
                       --token=$CONSUL_ACL_MASTER_TOKEN \
                       --management

-   Configure Vault
    -   Initialize Vault
        
            with-proxy vault 1 vault init
        
        Results with something like 
        
            Key 1: b268aa9a3e1dad2f6a67624b05e480fb7f7cbda923419d026d509e9ecd2f25eb01
            Key 2: 7c17ebb9c958ad9bfd01ac655f895a75cf1a5e8212a11ada1a24423f2b2fe69f02
            Key 3: b06cdd9a85393cc97c8180ad45f994f63d891ff197b49b991e47bac8868970d803
            Key 4: 48fe1eba938193b9fbe94ffb54771c22cb1323a477f46c1878743f7fde378a0804
            Key 5: 84852899dfe002eb7a6963334e07d2a1398062d7f2e1ed5b7c17c78873911c4f05
            Initial Root Token: f9ab89e6-f201-be0c-18bf-5d58b4c0da51
            
            Vault initialized with 5 keys and a key threshold of 3. Please
            securely distribute the above keys. When the Vault is re-sealed,
            restarted, or stopped, you must provide at least 3 of these keys
            to unseal it again.
            
            Vault does not store the master key. Without at least 3 keys, your
            Vault will remain permanently sealed.
    
    -   Unseal all the Vault pods
        
            for i in $(seq 3); do
                with-proxy vault $i for j in $(seq 3); do vault unseal; done
            done
    
    -   Authenticate with Vault
        
            with-proxy vault 1 vault auth
    
    -   Connect Vault to Consul
        
        Mounting & configuring Consul only works on the master Vault
        node.  We don't know which one that is so we'll just try them
        all. Won't hurt anything.
        
            # CYCLE THROUGH ALL THE VAULT SERVERS & CONFIGURE CONSUL
            for i in $(seq 3); do
                with-proxy vault $i (\
                    vault mount consul && \
                    vault write consul/config/access \
                          address=consul-vault.default.svc.cluster.local:8500 \
                          token=<new-mgmnt-token-from-above> \
                )
            done
    
    -   Create a Policy for Consul access
        
            # CREATE A VAULT POLICY FOR CONSUL GITHUB SSH KEYS
            with-proxy vault 1 \
                echo 'key "sig_service" { policy="read" }' \
                | base64 \
                | vault write consul/roles/sig_service policy=-
    
    -   Create a new Vault token for Sig Service 
        
            # CREATE A VAULT TOKEN FOR SIG-SERVICE
            with-proxy vault 1 vault token-create

## Deploy Sig Service

    kubectl create -f kube/sig-service-svc.yaml
    kubectl create -f kube/sig-service-rc.yaml
