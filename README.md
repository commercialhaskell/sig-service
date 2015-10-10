# Build

## Build the run-time base Docker image

Take a look at the [Dockerfile](Dockerfile) and then run the following in the
project directory:

    docker build --tag commercialhaskell/sig-service-base .

You should only need to build your run-time base Docker image
occasionally (as needed for changes to the Dockerfile).

## Build the Yesod web application and create the application images

Take a peek at [stack.yaml](stack.yaml) to see the docker & image options.  Then
run the following:

    stack image container

This will produce 2 new images layered on top of
commercialhaskell/sig-service-base:

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
           commercialhaskell/sig-service \
           sig-service Development --port 8080

You will need to have an accessible Consul & Vault service. [Read
on for more on Consul & Vault]

# Kubernetes

## Deploy Dependencies

-   Deploy Consul & Vault
    
    Consul & Vault run best together on the same node. Two
    containers run best side-by-side. They fit the 2-peas-in-a-pod
    pattern of Kubernetes Pod deployments.
    
    It's important to create the service first even before we have
    any docker containers deployed.  Deploying the service first
    sets up any needed load-balancers & also registers with the
    internal DNS service.  We need our service registered with DNS
    so that the rest of the cluster can figure out how to join one
    another.
    
    Create the service with the example [yaml file](file://kube/consul-vault-svc.yaml).
    
        kubectl create -f kube/consul-vault-svc.yaml
        kubectl get svc
    
        NAME           LABELS                                    SELECTOR           IP(S)       PORT(S)
        consul-vault   app=consul-vault                          app=consul-vault   10.3.0.5    8200/TCP
                                                                                                8300/TCP
                                                                                                8301/TCP
                                                                                                8301/UDP
                                                                                                8302/TCP
                                                                                                8302/UDP
                                                                                                8400/TCP
                                                                                                8500/TCP
                                                                                                8600/TCP
                                                                                                8600/UDP
        kubernetes     component=apiserver,provider=kubernetes   <none>             10.3.0.1    443/TCP
    
    Think of the service as a DNS entry with a load-balancer
    entry-point. Notice the service does not have a version number
    in it. The given SELECTOR (above) is used to decide where TCP
    connections are routed in the cluster dynamically.  We probably
    won't have to re-deploy or upgrade our service so long as the
    ports aren't changed.
    
    Now create the replication controller with the example [yaml file](file://kube/consul-vault-rc.yaml)
    This will instantiate & supervise the required number of
    replicant pods.
    
        kubectl create -f kube/consul-vault-rc.yaml
    
    List the replication controllers in your cluster.  
    
        kubectl get rc
    
        CONTROLLER           CONTAINER(S)   IMAGE(S)                SELECTOR                     REPLICAS
        consul-vault-0-0-1   consul         dysinger/consul-vault   app=consul-vault,ver=0.0.1   3
                             vault          dysinger/consul-vault
    
    You'll see that the replication controller has a version number
    in it's name.  This is so we can \`kubectl rolling-upgrade\` from
    one version to the next. As we are upgrading our service's
    selector will match all the pods in the cluster (old version &
    new).
    
    List the pods in your cluster.
    
        kubectl get po
    
        NAME                       READY     STATUS    RESTARTS   AGE
        consul-vault-0-0-1-4e3ea   2/2       Running   0          1d
        consul-vault-0-0-1-q1l38   2/2       Running   0          1d
        consul-vault-0-0-1-x2oza   2/2       Running   0          1d
    
    
    View the log output from the pods to make sure everything is OK.
    You should see each Consul pod joining other pods & electing a
    leader.
    
        kubectl logs consul-vault-0-0-1-4e3ea consul
    
    You should see Vault say that it's serving on an IP address &
    port number.
    
        kubectl logs consul-vault-0-0-1-4e3ea vault
    
    Let's create a function that will open a proxy port to select
    consul/vault pods. This way we aren't repeating commands over
    and over.
    
        function with-proxy {
            # SEPARATE FUNCTION ARGUMENTS FROM THE REST OF THE ARGS
            POD_SEARCH=$1 ; shift
            POD_INDEX=$1 ; shift
            # RUN A PROXY TO THE $POD_INDEX-th POD THAT MATCHES $POD_SEARCH
            kubectl port-forward \
                    -p $(kubectl get po|grep $POD_SEARCH|cut -d' ' -f1|head -n $POD_INDEX) \
                    8200 8300 8301 8302 8400 8500 8600 &
            # TRACK THE PID OF THE PREVIOUS BACKGROUND KUBECTL COMMAND
            PORT_FORWARD_PID=$!
            # EXECUTE THE REST OF THE ARGS GIVEN TO THIS FUNCTION
            $@
            # KILL OFF THE BACKGROUND PORT-FORWARD KUBECTL COMMAND
            kill $PORT_FORWARD_PID
        }
    
    Let's set a few environment variables for Consul.
    
        # SET THE CONSUL/VAULT HTTP ADDR TO THE LOCALHOST PROXY PORT
        CONSUL_HTTP_ADDR=127.0.0.1:8500
        # SET THE CONSUL MASTER TOKEN (SEE ALSO THE KUBERNETES RC FILE)
        CONSUL_ACL_MASTER_TOKEN=bba227cc-6ef8-11e5-a46b-4437e6b12281

-   Configuring Consul
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

-   Configuring Vault
    -   Initialize Vault
        
            with-proxy vault 1 vault init
        
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
        
        You can't use Vault until you unseal every node.  This requires that you enter 3 of the above keys on every node. 
        
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
