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

    Create the service with the example yaml file.

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

    Now create the replication controller with the example yaml
    file. This will instantiate & supervise the required number of
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

        function proxy-to {
            # SEPARATE FUNCTION ARGUMENTS FROM THE REST OF THE ARGS
            POD_INDEX=$1 ; shift
            # RUN A PROXY TO THE $POD_INDEX-th POD THAT MATCHES $POD_SEARCH
            POD=$(kubectl get po -l app=consul-vault -o json \
                         | jq -r '.items|.[]|.metadata.name' \
                         | sed -n "${POD_INDEX}p")
            kubectl port-forward -p $POD 8200 8500 &
            KUBECTL_PID=$!
            # WAIT FOR AN API PORT TO BE OPEN
            while ! nc -q 1 localhost 8200 </dev/null; do sleep 1; done
            while ! nc -q 1 localhost 8500 </dev/null; do sleep 1; done
            # EXECUTE THE REST OF THE ARGS GIVEN TO THIS FUNCTION
            $@
            # KILL OFF THE BACKGROUND PORT-FORWARD KUBECTL COMMAND
            kill $KUBECTL_PID
            # WAIT FOR AN API PORT TO BE CLOSED
            while nc -q 1 localhost 8500 </dev/null; do sleep 1; done
        }

        function proxy-to-all {
            for i in $(seq 3); do
                proxy-to $i $@
            done
        }

    Set some variables for your shell.

        # SET ADDRESSES
        export CONSUL_HTTP_ADDR=http://localhost:8500
        export VAULT_ADDR=http://localhost:8200
        # SET THE CONSUL MASTER TOKEN TO THE SAME UUID AS IN THE KUBE RC FILE
        export CONSUL_ACL_MASTER_TOKEN=bba227cc-6ef8-11e5-a46b-4437e6b12281

-   Configuring Consul
    -   Push SSH Keys for GitHub

        You would, of course, need the github ssh keys setup ahead of
        time on github. Use \`ssh-keygen\` & upload to github if you
        don't have any existing keys setup.

            # SEND SIG-SERVICE GITHUB SSH KEYS TO CONSUL
            function consul-ssh-keys {
                curl -v -X PUT \
                     http://127.0.0.1:8500/v1/kv/sig_service/ssh/private\?token\=$CONSUL_ACL_MASTER_TOKEN \
                     --data-binary @id_rsa
                curl -v -X PUT \
                     http://127.0.0.1:8500/v1/kv/sig_service/ssh/public\?token\=$CONSUL_ACL_MASTER_TOKEN \
                     --data-binary @id_rsa.pub
            }

            alias consul-init='proxy-to 1 consul-ssh-keys'

-   Configuring Vault
    -   Initialize Vault

            alias vault-init='proxy-to 1 vault init'

            Key 1: cb3e7a6f0f1476b9fe5bb445a38825ef76d88c4e2d9e67d770aaf0273fb7b71301
            Key 2: 0469b49b201d82568724d32edea441647ef34d0beadbd4dab040c99c08ec262f02
            Key 3: e1917ae3e2c4fcc448ce7e6e18a9f38151f228ec3c5a5009986a1c549116e59f03
            Key 4: bb889effdf89c4c28af38cc9b82bb21ce82d193177d51d34457d89c4cb37c15104
            Key 5: 5e7050871d50ba50451921897e2600f9c72c7cd6a15499e76d575c0c52cd02e105
            Initial Root Token: f4a10d48-13b7-c347-cf25-69b855017697

            Vault initialized with 5 keys and a key threshold of 3. Please
            securely distribute the above keys. When the Vault is re-sealed,
            restarted, or stopped, you must provide at least 3 of these keys
            to unseal it again.

            Vault does not store the master key. Without at least 3 keys, your
            Vault will remain permanently sealed.

    -   Unseal all the Vault pods

        You can't use Vault until you unseal every pod.  This requires
        that you enter 3 of the above keys on every pod.

            alias vault-unseal="proxy-to-all vault unseal"

    -   Authenticate with Vault

        Authenticate with the Initial Root Token (taken from the
        output of the above \`vault init\` command).

            alias vault-auth="proxy-to 1 vault auth"

    -   Connect Vault to Consul

        Mounting & configuring Consul only works on the master Vault
        pod.  If 1 isn't the master then try 2 or 3.

            alias vault-mount-consul="proxy-to 1 vault mount consul"
            alias vault-config-consul="proxy-to 1 vault write consul/config/access \
                address=consul-vault.default.svc.cluster.local:8500 \
                token=$CONSUL_ACL_MASTER_TOKEN"

    -   Create a Policy for Consul K/V Access

            alias vault-acl="proxy-to 1 \
                vault write consul/roles/sig_service \
                    policy=\"$(base64 <(echo 'key \"sig_service\" { policy=\"read\" }'))\""

    -   Create a new Vault token for Sig-Service

        This will output a token which you can put into the sig-service
        replication controller pod environment. (See the kube/ files for
        sig-service)

            alias vault-token="proxy-to 1 vault token-create"

### TODO Create an ACL for Vault users to access Consul Sig-Service

## Deploy Sig Service

-   Deploy Sig-Service to Kubernetes

    kubectl create -f kube/sig-service-svc.yaml
    kubectl create -f kube/sig-service-rc.yaml

-   Setup TLS on the ELB for port 443
