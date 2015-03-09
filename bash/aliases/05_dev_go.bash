
# Golang
# ------------------------------------------------------
if [[ -d "/usr/local/go/bin" ]]; then
    export PATH="$PATH:/usr/local/go/bin"
    if [[ -d "/data/go/bin" ]]; then
	    export GOPATH="/data/go"
	    export PATH="$PATH:$GOPATH/bin"
    fi
fi
