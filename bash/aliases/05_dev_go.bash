
# Golang
# ------------------------------------------------------
if [[ -d "/usr/local/go/bin" ]]; then
    export PATH="$PATH:/usr/local/go/bin"
    if [[ -d "$PROJECT_HOME/go/bin" ]]; then
	    export GOPATH="$PROJECT_HOME/go"
	    export PATH="$PATH:$GOPATH/bin"
    fi
fi
