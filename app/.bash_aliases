
# Dotfiles path
# ------------------------------------------------------
_bash_source="${BASH_SOURCE[0]}"
while [ -h "$_bash_source" ]; do # resolve $_bash_source until the file is no longer a symlink
	SILVUSDOTFILES="$( cd -P "$( dirname "$_bash_source" )"/../ && pwd )"
	_bash_source="$(readlink "$_bash_source")"
	[[ $_bash_source != /* ]] && _bash_source="$DIR/$SOURCE" # if $_bash_source was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done

export SILVUSDOTFILES="$( cd -P "$( dirname "$_bash_source" )"/../ && pwd )"

# Source files from bash and aliases folder
# ------------------------------------------------------
for file in "$SILVUSDOTFILES/shell/aliases/"*; do
	if [[ -f "$file" ]]; then
		source "$file"
	fi
done

for file in "$SILVUSDOTFILES/shell/bash/"*; do
	if [[ -f "$file" ]]; then
		source "$file"
	fi
done

# Environment specific configuration
# ------------------------------------------------------
if [[ -f "$SILVUSDOTFILES/shell/shell_env" ]]; then
	source "$SILVUSDOTFILES/shell/shell_env"
fi
