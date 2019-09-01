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
if [[ -f "$SILVUSDOTFILES_CUSTOM/shell" ]]; then
	source "$SILVUSDOTFILES_CUSTOM/shell"
fi
