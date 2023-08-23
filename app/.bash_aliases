# Source files from bash config folder
# ------------------------------------------------------
for file in "$SILVUSDOTFILES/app/.config/bash/"*; do
	if [[ -f "$file" ]]; then
		source "$file"
	fi
done

# Environment specific configuration
# ------------------------------------------------------
if [[ -f "$SILVUSDOTFILES_CUSTOM/shell" ]]; then
	source "$SILVUSDOTFILES_CUSTOM/shell"
fi
