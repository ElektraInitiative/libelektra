#!/bin/sh

if [ ! -f "$HOME/.xprofile" ]; then
	echo "#!/bin/sh" > "$HOME/.xprofile"
	echo >> "$HOME/.xprofile"
	chmod +x "$HOME/.xprofile"
fi

echo "export G_MESSAGES_DEBUG=all" >> "$HOME/.xprofile"
echo "export LD_LIBRARY_PATH=\"$(realpath ./lib)\"" >> "$HOME/.xprofile"
echo "export LD_PRELOAD=\"$(realpath ./lib/libxfconfbinding.so)\"" >> "$HOME/.xprofile"
