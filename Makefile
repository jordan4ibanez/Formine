default:
#? Leaving this in for when polymorphic types are implemented.
# --compiler flang-new 
	@fpm run --flag -fuse-ld=mold

# Use this if the vscode extension gives up.
clean:
	@./scripts/clear_mod_files.sh
	@./scripts/remove_build_folder.sh