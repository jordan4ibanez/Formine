default:
	@fpm run

release:
	@fpm run --flag -fuse-ld=mold --flag -O3

.PHONY: test
test:
	@fpm test --flag -fuse-ld=mold
	
# Use this if the vscode extension gives up.
clean:
	@./scripts/clear_mod_files.sh
	@./scripts/remove_build_folder.sh


#? Leaving this in for when polymorphic types are implemented.
# --compiler flang-new 
