default:
	@fpm run --flag -g --c-flag -g

release:
	@fpm run --flag -fuse-ld=mold --flag -O3 --flag -march=native --flag -mtune=native 

.PHONY: test
test:
	@fpm test
	
# Use this if the vscode extension gives up.
clean:
	@./scripts/clear_mod_files.sh
	@./scripts/remove_build_folder.sh


#? Leaving this in for when polymorphic types are implemented.
# --compiler flang-new 
