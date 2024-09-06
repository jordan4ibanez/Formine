#/bin/bash
#! this is just ridiculous.

FOUND=false
VERSION_STRING=""
RAW_VERSION_IDENTIFIER=""
MAJOR=0
MINOR=0
PATCH=0


while IFS= read -r LINE; do
  if [[ $LINE  == "version = "* ]]; then
    VERSION_STRING=$LINE
    FOUND=true
    break
  fi
done < fpm.toml


if ! $FOUND ; then
  echo ERROR: Could not find version. Please not remove the version identifier in the TOML.
  exit 125
fi


#* BEGIN 0.0.0 EXTRACTION.


# remove [version = ]
RAW_VERSION_IDENTIFIER=${VERSION_STRING#"version = "}

# remove leading "
RAW_VERSION_IDENTIFIER=${RAW_VERSION_IDENTIFIER#'"'}

# remove trailing "
RAW_VERSION_IDENTIFIER=${RAW_VERSION_IDENTIFIER%'"'}


#* BEGIN MAJOR, MINOR, PATCH EXTRACTION.


# Get MAJOR region.
MAJOR=${RAW_VERSION_IDENTIFIER%%.*}

# Remove MAJOR region.
RAW_VERSION_IDENTIFIER=${RAW_VERSION_IDENTIFIER#*.}


# Get MINOR region.
MINOR=${RAW_VERSION_IDENTIFIER%%.*}

# Remove MINOR region.
RAW_VERSION_IDENTIFIER=${RAW_VERSION_IDENTIFIER#*.}

# PATCH is whatever is left.
PATCH=$RAW_VERSION_IDENTIFIER


echo [$MAJOR].[$MINOR].[$PATCH]




