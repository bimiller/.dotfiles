#!/usr/bin/env bash

# Create symbolic links for my dotfiles and related files repository in my home directory.
#
# Files are organized into folders based on the environment (cygwin, darwin, linux, etc.)
# A common folder is used to store files that are common to all environments.
# The method for adding the dotfiles to my home directory is indicated in the
# extension added to the file or directory name.
#
# symlink         : Target will be a symbolic link to source.
#                 : Target name will (only) have the ".symlink" extension stripped.
#
# symlink_content : A folder will be created with the target name
#                 : and symbolic links of its content will be made in the folder.
#                 : Target name will have the ".symlink_content" extension stripped.
#                 : The symbolic links will not undergo any change in their name.
# NOTE: On cygwin, mklink must be run as administrator.
#
# This script was originally based upon https://github.com/darcyparker/my_dotfiles.


#-----------------------------------------------------------------------------------------
# Function: add_symlink(SOURCE,TARGET)
# - Creates TARGET as a link to SOURCE
# - Before creating the link, tests whether or not the target already exists.
#   Give and warning and skip the file if it already exists.
# - On windows, mklink is used so that links work in everywhere (windows, cygwin, mingw32)
#-----------------------------------------------------------------------------------------
add_symlink() {
  SOURCE=$1 ; TARGET=$2
  if [[ "$ENV_NAME" == "mingw32" || "$ENV_NAME" == "cygwin" ]] ; then
    if [ -e "$TARGET" ]; then
      echo "*** Warning: \"$TARGET\" already exists. No link will be created."
    else
      if [ -d "$SOURCE" ]; then
        #if a directory, then create a directory soft link
        echo "INFO: Adding symlink to dir \"$SOURCE\""
        cygstart --action=runas cmd /c mklink /d "$(cygpath -a -w "$TARGET")" "$(cygpath -a -w "$SOURCE")"
      else
        #If not a directory, then make a regular link
        echo "INFO: Adding symlink to file \"$SOURCE\""
        cygstart --action=runas cmd /c mklink "$(cygpath -a -w "$TARGET")" "$(cygpath -a -w "$SOURCE")"
      fi
    fi
  else
    #On unix systems, use the regular link command
    if [ -e "$TARGET" ]; then
      echo "*** Warning: \"$TARGET\" already exists. No link will be created."
    else
      echo "INFO: Adding symlink to file \"$SOURCE\""
      ln -s "$SOURCE" "$TARGET"
    fi
  fi
}

#-----------------------------------------------------------------------------------------
# Function: add_it(type,source)
# - Adds $SOURCE from the dot files repo to the desired target folder (usually $HOME)
# - The way it adds $SOURCE depends on the $TYPE value
#
#   "mkdir_for_symlinks" : makes a directory
#   "symlink"            : adds a symbolic link (does not transform name)
#   "symlink_content"    : adds a symbolic link (does not transform name)
#                          (however it does strip the ".symlink_content" from its parent folder)
#-----------------------------------------------------------------------------------------
add_it() {
  SOURCE=$2 ; TYPE=$1
  NAME=${SOURCE##*/} #extract filename or dirname from $SOURCE
  FROM_RELATIVE=${SOURCE#$FROM/} #strip $FROM from front of $SOURCE path
  TO_RELATIVE=${FROM_RELATIVE#*/} # strip first folder (such as common or platform)
  TO_RELATIVE=${TO_RELATIVE#$ENV_NAME/} # strip $ENV_NAME
  TO_RELATIVE=${TO_RELATIVE%$NAME} #strip $NAME from end

  case "$TYPE" in
    "mkdir_for_symlinks")
      TARGET=$TO/$TO_RELATIVE/${NAME%.symlink_content}
      echo "INFO: Creating directory \"$SOURCE\""
      mkdir -p "$TARGET"
      ;;
    "symlink")
      TARGET=$TO/$TO_RELATIVE/${NAME%.symlink}
      add_symlink "$SOURCE" "$TARGET"
      ;;
    "symlink_content")
      TARGET=$TO/${TO_RELATIVE/.symlink_content/}/$NAME
      add_symlink "$SOURCE" "$TARGET"
      ;;
    *)
      echo "*** Error: TYPE "$TYPE" is unknown. Cannot add."
      ;;
  esac
}

#-----------------------------------------------------------------------------------------
# Body of script
#-----------------------------------------------------------------------------------------

# Identify environment (eg. cygwin, darwin, mingw32, linux)
ENV_NAME=$(uname -s | tr '[:upper:]' '[:lower:]' | sed -e 's/_.*//')
echo "*** setting up for $ENV_NAME platform"

# Identify target folder to setup
if [ -d "$1" ]; then
  TO="$(cd "$1"; echo "$PWD")"
else
  TO="$HOME"
fi

# Identify folder to setup from
FROM="$(cd "$(dirname $(which "$0"))"; echo "$PWD")"

echo "*** Linking from $FROM to $TO"

# Create necessary folder structure if not present
find "$FROM/common" "$FROM/platforms/$ENV_NAME" -type d -regex ".*[.]symlink_content$" | \
  while read i; do
    add_it "mkdir_for_symlinks" "$i"
  done

# Create symbolic links to content found in *.symlink_content folders
find "$FROM/common" "$FROM/platforms/$ENV_NAME" -regex ".*[.]symlink_content/[^/]*" | \
  while read i; do
    add_it "symlink_content" "$i"
  done

# Create symbolic links to *.symlink files
find "$FROM/common" "$FROM/platforms/$ENV_NAME" -name "*.symlink" | \
  while read i; do
    add_it "symlink" "$i"
  done

echo "*** Done setup of dotfiles!"
