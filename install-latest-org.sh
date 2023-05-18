#! /bin/sh -x -u -e
#
# Install the latest version of Org on GitHub Actions
# Reference https://orgmode.org/manual/Installation.html
#

WHERE="${GITHUB_WORKSPACE}/src/"
mkdir -p ${WHERE}
cd ${WHERE}

# pull for first time if we don't have it

if [ ! -d ${WHERE}/org-mode ]; then
    git clone https://git.savannah.gnu.org/git/emacs/org-mode.git
else

    # get updates, if any
    
    cd ${WHERE}/org-mode
    git pull
fi

# build the autoloads
cd ${WHERE}/org-mode
make autoloads

# Add to .emacs, .emacs.d/init.el, whatever...
#
# (add-to-list 'load-path (concat (getenv "GITHUB_WORKSPACE") "/src/org-mode/lisp"))
