# Overwrites relevant parts of your current .emacs.d, assuming it's at ~/.emacs.d

import datetime
import os
import shutil

INIT_EL = os.path.expanduser("~/.emacs.d/init.el")
ELISP = os.path.expanduser("~/.emacs.d/elisp")
REPO_ELISP = os.path.abspath("./elisp")
REPO_INIT = os.path.abspath("./init.el")

def write_files():
    if os.path.isdir(ELISP):
        shutil.rmtree(ELISP)
    if os.path.isfile(INIT_EL):
        os.remove(INIT_EL)
    shutil.copytree(REPO_ELISP, ELISP)
    shutil.copy(REPO_INIT, INIT_EL)
   
if __name__ == "__main__":
    write_files()
