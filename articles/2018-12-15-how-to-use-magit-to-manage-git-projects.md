# How to use magit to manage Git projects?

We all use [Git](https://git-scm.com) to manage projects. Git is an
excellent [version control](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control)
tool but for novice, learning Git can be hard. Especially from the
command line when they are unaware of the flags & options to use for
appropriate situations. This may be discouraging sometimes and may
lead them to stuck with very limited usage of Git. Luckily most IDEs
these days provide Git extensions which makes using Git a lot easier.
One of such git extension is available in Emacs called **Magit**.

The Magit project defines itself as "A Git Porcelain inside Emacs" or
rather an interface where every action can be managed by pressing a
key. In this post I'll walk you through Magit interface and how to
manage a Git project using Magit. This post assumes that you already
have Emacs installed. Follow the steps on this
[page](https://magit.vc/manual/magit/Installing-from-Melpa.html#Installing-from-Melpa)
to install Magit.

## Magit's interface

Start by visiting a project directory in Emacs [dired mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-Enter.html#Dired-Enter).
For the sake of this post I'll visit the directory which has all my
Emacs configurations: `~/.emacs.d/` which is managed by Git.


	![Use Dired mode to navigate a Git project](visiting_a_git_project.png)

On the command-line we use the command `git status` to know the
current status of the project. Well, Magit has something similar
function know as `magit-status`. One can call this function using `M-x
magit-status`(short form for the keys 'Alt + x magit-status'). As a
result one can see pop-up something like below

	![Magit status popup window](magit_status.png)

As noticed, Magit is capable of showing much more than the command
`git status`. It shows a list of untracked files, files which are not
staged along with staged files. It also shows the stash list and few
recent commits. All this in a single window.

This is not where it stops, one can check "what changed?" using a TAB
key. For example I move the cursor over one of the unstaged file
`custom_functions.org` and pressed TAB key to display the changes as
seen below.

	![Use TAB key to view changes](show_unstaged_content.png)

This is similarly to using a command `git diff custom_functions.org`.
Staging a file is even easier. Simply move the cursor over a file and
press the **s** key. The file will be quickly moved to the staged file
list as show below

	![Use the key 's' to stage a file](staging_a_file.png)

Similarly to unstage a file use the **u** key. Using **s** & **u**
instead of `git add -u <file>` & `git reset HEAD <file>` was never
that time saving & fun.

## Commit changes

In the same Magit window pressing the key **c** will popup a commit
window which provides flags like `--all` to stage all files or
`--signoff` to add a signoff line in a commit message.

	![Magit commit popup window](magit_commit_popup.png)

I'll move the cursor to the line which enables signoff flag and press
ENTER. This will highlight the **--signoff** text which indicates that
the flag is enabled.

	![Enable signoff](magit_signoff_commit.png)

Pressing **c** again will popup the window to write the commit message

	![Magit commit message popup window](magit_commit_message.png)

Finally use `C-c C-c`(short form for keys Ctrl + cc) to commit the
changes.

	![Commit changes](magit_commit_message_2.png)

## Push changes

Once the changes are commited, the commit line will appear under
_Recent commits_ section.

	![The new commit will appear in the 'Recent commits' section](magit_commit_log.png)

Place the cursor on that commit and press **p** to push changes.

I've uploaded the demonstration on YouTube for you to get a feel of
Magit. Please see the video on this link --https://youtu.be/Vvw75Pqp7Mc
We have just scratched the surface. There
are many cool features in Magit to help you with Git branches,
rebasing etc. Magit is been around for 10 years and has much more to
offer. Please visit https://magit.vc to learn more.
