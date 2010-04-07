# /etc/skel/.bashrc:
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
if [[ -f ~/.dir_colors ]]; then
	eval `dircolors -b ~/.dir_colors`
else
	eval `dircolors -b /etc/DIR_COLORS`
fi

# Change the window title of X terminals 
case ${TERM} in
	xterm*|rxvt*|Eterm|aterm|kterm|gnome)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
		;;
	screen)
		PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
		;;
esac

# Bash completion
[[ -f /etc/profile.d/bash-completion.sh ]] && source /etc/profile.d/bash-completion.sh

export PATH="~/.cabal/bin:~/local/bin:/sbin:/usr/sbin:$PATH"

alias ls='ls --color'
alias ss="sudo su -"
alias cal="cal -m"
alias vip="vim -p"
alias info="pinfo"
alias df="pydf"

# SSH stuff
alias sshs='ssh ddvlad@swarm.cs.pub.ro'
alias sshe='ssh so@elf.cs.pub.ro'

export PS1='[$?] '"$PS1"

# Ripped off from Cosmin Ratiu, un SO list; added 30 Jun 2009
function gen-cscope
{
    find "${@:-.}" -iregex '.*\.\(c\|h\|cpp\|hpp\|cc\|hh\|cxx\|hxx\|py\)' | cscope -b -i-
}

# Required for vim zenburn colorscheme
export TERM=xterm-256color

# Locale stuff
export LC_COLLATE="C"

# Sane $LESS for git
export LESS="-FXRSM~g"

# Vmware
export PATH="$PATH:/opt/vmware/workstation/bin"

export CDPATH="~/school/2009-2010"
