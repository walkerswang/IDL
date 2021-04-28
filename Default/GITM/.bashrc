# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export PYTHONPATH=/raid3/columbanus/nperlong/python:$PYTHONPATH
#export PYTHONPATH=+/raid3/python/:$PYTHONPATH
export  IDL_PATH=+/raid3/idl/ridley/:/usr/local/itt/idl71/lib/obsolete:/raid3/idl/nperlong/
export IDL_PATH=+/raid3/idl/ridley:/raid3/idl/amie:/raid3/idl/gtoth/:.:/usr/local/itt/idl/lib:/usr/local/itt/idl/lib/obsolete:/usr/local/itt/idl/lib/utilities:/Users/ridley/AMIE/src/srcIDL:/raid3/idl/nperlong/astron:/raid3/columbanus/nperlong/idl
IDL_EXTRAS=/raid3/idl/extras/
export IDL_STARTUP=/raid3/idl/ridley/startup

# User specific aliases and functions
