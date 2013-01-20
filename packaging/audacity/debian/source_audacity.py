'''apport package hook for audacity

Author:
David Henningsson <diwic@ubuntu.com>

'''

from apport.hookutils import *
import os.path
import re

def add_info(report):
	# we probably don't need all alsa_base info, but some hint
	# of the user's soundcards would be nice.
	report['AlsaCards'] = command_output(['cat', '/proc/asound/cards'])

	cfgfilename = os.path.expanduser('~/.audacity-data/audacity.cfg')
	if os.path.exists(cfgfilename):
		s = open(cfgfilename).read()
		# give the user some privacy by removing MRU file list
		s = re.sub(r'\n(file[0-9]+=)[^\n]*', r'\n\1(removed for privacy)', s)
		report['AudacityCfg'] = s
