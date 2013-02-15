#!/usr/bin/python
# $Id: strix.py,v 1.4 2001/12/11 04:12:24 david Exp $

# copyright 2000, Drew Parsons <dfparsons@ucdavis.edu>
# released under General Public Licence (GPL) etc etc

# python script for transforming ispell word lists (with affix data)
# into plain wordlists (for /usr/share/dict), resolving affix data into
# derivative words

# case has not been carefully accounted for:
# affix rules are assumed Upper Case, words are assumed to be in lower case

from re import *

# read in each line from stdin until EOF reached
while 1:
	try:
		entry = raw_input()
	except EOFError:
		break

# [to do: break line into words if necessary?  if more than one word per line ]
# assume one word per line for now
	word = entry

# find '/' character in word, dividing root word from prefix and suffix rules
	base = split('/',word)  # from re

# print root word
	root = base[0]
	print root

# prefix and suffix rules are defined in ispell's english.aff in language/english
# (take care to be sure these rules are consistent with the dictionaries used)
# [ if you want to get really clever, you could read those rules from the affix files ]

	if len(base) > 1 :
			affices = base[1]

			# affix rules from ispell v 3.1.20

			if 'A' in affices:
				print 're'+root
			if 'I' in affices:
				print 'in'+root
			if 'U' in affices:
				print 'un'+root

			if 'V' in affices:
				if root[-1]=='e':
					print root[:-1]+'ive'
				else:
					print root+'ive'
			if 'N' in affices:
				 if root[-1]=='e':
					 print root[:-1]+'ion'
				 elif root[-1]=='y':
					 print root[:-1]+'ication'
				 else:
					 print root+'en'
			if 'X' in affices:
				  if root[-1]=='e':
					  print root[:-1]+'ions'
				  elif root[-1]=='y':
					  print root[:-1]+'ications'
				  else:
					  print root+'ens'
			if 'H' in affices:
				  if root[-1]=='y':
					  print root[:-1]+'ieth'
				  else:
					  print root+'th'
			if 'Y' in affices:
				  print root+'ly'
			if 'G' in affices:
				  if root[-1]=='e':
					  print root[:-1]+'ing'
				  else:
					  print root+'ing'
			if 'J' in affices:
				  if root[-1]=='e':
					  print root[:-1]+'ings'
				  else:
					  print root+'ings'
			if 'D' in affices:
				if root[-1]=='e':
					  print root+'d'
				elif root[-1]=='y':
					if match('[aeiou]',root[-2]):
						print root+'ed'
					else:
						print root[:-1]+'ied'
				else:
					print root+'ed'
			if 'T' in affices:
				if root[-1]=='e':
					  print root+'st'
				elif root[-1]=='y':
					if match('[aeiou]',root[-2]):
						print root+'est'
					else:
						print root[:-1]+'iest'
				else:
					print root+'est'
			if 'R' in affices:
				if root[-1]=='e':
					  print root+'r'
				elif root[-1]=='y':
					if match('[aeiou]',root[-2]):
						print root+'er'
					else:
						print root[:-1]+'ier'
				else:
					print root+'er'
			if 'Z' in affices:
				if root[-1]=='e':
					  print root+'rs'
				elif root[-1]=='y':
					if match('[aeiou]',root[-2]):
						print root+'ers'
					else:
						print root[:-1]+'iers'
				else:
					print root+'ers'
			if 'S' in affices:
				if root[-1]=='y':
					if match('[aeiou]',root[-2]):
						print root+'s'
					else:
						print root[:-1]+'ies'
				elif match('[sxzh]',root[-1]):
					print root+'es'
				else:
					print root+'s'
			if 'P' in affices:
				if root[-1]=='y':
					if match('[aeiou]',root[-2]):
						print root+'ness'
					else:
						print root[:-1]+'iness'
				else:
					print root+'ness'
#			if 'M' in affices:
#				print root+"'s"
