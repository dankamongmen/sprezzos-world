#!/usr/bin/env python
# This file was downloaded from:
#   http://code.google.com/p/rssdler/source/browse/branches/fixSession080.py
import getopt
import os
import sys

from types import StringType, IntType, LongType, DictType, ListType, TupleType
try: from types import BooleanType
except ImportError: BooleanType = None

helpMessage = """Fix the session files from rtorrent 0.7.* to 0.8.0
  rTorrent stores its session files differently now. Unfortunately, code was not
  implemented to make the transition as smooth as it could be. However, the 
  change is minor enough that this script SHOULD reliably fix the issue. 
  
  To use, SHUTDOWN rTorrent, and BACKUP your session directories.
  
  python %s <session directory> [<other> <session> <dirs>]
  
  -h prints this message
  -u will try to undo the previous run of this script. May not work. Some sanity check

(C) 2008, Distributed under the GNU General Public License v2 by lostnihilist. 
For full terms of this license, see: http://www.gnu.org/licenses/gpl-2.0.html
  
""" % sys.argv[0]

# # # # # 
# Torrent Handling from BitTorrent by Bram Cohen/Petru Paler
def bdecode(x):
        """This function decodes torrent data. 
        It comes (modified) from the GPL Python BitTorrent implementation"""
        def decode_int(x, f):
            f += 1
            newf = x.index('e', f)
            try: n = int(x[f:newf])
            except (OverflowError, ValueError):  n = long(x[f:newf])
            if x[f] == '-':
                if x[f + 1] == '0': raise ValueError
            elif x[f] == '0' and newf != f+1:  raise ValueError
            return (n, newf+1)
        def decode_string(x, f):
            colon = x.index(':', f)
            try:  n = int(x[f:colon])
            except (OverflowError, ValueError):  n = long(x[f:colon])
            if x[f] == '0' and colon != f+1:  raise ValueError
            colon += 1
            return (x[colon:colon+n], colon+n)
        def decode_list(x, f):
            r, f = [], f+1
            while x[f] != 'e':
                v, f = decode_func[x[f]](x, f)
                r.append(v)
            return (r, f + 1)
        def decode_dict(x, f):
            r, f = {}, f+1
            lastkey = None
            while x[f] != 'e':
                k, f = decode_string(x, f)
                if lastkey >= k:   raise ValueError
                lastkey = k
                r[k], f = decode_func[x[f]](x, f)
            return (r, f + 1)
        decode_func = {
          'l' : decode_list ,
          'd' : decode_dict,
          'i' : decode_int}
        for i in range(10): decode_func[str(i)] = decode_string
        if hasattr(x, 'read'): x = x.read()
        try:  r, l = decode_func[x[0]](x, 0)
        except (IndexError, KeyError):
            try: 
                x = open(x, 'r').read()
                r, l = decode_func[x[0]](x,0)
            except (OSError, IOError, IndexError, KeyError): raise ValueError
        if l != len(x):  raise ValueError
        return r

def bencode(data=None,file=None):
  "returns bencoded data, file may be name or descriptor, data encoded directly"
  class Bencached(object):
    __slots__ = ['bencoded']
    def __init__(self, s):
      self.bencoded = s

  def encode_bencached(x,r): r.append(x.bencoded)
  def encode_int(x, r): r.extend(('i', str(x), 'e'))
  def encode_string(x, r): r.extend((str(len(x)), ':', x))
  def encode_list(x, r):
    r.append('l')
    for i in x: encode_func[type(i)](i, r)
    r.append('e')
  def encode_dict(x,r):
    r.append('d')
    for k, v in sorted(list(x.items())):
      r.extend((str(len(k)), ':', k))
      encode_func[type(v)](v, r)
    r.append('e')
  encode_func = {}
  encode_func[type(Bencached(0))] = encode_bencached
  encode_func[IntType] = encode_func[LongType] = encode_int
  encode_func[StringType] = encode_string
  encode_func[ListType] = encode_func[TupleType] = encode_list
  encode_func[DictType] = encode_dict
  if BooleanType: encode_func[BooleanType] = encode_int
  if file is not None:
    if hasattr(file, 'read'): data = file.read()
    else: data = open(file,'rb').read() # string or binary?
  elif data is None: 
    raise ValueError('must provide file (name or descriptor) or data')
  x = data
  r = []
  encode_func[type(x)](x, r)
  return ''.join(r)

# # # # # 
def parseArgs(args):
  try: (argp, rest) =  getopt.gnu_getopt(args, "hu", ['help'])
  except getopt.GetoptError: raise SystemExit(helpMessage)
  if not rest: 
    print helpMessage
    raise SystemExit
  if ('-u', '') in argp: action = 'undo'
  else: action = 'do'
  return rest, action

def checkArgs(directories):
  ln = len(directories) -1
  for i, directory in enumerate(reversed(directories)):
    if not os.path.isdir(directory): 
      del directories[ln-i]
      print >> sys.stderr, '%s is not a directory, will skip' % directory
  return directories

def getTorNames(dir):
    return [os.path.join(dir, x) for x in os.listdir(dir) if x.endswith('.torrent')]
  
def main():
  directories, action =  parseArgs(sys.argv[1:])
  directories = checkArgs(directories)
  for dir in directories:
    for tor in getTorNames(dir):
      tord = bdecode(tor)
      if 'files' not in tord['info']: continue #single file torrent
      if 'rtorrent' not in tord: 
        print >> sys.stderr, "file %s appears to not be a session file" % tor
        continue
      if action == 'do':
        tord['rtorrent']['directory'] = '%s/%s/' % (tord['rtorrent']['directory'].rstrip('/'),tord['info']['name'])
      elif action =='undo':
        dn, fn = os.path.split(tord['info']['name'].rstrip('/'))
        if fn == tord['info']['name']: 
          tord['rtorrent']['directory'] = '%s%s' % (dn, '/')
      fdw = open(tor, 'w')
      fdw.write(bencode(data=tord))
      fdw.close()

if __name__ == '__main__': main()
