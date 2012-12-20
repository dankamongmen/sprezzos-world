# mkbinfmt.py
import imp, sys, string, os.path

magic = string.join(["\\x%.2x" % ord(c) for c in imp.get_magic()],"")

name = sys.argv[1]
 
binfmt = '''\
package %s
interpreter /usr/bin/%s
magic %s\
''' % (name, name, magic)

#filename = '/usr/share/binfmts/' + name
#open(filename,'w+').write(binfmt)

print binfmt
