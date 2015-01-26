#!/usr/bin/env python

from collections import namedtuple
from collections import OrderedDict


field_to_type = OrderedDict([('Artist', str), ('Title', str), ('Rating', int), ('Ripped', bool)])
cd = namedtuple('CD', field_to_type.iterkeys())
db = []

def field_to_formatted_string(field, value):
	return '"%s"' % value if field_to_type[field] == str else '%s' % value


def dumpdb():
	for cd in db:
		print cd


def prompt_for_cd():
	fields_and_values = [(field, raw_input('%s: ' % field)) for field in cd._fields]
	return eval('cd(%s)' % ', '.join([field_to_formatted_string(field, value) for field, value in fields_and_values]))


def add_cds():
	while True:
		db.append(prompt_for_cd())
		if not raw_input('Another? [y/n]: ').lower() == 'y':
			break


def select(*args):
	def column_matches(row, col, val):
		return eval('row.%s == %s' % (col, field_to_formatted_string(col, val)))

	print zip(args, args[1:])
	return [row for row in db if all([column_matches(row, col, val) for col, val in args])]


db.append(cd('Phish', 'Hoist', 6, True))
db.append(cd('John Coltrane', 'A Love Supreme', 10, True))
# add_cds()
dumpdb()
print select(('Artist', 'John Coltrane'), ('Rating', 10))

