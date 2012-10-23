#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <autounit/autounit.h>

#include <debian-installer/hash.h>
#include <debian-installer/string.h>

#define STRING_MAX_LENGTH 10
#define KEY_VALUE_NBR 20

void get_key( const char *name, di_rstring *key) {

  size_t size;

  size = strlen (name);

  /* i know that is bad, but i know it is not written by the lookup */
  key->string = (char *) name;
  key->size = size;
}

gint
test_hash(autounit_test_t *t) 
{
  di_hash_table *table;
  int i,nbr_of_insert=20;
  char str[KEY_VALUE_NBR][STRING_MAX_LENGTH];
  di_rstring key[KEY_VALUE_NBR];


  table = di_hash_table_new(di_rstring_hash, di_rstring_equal);

  for (i=0;i<nbr_of_insert;i++) {
    snprintf(str[i],STRING_MAX_LENGTH,"%d",i);
    get_key(str[i],&key[i]);
    di_hash_table_insert (table, &key[i], str[i]);
  }
  au_assert(t,di_hash_table_size(table) == nbr_of_insert,"hash table size is different with the number of data inserted");

  return TRUE;
}

autounit_test_group_t test_hash_tests[] = {
  {"hash", test_hash, TRUE, TRUE},
  {0, 0, FALSE, FALSE}
};

int test_hash_run() {
  autounit_suite_t *c_unit_test_suite;
  int result;

  c_unit_test_suite = 
    au_new_suite(g_string_new("test di_hash"), 0, 0);
  au_add_test_group(c_unit_test_suite, test_hash_tests);

  result = au_run_suite(c_unit_test_suite);

  au_delete_suite(c_unit_test_suite);

  return result;
};
