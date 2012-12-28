#ifndef _CDEBCONF_MYSQL_H_
#define _CDEBCONF_MYSQL_H_

#include <mysql/mysql.h>
struct dbdata {
	MYSQL dbh;
};

/*
 * These two structs (database and database_module) are just dummy structs
 * created to get the source compiling. [pere@hungry.com 2002-09-06]
 */
struct database {
	struct dbdata *data;
};
struct database_module {
	void *initialize;
        void *shutdown;
        void *load;
        void *save;

        void *template_set;
        void *template_get;
        void *template_remove;
        void *template_iterate;

        void *question_get;
        void *question_set;
        void *question_disown;
        void *question_iterate;
};

#endif
