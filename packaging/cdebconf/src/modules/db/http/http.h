#ifndef _HTTP_H_
#define _HTTP_H_

struct template;
struct question;

struct template_db_cache {
	struct template *templates;
};

struct question_db_cache {
	struct question *questions;
};

#endif
